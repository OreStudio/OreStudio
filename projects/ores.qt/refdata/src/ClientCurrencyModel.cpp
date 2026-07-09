/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/ClientCurrencyModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string currency_key_extractor(const refdata::domain::currency& e) {
    return e.iso_code;
}
}

ClientCurrencyModel::ClientCurrencyModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(currency_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientCurrencyModel::onCurrenciesLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientCurrencyModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientCurrencyModel::onPulsingComplete);
}

int ClientCurrencyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(currencies_.size());
}

int ClientCurrencyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCurrencyModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= currencies_.size())
        return {};

    const auto& currency = currencies_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case IsoCode:
                return QString::fromStdString(currency.iso_code);
            case CurrencyName:
                return QString::fromStdString(currency.name);
            case NumericCode:
                return QString::fromStdString(currency.numeric_code);
            case Symbol:
                return QString::fromStdString(currency.symbol);
            case FractionSymbol:
                return QString::fromStdString(currency.fraction_symbol);
            case FractionsPerUnit:
                return static_cast<qlonglong>(currency.fractions_per_unit);
            case RoundingType:
                return QString::fromStdString(currency.rounding_type);
            case RoundingPrecision:
                return static_cast<qlonglong>(currency.rounding_precision);
            case Format:
                return QString::fromStdString(currency.format);
            case MonetaryNature:
                return QString::fromStdString(currency.monetary_nature);
            case MarketTier:
                return QString::fromStdString(currency.market_tier);
            case SpotDays:
                return static_cast<qlonglong>(currency.spot_days);
            case DayBasis:
                return QString::fromStdString(currency.day_basis);
            case BasePrecedence:
                return static_cast<qlonglong>(currency.base_precedence);
            case HolidayCalendar:
                return currency.holiday_calendar ?
                           QString::fromStdString(*currency.holiday_calendar) :
                           QString{};
            case Version:
                return static_cast<qlonglong>(currency.version);
            case ModifiedBy:
                return QString::fromStdString(currency.modified_by);
            case RecordedAt:
                return relative_time_helper::format(currency.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::DecorationRole && index.column() == iconColumn()) {
        return flagDecoration(currency.image_id);
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(currency.iso_code);
    }

    return {};
}

QVariant ClientCurrencyModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case IsoCode:
            return tr("Code");
        case CurrencyName:
            return tr("Currency Name");
        case NumericCode:
            return tr("Numeric Code");
        case Symbol:
            return tr("Symbol");
        case FractionSymbol:
            return tr("Fraction");
        case FractionsPerUnit:
            return tr("Per Unit");
        case RoundingType:
            return tr("Rounding Type");
        case RoundingPrecision:
            return tr("Precision");
        case Format:
            return tr("Format");
        case MonetaryNature:
            return tr("Monetary Nature");
        case MarketTier:
            return tr("Market Tier");
        case SpotDays:
            return tr("Spot Days");
        case DayBasis:
            return tr("Day Basis");
        case BasePrecedence:
            return tr("Base Precedence");
        case HolidayCalendar:
            return tr("Holiday Calendar");
        case Version:
            return tr("Version");
        case ModifiedBy:
            return tr("Modified By");
        case RecordedAt:
            return tr("Recorded At");
        default:
            return {};
    }
}

void ClientCurrencyModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh currency model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!currencies_.empty()) {
        beginResetModel();
        currencies_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_currencies(0, page_size_);
}

void ClientCurrencyModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!currencies_.empty()) {
        beginResetModel();
        currencies_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_currencies(offset, limit);
}

void ClientCurrencyModel::fetch_currencies(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCurrencyModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making currencies request with offset=" << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .currencies = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_currencies_request request;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .currencies = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->currencies.size() << " currencies";
                const std::uint32_t count = static_cast<std::uint32_t>(result->currencies.size());
                return {.success = true,
                        .currencies = std::move(result->currencies),
                        .total_available_count = count,
                        .error_message = {},
                        .error_details = {}};
            },
            "currencies");
    });

    watcher_->setFuture(future);
}

void ClientCurrencyModel::onCurrenciesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch currencies: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.currencies.size());

    if (new_count > 0) {
        beginResetModel();
        currencies_ = std::move(result.currencies);
        endResetModel();

        const bool has_recent = recencyTracker_.update(currencies_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " currencies newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " currencies."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientCurrencyModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::currency* ClientCurrencyModel::getCurrency(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= currencies_.size())
        return nullptr;
    return &currencies_[idx];
}

std::vector<refdata::domain::currency> ClientCurrencyModel::getCurrencies() const {
    return currencies_;
}

QVariant ClientCurrencyModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCurrencyModel::onPulseStateChanged(bool /*isOn*/) {
    if (!currencies_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientCurrencyModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

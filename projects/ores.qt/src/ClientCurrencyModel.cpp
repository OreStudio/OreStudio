/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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

#include <QtConcurrent>
#include <QColor>
#include <QDateTime>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.refdata/messaging/protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string currency_key_extractor(const refdata::domain::currency& c) {
        return c.iso_code;
    }
}

ClientCurrencyModel::
ClientCurrencyModel(ClientManager* clientManager, ImageCache* imageCache,
    QObject* parent)
    : QAbstractTableModel(parent), clientManager_(clientManager),
      imageCache_(imageCache),
      watcher_(new QFutureWatcher<FutureWatcherResult>(this)),
      recencyTracker_(currency_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
        &QFutureWatcher<FutureWatcherResult>::finished,
        this, &ClientCurrencyModel::onCurrenciesLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
        this, &ClientCurrencyModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
        this, &ClientCurrencyModel::onPulsingComplete);

    // Connect to image cache to refresh decorations when images are loaded
    if (imageCache_) {
        connect(imageCache_, &ImageCache::imagesLoaded, this, [this]() {
            if (!currencies_.empty()) {
                emit dataChanged(index(0, Column::Flag),
                    index(rowCount() - 1, Column::Flag),
                    {Qt::DecorationRole});
            }
        });

        // Also refresh when individual images load (on-demand loading)
        connect(imageCache_, &ImageCache::imageLoaded, this, [this](const QString&) {
            if (!currencies_.empty()) {
                emit dataChanged(index(0, Column::Flag),
                    index(rowCount() - 1, Column::Flag),
                    {Qt::DecorationRole});
            }
        });
    }
}

int ClientCurrencyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(currencies_.size());
}

int ClientCurrencyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return Column::ColumnCount;
}

QVariant ClientCurrencyModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= currencies_.size())
        return {};

    const auto& currency = currencies_[row];

    // Handle DecorationRole for Flag column
    if (role == Qt::DecorationRole && index.column() == Column::Flag) {
        if (imageCache_ && currency.image_id) {
            const auto image_id_str = boost::uuids::to_string(*currency.image_id);
            return imageCache_->getIcon(image_id_str);
        }
        return {};
    }

    if (role == Qt::ForegroundRole) {
        return foreground_color(currency.iso_code);
    }

    if (role != Qt::DisplayRole)
        return {};

    switch (index.column()) {
    case Column::Flag: return {};  // No text for flag column
    case Column::CurrencyName: return QString::fromStdString(currency.name);
    case Column::IsoCode: return QString::fromStdString(currency.iso_code);
    case Column::Version: return currency.version;
    case Column::NumericCode: return QString::fromStdString(currency.numeric_code);
    case Column::Symbol: return QString::fromStdString(currency.symbol);
    case Column::FractionSymbol: return QString::fromStdString(currency.fraction_symbol);
    case Column::FractionsPerUnit: return currency.fractions_per_unit;
    case Column::RoundingType: return QString::fromStdString(currency.rounding_type);
    case Column::RoundingPrecision: return currency.rounding_precision;
    case Column::Format: return QString::fromStdString(currency.format);
    case Column::CurrencyType: return QString::fromStdString(currency.currency_type);
    case Column::RecordedBy: return QString::fromStdString(currency.recorded_by);
    case Column::RecordedAt: return relative_time_helper::format(currency.recorded_at);
    default: return {};
    }
}

QVariant ClientCurrencyModel::
headerData(int section, Qt::Orientation orientation, int role) const {
    if (role != Qt::DisplayRole)
        return {};

    if (orientation == Qt::Horizontal) {
        switch (section) {
        case Column::Flag: return tr("Flag");
        case Column::CurrencyName: return tr("Currency Name");
        case Column::IsoCode: return tr("ISO Code");
        case Column::Version: return tr("Version");
        case Column::NumericCode: return tr("Numeric Code");
        case Column::Symbol: return tr("Symbol");
        case Column::FractionSymbol: return tr("Frac. Symbol");
        case Column::FractionsPerUnit: return tr("Frac. per unit");
        case Column::RoundingType: return tr("Rounding type");
        case Column::RoundingPrecision: return tr("Rounding precision");
        case Column::Format: return tr("Format");
        case Column::CurrencyType: return tr("Currency Type");
        case Column::RecordedBy: return tr("Recorded By");
        case Column::RecordedAt: return tr("Recorded At");
        default: return {};
        }
    }

    return {};
}

void ClientCurrencyModel::refresh(bool /*replace*/) {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh currency model: disconnected.";
        return;
    }

    if (!currencies_.empty()) {
        beginResetModel();
        currencies_.clear();
        recencyTracker_.clear();
        synthetic_iso_codes_.clear();
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

    // Clear existing data and load the requested page
    if (!currencies_.empty()) {
        beginResetModel();
        currencies_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_currencies(offset, limit);
}

void ClientCurrencyModel::fetch_currencies(std::uint32_t offset,
                                            std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCurrencyModel> self = this;

    QFuture<FutureWatcherResult> future =
        QtConcurrent::run([self, offset, limit]() -> FutureWatcherResult {
            return exception_helper::wrap_async_fetch<FutureWatcherResult>([&]() -> FutureWatcherResult {
                BOOST_LOG_SEV(lg(), debug) << "Making a currencies request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .currencies = {}, .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                // Fetch currencies using typed request
                refdata::messaging::get_currencies_request request;
                request.offset = offset;
                request.limit = limit;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch currencies: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .currencies = {}, .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch currencies: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Received " << result->currencies.size()
                                           << " currencies, total available: "
                                           << result->total_available_count;

                return {.success = true, .currencies = std::move(result->currencies),
                        .total_available_count = result->total_available_count,
                        .error_message = {}, .error_details = {}};
            }, "currencies");
        });

     watcher_->setFuture(future);
}

void ClientCurrencyModel::onCurrenciesLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On currencies loaded event.";
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch currencies: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.currencies.size());

    if (new_count > 0) {
        beginInsertRows(QModelIndex(), 0, new_count - 1);
        currencies_ = std::move(result.currencies);
        endInsertRows();

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

const refdata::domain::currency* ClientCurrencyModel::getCurrency(int row) const {
    if (row < 0 || row >= static_cast<int>(currencies_.size()))
        return nullptr;

    return &currencies_[row];
}

std::vector<refdata::domain::currency> ClientCurrencyModel::getCurrencies() const {
    return currencies_;
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

QVariant ClientCurrencyModel::
foreground_color(const std::string& iso_code) const {
    // Synthetic currencies always show blue (no pulsing)
    if (synthetic_iso_codes_.find(iso_code) != synthetic_iso_codes_.end()) {
        return color_constants::synthetic_indicator;
    }

    // Recent currencies show yellow when pulsing
    if (recencyTracker_.is_recent(iso_code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }

    return {};
}

void ClientCurrencyModel::
add_synthetic_currencies(std::vector<refdata::domain::currency> currencies) {
    if (currencies.empty()) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Adding " << currencies.size()
                               << " synthetic currencies to model";

    const int old_size = static_cast<int>(currencies_.size());
    const int new_count = static_cast<int>(currencies.size());

    beginInsertRows(QModelIndex(), old_size, old_size + new_count - 1);
    for (auto& currency : currencies) {
        synthetic_iso_codes_.insert(currency.iso_code);
        currencies_.push_back(std::move(currency));
    }
    endInsertRows();

    BOOST_LOG_SEV(lg(), debug) << "Model now has " << currencies_.size()
                               << " currencies (" << synthetic_iso_codes_.size()
                               << " synthetic)";
}

bool ClientCurrencyModel::is_synthetic(const std::string& iso_code) const {
    return synthetic_iso_codes_.find(iso_code) != synthetic_iso_codes_.end();
}

void ClientCurrencyModel::mark_as_saved(const std::string& iso_code) {
    auto it = synthetic_iso_codes_.find(iso_code);
    if (it != synthetic_iso_codes_.end()) {
        synthetic_iso_codes_.erase(it);
        BOOST_LOG_SEV(lg(), debug) << "Marked currency as saved: " << iso_code;

        // Find the row and emit dataChanged for the color update
        auto currency_it = std::find_if(currencies_.begin(), currencies_.end(),
            [&iso_code](const auto& currency) {
                return currency.iso_code == iso_code;
            });
        if (currency_it != currencies_.end()) {
            const auto row = std::distance(currencies_.begin(), currency_it);
            emit dataChanged(index(static_cast<int>(row), 0),
                index(static_cast<int>(row), columnCount() - 1),
                {Qt::ForegroundRole});
        }
    }
}

void ClientCurrencyModel::clear_synthetic_markers() {
    if (!synthetic_iso_codes_.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Clearing " << synthetic_iso_codes_.size()
                                   << " synthetic markers";
        synthetic_iso_codes_.clear();
    }
}

void ClientCurrencyModel::onPulseStateChanged(bool /*isOn*/) {
    if (!currencies_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientCurrencyModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

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
#include "ores.qt/ClientCurrencyPairConventionModel.hpp"
#include "ores.qt/BoolYesNoLabel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string
currency_pair_convention_key_extractor(const refdata::domain::currency_pair_convention& e) {
    return e.pair_code;
}
}

ClientCurrencyPairConventionModel::ClientCurrencyPairConventionModel(ClientManager* clientManager,
                                                                     QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(currency_pair_convention_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientCurrencyPairConventionModel::onConventionsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientCurrencyPairConventionModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientCurrencyPairConventionModel::onPulsingComplete);
}

int ClientCurrencyPairConventionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(conventions_.size());
}

int ClientCurrencyPairConventionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCurrencyPairConventionModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= conventions_.size())
        return {};

    const auto& convention = conventions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case PairCode:
                return QString::fromStdString(convention.pair_code);
            case PipFactor:
                return convention.pip_factor;
            case TickSize:
                return convention.tick_size;
            case DecimalPlaces:
                return static_cast<qlonglong>(convention.decimal_places);
            case AdvanceCalendar:
                return convention.advance_calendar ?
                           QString::fromStdString(*convention.advance_calendar) :
                           QString{};
            case BusinessDayConvention:
                return convention.business_day_convention ?
                           QString::fromStdString(*convention.business_day_convention) :
                           QString{};
            case SpotRelative:
                return boolYesNoLabel(convention.spot_relative);
            case EndOfMonth:
                return boolYesNoLabel(convention.end_of_month);
            case Version:
                return static_cast<qlonglong>(convention.version);
            case ModifiedBy:
                return QString::fromStdString(convention.modified_by);
            case RecordedAt:
                return relative_time_helper::format(convention.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::DecorationRole && imageCache_) {
        if (index.column() == Column::PairCode)
            return currency_flag_icon_from_pair_code(*imageCache_, convention.pair_code);
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(convention.pair_code);
    }

    return {};
}

QVariant ClientCurrencyPairConventionModel::headerData(int section,
                                                       Qt::Orientation orientation,
                                                       int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case PairCode:
            return tr("Pair");
        case PipFactor:
            return tr("Pip Factor");
        case TickSize:
            return tr("Tick Size");
        case DecimalPlaces:
            return tr("Decimal Places");
        case AdvanceCalendar:
            return tr("Advance Calendar");
        case BusinessDayConvention:
            return tr("Business Day Convention");
        case SpotRelative:
            return tr("Spot Relative");
        case EndOfMonth:
            return tr("End Of Month");
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

void ClientCurrencyPairConventionModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh currency pair convention model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!conventions_.empty()) {
        beginResetModel();
        conventions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_conventions(0, page_size_);
}

void ClientCurrencyPairConventionModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!conventions_.empty()) {
        beginResetModel();
        conventions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_conventions(offset, limit);
}

void ClientCurrencyPairConventionModel::fetch_conventions(std::uint32_t offset,
                                                          std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCurrencyPairConventionModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making currency pair conventions request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .conventions = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_currency_pair_conventions_request request;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .conventions = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->conventions.size() << " currency pair conventions";
                const std::uint32_t count = static_cast<std::uint32_t>(result->conventions.size());
                return {.success = true,
                        .conventions = std::move(result->conventions),
                        .total_available_count = count,
                        .error_message = {},
                        .error_details = {}};
            },
            "currency pair conventions");
    });

    watcher_->setFuture(future);
}

void ClientCurrencyPairConventionModel::onConventionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch currency pair conventions: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.conventions.size());

    if (new_count > 0) {
        beginResetModel();
        conventions_ = std::move(result.conventions);
        endResetModel();

        const bool has_recent = recencyTracker_.update(conventions_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " currency pair conventions newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " currency pair conventions."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientCurrencyPairConventionModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::currency_pair_convention*
ClientCurrencyPairConventionModel::getConvention(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= conventions_.size())
        return nullptr;
    return &conventions_[idx];
}


QVariant
ClientCurrencyPairConventionModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCurrencyPairConventionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!conventions_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientCurrencyPairConventionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

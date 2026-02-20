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
#include "ores.qt/ClientTradeModel.hpp"

#include <QtConcurrent>
#include "ores.trade/messaging/trade_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string trade_key_extractor(const trade::domain::trade& e) {
        return e.external_id;
    }
}

ClientTradeModel::ClientTradeModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(trade_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientTradeModel::onTradesLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientTradeModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientTradeModel::onPulsingComplete);
}

int ClientTradeModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(trades_.size());
}

int ClientTradeModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientTradeModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= trades_.size())
        return {};

    const auto& trade = trades_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case ExternalId:
            return QString::fromStdString(trade.external_id);
        case TradeType:
            return QString::fromStdString(trade.trade_type);
        case LifecycleEvent:
            return QString::fromStdString(trade.lifecycle_event);
        case TradeDate:
            return QString::fromStdString(trade.trade_date);
        case EffectiveDate:
            return QString::fromStdString(trade.effective_date);
        case TerminationDate:
            return QString::fromStdString(trade.termination_date);
        case NettingSetId:
            return QString::fromStdString(trade.netting_set_id);
        case Version:
            return trade.version;
        case ModifiedBy:
            return QString::fromStdString(trade.modified_by);
        case RecordedAt:
            return relative_time_helper::format(trade.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(trade.external_id);
    }

    return {};
}

QVariant ClientTradeModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case ExternalId:
        return tr("External ID");
    case TradeType:
        return tr("Type");
    case LifecycleEvent:
        return tr("Event");
    case TradeDate:
        return tr("Trade Date");
    case EffectiveDate:
        return tr("Effective");
    case TerminationDate:
        return tr("Termination");
    case NettingSetId:
        return tr("Netting Set");
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

void ClientTradeModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh trade model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!trades_.empty()) {
        beginResetModel();
        trades_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_trades(0, page_size_);
}

void ClientTradeModel::load_page(std::uint32_t offset,
                                          std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!trades_.empty()) {
        beginResetModel();
        trades_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_trades(offset, limit);
}

void ClientTradeModel::fetch_trades(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientTradeModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making trades request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .trades = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                trade::messaging::get_trades_request request;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch trades: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .trades = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch trades: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->trades.size()
                                           << " trades";
                const std::uint32_t count =
                    static_cast<std::uint32_t>(result->trades.size());
                return {.success = true,
                        .trades = std::move(result->trades),
                        .total_available_count = count,
                        .error_message = {}, .error_details = {}};
            }, "trades");
        });

    watcher_->setFuture(future);
}

void ClientTradeModel::onTradesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch trades: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.trades.size());

    if (new_count > 0) {
        beginResetModel();
        trades_ = std::move(result.trades);
        endResetModel();

        const bool has_recent = recencyTracker_.update(trades_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " trades newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " trades."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientTradeModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const trade::domain::trade*
ClientTradeModel::getTrade(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= trades_.size())
        return nullptr;
    return &trades_[idx];
}

QVariant ClientTradeModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientTradeModel::onPulseStateChanged(bool /*isOn*/) {
    if (!trades_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientTradeModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

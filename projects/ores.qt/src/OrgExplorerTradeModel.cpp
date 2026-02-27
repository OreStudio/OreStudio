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
#include "ores.qt/OrgExplorerTradeModel.hpp"

#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.trading/messaging/trade_protocol.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

OrgExplorerTradeModel::OrgExplorerTradeModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &OrgExplorerTradeModel::onTradesLoaded);
}

int OrgExplorerTradeModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(trades_.size());
}

int OrgExplorerTradeModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant OrgExplorerTradeModel::data(
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
        case CounterpartyShortCode: {
            if (!trade.counterparty_id.has_value())
                return {};
            const auto key = boost::uuids::to_string(*trade.counterparty_id);
            const auto it = cpty_map_.find(key);
            if (it == cpty_map_.end())
                return {};
            return QString::fromStdString(it->second.short_code);
        }
        case CounterpartyName: {
            if (!trade.counterparty_id.has_value())
                return {};
            const auto key = boost::uuids::to_string(*trade.counterparty_id);
            const auto it = cpty_map_.find(key);
            if (it == cpty_map_.end())
                return {};
            return QString::fromStdString(it->second.full_name);
        }
        case LifecycleEvent:
            return QString::fromStdString(trade.lifecycle_event);
        case TradeDate:
            return QString::fromStdString(trade.trade_date);
        case EffectiveDate:
            return QString::fromStdString(trade.effective_date);
        case TerminationDate:
            return QString::fromStdString(trade.termination_date);
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

    return {};
}

QVariant OrgExplorerTradeModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case ExternalId:            return tr("External ID");
    case TradeType:             return tr("Type");
    case CounterpartyShortCode: return tr("Cpty Code");
    case CounterpartyName:      return tr("Counterparty");
    case LifecycleEvent:        return tr("Event");
    case TradeDate:             return tr("Trade Date");
    case EffectiveDate:         return tr("Effective");
    case TerminationDate:       return tr("Termination");
    case Version:               return tr("Version");
    case ModifiedBy:            return tr("Modified By");
    case RecordedAt:            return tr("Recorded At");
    default:                    return {};
    }
}

void OrgExplorerTradeModel::set_filter(
    std::optional<boost::uuids::uuid> book_id,
    std::optional<boost::uuids::uuid> business_unit_id) {
    filter_book_id_ = book_id;
    filter_business_unit_id_ = business_unit_id;
}

void OrgExplorerTradeModel::set_counterparty_map(
    std::unordered_map<std::string, CounterpartyInfo> cpty_map) {
    cpty_map_ = std::move(cpty_map);
    if (!trades_.empty()) {
        emit dataChanged(index(0, CounterpartyShortCode),
                         index(rowCount() - 1, CounterpartyName),
                         {Qt::DisplayRole});
    }
}

void OrgExplorerTradeModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch in progress, ignoring refresh.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!trades_.empty()) {
        beginResetModel();
        trades_.clear();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_trades(0, 100);
}

void OrgExplorerTradeModel::load_page(
    std::uint32_t offset, std::uint32_t limit) {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch in progress, ignoring load_page.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!trades_.empty()) {
        beginResetModel();
        trades_.clear();
        endResetModel();
    }

    fetch_trades(offset, limit);
}

void OrgExplorerTradeModel::fetch_trades(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<OrgExplorerTradeModel> self = this;

    const auto book_id = filter_book_id_;
    const auto business_unit_id = filter_business_unit_id_;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit, book_id, business_unit_id]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                if (!self || !self->clientManager_) {
                    return {.success = false, .trades = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                trading::messaging::get_trades_request request;
                request.offset = offset;
                request.limit = limit;
                request.book_id = book_id;
                request.business_unit_id = business_unit_id;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    return {.success = false, .trades = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch trades: " +
                                comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                return {.success = true,
                        .trades = std::move(result->trades),
                        .total_available_count = result->total_available_count,
                        .error_message = {}, .error_details = {}};
            }, "org trades");
        });

    watcher_->setFuture(future);
}

void OrgExplorerTradeModel::onTradesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch trades: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    beginResetModel();
    trades_ = std::move(result.trades);
    endResetModel();

    BOOST_LOG_SEV(lg(), info) << "Loaded " << trades_.size()
                              << " trades, total available: "
                              << total_available_count_;
    emit dataLoaded();
}

const trading::domain::trade*
OrgExplorerTradeModel::get_trade(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= trades_.size())
        return nullptr;
    return &trades_[idx];
}

}

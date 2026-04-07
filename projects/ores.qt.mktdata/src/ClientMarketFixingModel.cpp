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
#include "ores.qt/ClientMarketFixingModel.hpp"

#include <format>
#include <QtConcurrent>
#include <QPointer>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.marketdata.api/messaging/market_fixing_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientMarketFixingModel::ClientMarketFixingModel(
    ClientManager* clientManager,
    const boost::uuids::uuid& series_id,
    QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      series_id_(series_id),
      watcher_(new QFutureWatcher<FetchResult>(this)) {
    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientMarketFixingModel::onDataLoaded);
}

int ClientMarketFixingModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(entries_.size());
}

int ClientMarketFixingModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientMarketFixingModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(entries_.size()))
        return {};

    const auto& f = entries_[static_cast<std::size_t>(index.row())];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case FixingDate: {
            const auto& d = f.fixing_date;
            return QString::fromStdString(
                std::format("{:04d}-{:02d}-{:02d}",
                    static_cast<int>(d.year()),
                    static_cast<unsigned>(d.month()),
                    static_cast<unsigned>(d.day())));
        }
        case Value:
            return QString::fromStdString(f.value);
        case Source:
            return f.source ? QString::fromStdString(*f.source) : QString{};
        case RecordedAt:
            return relative_time_helper::format(f.recorded_at);
        default: break;
        }
    }
    return {};
}

QVariant ClientMarketFixingModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case FixingDate:  return "Fixing Date";
    case Value:       return "Value";
    case Source:      return "Source";
    case RecordedAt:  return "Recorded At";
    default: return {};
    }
}

void ClientMarketFixingModel::refresh() {
    if (is_fetching_) return;
    fetch_data();
}

void ClientMarketFixingModel::fetch_data() {
    is_fetching_ = true;
    QPointer<ClientMarketFixingModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>(
                [&]() -> FetchResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false, .entries = {},
                                .total_available_count = 0,
                                .error_message = "Model destroyed",
                                .error_details = {}};
                    }
                    marketdata::messaging::get_market_fixings_request req;
                    req.series_id = boost::uuids::to_string(self->series_id_);

                    auto result =
                        self->clientManager_->process_authenticated_request(
                            std::move(req));
                    if (!result) {
                        return {.success = false, .entries = {},
                                .total_available_count = 0,
                                .error_message = QString::fromStdString(
                                    "Failed to fetch fixings: " +
                                    result.error()),
                                .error_details = {}};
                    }
                    const auto count = static_cast<std::uint32_t>(
                        result->total_available_count);
                    return {.success = true,
                            .entries = std::move(result->fixings),
                            .total_available_count = count,
                            .error_message = {},
                            .error_details = {}};
                }, "market fixings");
        });

    watcher_->setFuture(future);
}

void ClientMarketFixingModel::onDataLoaded() {
    is_fetching_ = false;
    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch fixings: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    entries_ = std::move(result.entries);
    total_available_count_ = result.total_available_count;
    endResetModel();

    emit dataLoaded();
}

}

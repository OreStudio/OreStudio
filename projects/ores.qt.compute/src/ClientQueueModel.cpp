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
#include "ores.qt/ClientQueueModel.hpp"

#include <QtConcurrent>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientQueueModel::ClientQueueModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientQueueModel::onDataLoaded);
}

int ClientQueueModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(rows_.size());
}

int ClientQueueModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientQueueModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= rows_.size())
        return {};

    const auto& r = rows_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case StreamName:
            return QString::fromStdString(r.name);
        case Subjects:
            return QString::fromStdString(r.subjects);
        case Messages:
            return static_cast<qlonglong>(r.message_count);
        case Bytes:
            return static_cast<qlonglong>(r.byte_count);
        case Consumers:
            return static_cast<qlonglong>(r.consumer_count);
        case CreatedAt:
            return relative_time_helper::format(r.created_at);
        case LastMessageAt:
            return r.last_message_at
                ? relative_time_helper::format(*r.last_message_at)
                : tr("—");
        default:
            return {};
        }
    }

    return {};
}

QVariant ClientQueueModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case StreamName:    return tr("Stream");
    case Subjects:      return tr("Subjects");
    case Messages:      return tr("Messages");
    case Bytes:         return tr("Bytes");
    case Consumers:     return tr("Consumers");
    case CreatedAt:     return tr("Created");
    case LastMessageAt: return tr("Last Message");
    default:            return {};
    }
}

void ClientQueueModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, skipping refresh";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError(tr("Not connected to server"));
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting stream fetch";
    is_fetching_ = true;

    QPointer<ClientQueueModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .rows = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            auto admin = self->clientManager_->admin();
            const auto streams = admin.list_streams();

            std::vector<queue_row> rows;
            rows.reserve(streams.size());
            for (const auto& si : streams) {
                queue_row r;
                r.id   = si.name;
                r.name = si.name;

                // Join subjects with ", "
                for (std::size_t i = 0; i < si.subjects.size(); ++i) {
                    if (i > 0)
                        r.subjects += ", ";
                    r.subjects += si.subjects[i];
                }

                r.message_count  = si.message_count;
                r.byte_count     = si.byte_count;
                r.consumer_count = si.consumer_count;
                r.created_at     = si.created_at;

                if (si.last_seq > 0)
                    r.last_message_at = si.last_message_at;

                rows.push_back(std::move(r));
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << rows.size() << " streams";
            return {.success = true, .rows = std::move(rows),
                    .error_message = {}, .error_details = {}};
        }, "streams");
    });

    watcher_->setFuture(future);
}

void ClientQueueModel::onDataLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch streams: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    rows_ = std::move(result.rows);
    endResetModel();

    BOOST_LOG_SEV(lg(), info) << "Loaded " << rows_.size() << " streams";
    emit dataLoaded();
}

const queue_row* ClientQueueModel::getRow(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= rows_.size())
        return nullptr;
    return &rows_[idx];
}

}

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

#include <unordered_map>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_hash.hpp>
#include <QtConcurrent>
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.mq/messaging/mq_protocol.hpp"
#include "ores.mq/domain/queue_scope_type.hpp"
#include "ores.mq/domain/queue_type.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

namespace {

std::string to_string(ores::mq::domain::queue_scope_type v) {
    switch (v) {
    case ores::mq::domain::queue_scope_type::party:  return "party";
    case ores::mq::domain::queue_scope_type::tenant: return "tenant";
    case ores::mq::domain::queue_scope_type::system: return "system";
    }
    return "unknown";
}

std::string to_string(ores::mq::domain::queue_type v) {
    switch (v) {
    case ores::mq::domain::queue_type::task:    return "task";
    case ores::mq::domain::queue_type::channel: return "channel";
    }
    return "unknown";
}

} // namespace

ClientQueueModel::ClientQueueModel(ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
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
        case QueueName:
            return QString::fromStdString(r.name);
        case Description:
            return QString::fromStdString(r.description);
        case ScopeType:
            return QString::fromStdString(r.scope_type);
        case QueueType:
            return QString::fromStdString(r.queue_type);
        case PendingCount:
            return static_cast<qlonglong>(r.pending_count);
        case ProcessingCount:
            return static_cast<qlonglong>(r.processing_count);
        case TotalArchived:
            return static_cast<qlonglong>(r.total_archived);
        case CreatedAt:
            return relative_time_helper::format(r.created_at);
        case StatsRecordedAt:
            return relative_time_helper::format(r.stats_recorded_at);
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
    case QueueName:       return tr("Queue Name");
    case Description:     return tr("Description");
    case ScopeType:       return tr("Scope");
    case QueueType:       return tr("Type");
    case PendingCount:    return tr("Pending");
    case ProcessingCount: return tr("Processing");
    case TotalArchived:   return tr("Archived");
    case CreatedAt:       return tr("Created");
    case StatsRecordedAt: return tr("Last Stats");
    default:              return {};
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

    BOOST_LOG_SEV(lg(), debug) << "Starting queue fetch";
    is_fetching_ = true;

    QPointer<ClientQueueModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .rows = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            // --- Request 1: list all queue definitions ---
            mq::messaging::get_queues_request queues_req;
            auto queues_payload = queues_req.serialize();
            frame queues_frame(message_type::get_queues_request, 0,
                               std::move(queues_payload));

            auto queues_result = self->clientManager_->sendRequest(
                std::move(queues_frame));
            if (!queues_result) {
                return {.success = false, .rows = {},
                        .error_message = tr("Failed to send get_queues request"),
                        .error_details = {}};
            }

            if (auto err = exception_helper::check_error_response(*queues_result)) {
                return {.success = false, .rows = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto queues_bytes = queues_result->decompressed_payload();
            if (!queues_bytes) {
                return {.success = false, .rows = {},
                        .error_message = tr("Failed to decompress queues response"),
                        .error_details = {}};
            }

            auto queues_resp = mq::messaging::get_queues_response::
                deserialize(*queues_bytes);
            if (!queues_resp || !queues_resp->success) {
                return {.success = false, .rows = {},
                        .error_message = queues_resp
                            ? QString::fromStdString(queues_resp->message)
                            : tr("Failed to deserialize queues response"),
                        .error_details = {}};
            }

            // --- Request 2: get stats for all queues ---
            mq::messaging::get_queue_stats_request stats_req;
            auto stats_payload = stats_req.serialize();
            frame stats_frame(message_type::get_queue_stats_request, 0,
                              std::move(stats_payload));

            auto stats_result = self->clientManager_->sendRequest(
                std::move(stats_frame));
            if (!stats_result) {
                return {.success = false, .rows = {},
                        .error_message = tr("Failed to send get_queue_stats request"),
                        .error_details = {}};
            }

            if (auto err = exception_helper::check_error_response(*stats_result)) {
                return {.success = false, .rows = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto stats_bytes = stats_result->decompressed_payload();
            if (!stats_bytes) {
                return {.success = false, .rows = {},
                        .error_message = tr("Failed to decompress stats response"),
                        .error_details = {}};
            }

            auto stats_resp = mq::messaging::get_queue_stats_response::
                deserialize(*stats_bytes);
            if (!stats_resp || !stats_resp->success) {
                return {.success = false, .rows = {},
                        .error_message = stats_resp
                            ? QString::fromStdString(stats_resp->message)
                            : tr("Failed to deserialize stats response"),
                        .error_details = {}};
            }

            // --- Merge: index stats by queue_id, keeping latest per queue ---
            using uuid = boost::uuids::uuid;
            std::unordered_map<uuid,
                const mq::domain::queue_stats*> stats_map;
            for (const auto& s : stats_resp->stats) {
                auto it = stats_map.find(s.queue_id);
                if (it == stats_map.end() ||
                    s.recorded_at > it->second->recorded_at) {
                    stats_map[s.queue_id] = &s;
                }
            }

            std::vector<queue_row> rows;
            rows.reserve(queues_resp->queues.size());
            for (const auto& qd : queues_resp->queues) {
                queue_row r;
                r.id          = qd.id;
                r.name        = qd.name;
                r.description = qd.description;
                r.scope_type  = to_string(qd.scope_type);
                r.queue_type  = to_string(qd.queue_type);
                r.created_at  = qd.created_at;
                r.is_active   = qd.is_active;

                if (auto it = stats_map.find(qd.id);
                    it != stats_map.end()) {
                    const auto& s = *it->second;
                    r.pending_count    = s.pending_count;
                    r.processing_count = s.processing_count;
                    r.total_archived   = s.total_archived;
                    r.stats_recorded_at = s.recorded_at;
                }

                rows.push_back(std::move(r));
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << rows.size() << " queues";
            return {.success = true, .rows = std::move(rows),
                    .error_message = {}, .error_details = {}};
        }, "queues");
    });

    watcher_->setFuture(future);
}

void ClientQueueModel::onDataLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch queues: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    rows_ = std::move(result.rows);
    endResetModel();

    BOOST_LOG_SEV(lg(), info) << "Loaded " << rows_.size() << " queues";
    emit dataLoaded();
}

const queue_row* ClientQueueModel::getRow(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= rows_.size())
        return nullptr;
    return &rows_[idx];
}

}

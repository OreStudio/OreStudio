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
#include <QtConcurrent>
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.mq/messaging/mq_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

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
            return QString::fromStdString(r.queue_name);
        case QueueLength:
            return static_cast<qlonglong>(r.queue_length);
        case TotalMessages:
            return static_cast<qlonglong>(r.total_messages);
        case NewestMsgAge:
            return r.newest_msg_age_sec
                ? QVariant(static_cast<int>(*r.newest_msg_age_sec))
                : QVariant{};
        case OldestMsgAge:
            return r.oldest_msg_age_sec
                ? QVariant(static_cast<int>(*r.oldest_msg_age_sec))
                : QVariant{};
        case IsUnlogged:
            return r.is_unlogged ? tr("Yes") : tr("No");
        case IsPartitioned:
            return r.is_partitioned ? tr("Yes") : tr("No");
        case CreatedAt:
            return relative_time_helper::format(r.created_at);
        case ScrapeTime:
            return r.scrape_time
                ? relative_time_helper::format(*r.scrape_time)
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
    case QueueName:     return tr("Queue Name");
    case QueueLength:   return tr("Length");
    case TotalMessages: return tr("Total Sent");
    case NewestMsgAge:  return tr("Newest (s)");
    case OldestMsgAge:  return tr("Oldest (s)");
    case IsUnlogged:    return tr("Unlogged");
    case IsPartitioned: return tr("Partitioned");
    case CreatedAt:     return tr("Created");
    case ScrapeTime:    return tr("Last Scraped");
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

            // --- Request 1: list all queues ---
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

            // --- Request 2: get metrics for all queues ---
            mq::messaging::get_queue_metrics_request metrics_req;
            auto metrics_payload = metrics_req.serialize();
            frame metrics_frame(message_type::get_queue_metrics_request, 0,
                                std::move(metrics_payload));

            auto metrics_result = self->clientManager_->sendRequest(
                std::move(metrics_frame));
            if (!metrics_result) {
                return {.success = false, .rows = {},
                        .error_message = tr("Failed to send get_queue_metrics request"),
                        .error_details = {}};
            }

            if (auto err = exception_helper::check_error_response(*metrics_result)) {
                return {.success = false, .rows = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto metrics_bytes = metrics_result->decompressed_payload();
            if (!metrics_bytes) {
                return {.success = false, .rows = {},
                        .error_message = tr("Failed to decompress metrics response"),
                        .error_details = {}};
            }

            auto metrics_resp = mq::messaging::get_queue_metrics_response::
                deserialize(*metrics_bytes);
            if (!metrics_resp || !metrics_resp->success) {
                return {.success = false, .rows = {},
                        .error_message = metrics_resp
                            ? QString::fromStdString(metrics_resp->message)
                            : tr("Failed to deserialize metrics response"),
                        .error_details = {}};
            }

            // --- Merge: index metrics by queue_name ---
            std::unordered_map<std::string,
                const mq::pgmq::queue_metrics*> metrics_map;
            for (const auto& m : metrics_resp->metrics)
                metrics_map.emplace(m.queue_name, &m);

            std::vector<queue_row> rows;
            rows.reserve(queues_resp->queues.size());
            for (const auto& qi : queues_resp->queues) {
                queue_row r;
                r.queue_name    = qi.queue_name;
                r.created_at    = qi.created_at;
                r.is_unlogged   = qi.is_unlogged;
                r.is_partitioned = qi.is_partitioned;

                if (auto it = metrics_map.find(qi.queue_name);
                    it != metrics_map.end()) {
                    const auto& m = *it->second;
                    r.queue_length       = m.queue_length;
                    r.total_messages     = m.total_messages;
                    r.newest_msg_age_sec = m.newest_msg_age_sec;
                    r.oldest_msg_age_sec = m.oldest_msg_age_sec;
                    r.scrape_time        = m.scrape_time;
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

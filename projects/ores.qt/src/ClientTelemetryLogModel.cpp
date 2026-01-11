/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/ClientTelemetryLogModel.hpp"

#include <QtConcurrent>
#include <QDateTime>
#include <boost/uuid/uuid_io.hpp>
#include "ores.comms/net/client_session.hpp"
#include "ores.telemetry/messaging/telemetry_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientTelemetryLogModel::
ClientTelemetryLogModel(ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent), clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientTelemetryLogModel::onLogsLoaded);

    // Default to last 24 hours
    end_time_ = std::chrono::system_clock::now();
    start_time_ = end_time_ - std::chrono::hours(24);
}

int ClientTelemetryLogModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(entries_.size());
}

int ClientTelemetryLogModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return Column::ColumnCount;
}

QVariant ClientTelemetryLogModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= entries_.size())
        return {};

    const auto& entry = entries_[row];

    if (role != Qt::DisplayRole)
        return {};

    switch (index.column()) {
    case Column::Timestamp: {
        const auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
            entry.timestamp.time_since_epoch()).count();
        return QDateTime::fromMSecsSinceEpoch(msecs).toString("yyyy-MM-dd hh:mm:ss.zzz");
    }
    case Column::Level:
        return QString::fromStdString(entry.level);
    case Column::Source:
        return QString::fromStdString(entry.source_name);
    case Column::Component:
        return QString::fromStdString(entry.component);
    case Column::Message:
        return QString::fromStdString(entry.message);
    default:
        return {};
    }
}

QVariant ClientTelemetryLogModel::
headerData(int section, Qt::Orientation orientation, int role) const {
    if (role != Qt::DisplayRole)
        return {};

    if (orientation == Qt::Horizontal) {
        switch (section) {
        case Column::Timestamp: return tr("Timestamp");
        case Column::Level: return tr("Level");
        case Column::Source: return tr("Source");
        case Column::Component: return tr("Component");
        case Column::Message: return tr("Message");
        default: return {};
        }
    }

    return {};
}

void ClientTelemetryLogModel::
load_session_logs(const boost::uuids::uuid& session_id) {
    BOOST_LOG_SEV(lg(), debug) << "Loading logs for session: "
                               << boost::uuids::to_string(session_id);
    current_session_id_ = session_id;
    current_offset_ = 0;
    fetch_logs();
}

void ClientTelemetryLogModel::
load_logs(std::chrono::system_clock::time_point start_time,
          std::chrono::system_clock::time_point end_time) {
    BOOST_LOG_SEV(lg(), debug) << "Loading logs for time range.";
    current_session_id_ = std::nullopt;
    start_time_ = start_time;
    end_time_ = end_time;
    current_offset_ = 0;
    fetch_logs();
}

void ClientTelemetryLogModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Loading page: offset=" << offset
                               << ", limit=" << limit;
    current_offset_ = offset;
    page_size_ = limit;
    fetch_logs();
}

void ClientTelemetryLogModel::clear() {
    if (!entries_.empty()) {
        beginResetModel();
        entries_.clear();
        total_available_count_ = 0;
        endResetModel();
    }
}

const telemetry::domain::telemetry_log_entry*
ClientTelemetryLogModel::get_entry(int row) const {
    if (row < 0 || row >= static_cast<int>(entries_.size()))
        return nullptr;
    return &entries_[row];
}

void ClientTelemetryLogModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 10000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
    }
}

void ClientTelemetryLogModel::
set_min_level(const std::optional<std::string>& level) {
    min_level_ = level;
}

void ClientTelemetryLogModel::
set_message_filter(const std::optional<std::string>& text) {
    message_filter_ = text;
}

void ClientTelemetryLogModel::fetch_logs() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot fetch logs: disconnected.";
        return;
    }

    is_fetching_ = true;

    // Clear existing data
    if (!entries_.empty()) {
        beginResetModel();
        entries_.clear();
        endResetModel();
    }

    QPointer<ClientTelemetryLogModel> self = this;
    auto session_id = current_session_id_;
    auto start = start_time_;
    auto end = end_time_;
    auto min_level = min_level_;
    auto msg_filter = message_filter_;
    auto offset = current_offset_;
    auto limit = page_size_;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, session_id, start, end, min_level,
                          msg_filter, offset, limit]() -> FetchResult {
            if (!self) return {false, {}, 0};

            telemetry::messaging::get_telemetry_logs_request request;
            request.query.start_time = start;
            request.query.end_time = end;
            request.query.session_id = session_id;
            request.query.min_level = min_level;
            request.query.message_contains = msg_filter;
            request.query.offset = offset;
            request.query.limit = limit;

            BOOST_LOG_SEV(lg(), debug) << "Fetching telemetry logs with offset="
                                       << offset << ", limit=" << limit;

            auto result = self->clientManager_->
                process_authenticated_request(std::move(request));

            if (!result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to fetch telemetry logs: "
                                           << comms::net::to_string(result.error());
                return {false, {}, 0};
            }

            if (!result->success) {
                BOOST_LOG_SEV(lg(), error) << "Server returned error: "
                                           << result->message;
                return {false, {}, 0};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << result->entries.size()
                                       << " log entries, total: "
                                       << result->total_count;

            return {true, std::move(result->entries), result->total_count};
        });

    watcher_->setFuture(future);
}

void ClientTelemetryLogModel::onLogsLoaded() {
    is_fetching_ = false;

    auto result = watcher_->result();
    if (result.success) {
        total_available_count_ = result.total_count;

        const int new_count = static_cast<int>(result.entries.size());
        if (new_count > 0) {
            beginInsertRows(QModelIndex(), 0, new_count - 1);
            entries_ = std::move(result.entries);
            endInsertRows();
        }

        BOOST_LOG_SEV(lg(), info) << "Loaded " << entries_.size()
                                  << " log entries. Total available: "
                                  << total_available_count_;
        emit dataLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Telemetry logs request failed.";
        emit loadError(tr("Failed to load telemetry logs from server"));
    }
}

}

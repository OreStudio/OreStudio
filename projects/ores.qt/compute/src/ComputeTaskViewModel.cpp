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
#include "ores.qt/ComputeTaskViewModel.hpp"

#include <unordered_map>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

QString ComputeTaskViewModel::format_state(int s) {
    switch (s) {
    case 1: return QObject::tr("Inactive");
    case 2: return QObject::tr("Unsent");
    case 4: return QObject::tr("Running");
    case 5: return QObject::tr("Done");
    default: return QString::number(s);
    }
}

QString ComputeTaskViewModel::format_outcome(int o) {
    switch (o) {
    case 0: return QObject::tr("Pending");
    case 1: return QObject::tr("Success");
    case 3: return QObject::tr("Failed");
    case 4: return QObject::tr("No Reply");
    default: return QString::number(o);
    }
}

namespace {

QString format_duration(const compute::domain::result& r) {
    if (r.server_state != 5 ||
            r.received_at == std::chrono::system_clock::time_point{} ||
            r.recorded_at == std::chrono::system_clock::time_point{}) {
        return QObject::tr("—");
    }
    const auto secs = std::chrono::duration_cast<std::chrono::seconds>(
        r.received_at - r.recorded_at).count();
    if (secs < 60)
        return QObject::tr("%1s").arg(secs);
    if (secs < 3600)
        return QObject::tr("%1m %2s").arg(secs / 60).arg(secs % 60);
    return QObject::tr("%1h %2m").arg(secs / 3600).arg((secs % 3600) / 60);
}

} // namespace

ComputeTaskViewModel::ComputeTaskViewModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {
    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ComputeTaskViewModel::onTasksLoaded);
}

int ComputeTaskViewModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(tasks_.size());
}

int ComputeTaskViewModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ComputeTaskViewModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid()) return {};
    const auto row = static_cast<std::size_t>(index.row());
    if (row >= tasks_.size()) return {};
    const auto& t = tasks_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Label:
            return tr("Batch #%1 / Task %2")
                .arg(t.batch_ordinal).arg(t.task_ordinal);
        case State:
            return format_state(t.result.server_state);
        case Outcome:
            return format_outcome(t.result.outcome);
        case Host: {
            if (t.result.host_id == boost::uuids::uuid{})
                return tr("—");
            const auto quuid = QString::fromStdString(
                boost::uuids::to_string(t.result.host_id));
            return host_name_cache_
                ? host_name_cache_->display_name_for(quuid)
                : quuid.left(8);
        }
        case Duration:
            return format_duration(t.result);
        case Batch:
            return QString::fromStdString(t.batch.external_ref);
        case Received:
            return relative_time_helper::format(t.result.received_at);
        default:
            return {};
        }
    }

    if (role == Qt::ToolTipRole) {
        switch (index.column()) {
        case Label:
            return QString::fromStdString(boost::uuids::to_string(t.result.id));
        case Host:
            if (host_name_cache_ && t.result.host_id != boost::uuids::uuid{})
                return QString::fromStdString(
                    boost::uuids::to_string(t.result.host_id));
            return {};
        case Batch:
            return QString::fromStdString(boost::uuids::to_string(t.batch.id));
        default:
            return {};
        }
    }

    return {};
}

QVariant ComputeTaskViewModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole) return {};
    switch (section) {
    case Label:    return tr("Task");
    case State:    return tr("State");
    case Outcome:  return tr("Outcome");
    case Host:     return tr("Host");
    case Duration: return tr("Duration");
    case Batch:    return tr("Batch");
    case Received: return tr("Received");
    default:       return {};
    }
}

void ComputeTaskViewModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress.";
        return;
    }
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh: disconnected.";
        emit loadError("Not connected to server");
        return;
    }
    if (!tasks_.empty()) {
        beginResetModel();
        tasks_.clear();
        endResetModel();
    }
    fetch_tasks();
}

void ComputeTaskViewModel::fetch_tasks() {
    is_fetching_ = true;
    QPointer<ComputeTaskViewModel> self = this;

    watcher_->setFuture(QtConcurrent::run(
        [self]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>(
                [&]() -> FetchResult {
                    if (!self || !self->clientManager_)
                        return {.success = false,
                                .error_message = "Model was destroyed"};

                    // --- Batches ---
                    compute::messaging::list_batches_request breq;
                    breq.limit = 1000;
                    auto br = self->clientManager_->
                        process_authenticated_request(std::move(breq));
                    if (!br)
                        return {.success = false,
                                .error_message = QString::fromStdString(
                                    "Failed to fetch batches: " + br.error())};

                    std::unordered_map<std::string, compute::domain::batch> batch_map;
                    int batch_idx = 0;
                    std::unordered_map<std::string, int> batch_ordinals;
                    for (auto& b : br->batches) {
                        const auto key = boost::uuids::to_string(b.id);
                        batch_ordinals[key] = ++batch_idx;
                        batch_map.emplace(key, std::move(b));
                    }

                    // --- Workunits ---
                    compute::messaging::list_workunits_request wreq;
                    wreq.limit = 1000;
                    auto wr = self->clientManager_->
                        process_authenticated_request(std::move(wreq));
                    if (!wr)
                        return {.success = false,
                                .error_message = QString::fromStdString(
                                    "Failed to fetch workunits: " + wr.error())};

                    std::unordered_map<std::string, compute::domain::workunit> wu_map;
                    for (auto& w : wr->workunits)
                        wu_map.emplace(boost::uuids::to_string(w.id), std::move(w));

                    // --- Results ---
                    compute::messaging::list_results_request rreq;
                    rreq.limit = 1000;
                    auto rr = self->clientManager_->
                        process_authenticated_request(std::move(rreq));
                    if (!rr)
                        return {.success = false,
                                .error_message = QString::fromStdString(
                                    "Failed to fetch results: " + rr.error())};

                    // --- Join ---
                    // Count results per batch for task ordinals
                    std::unordered_map<std::string, int> task_counters;
                    std::vector<compute_task> tasks;
                    tasks.reserve(rr->results.size());

                    for (auto& res : rr->results) {
                        const auto wu_key =
                            boost::uuids::to_string(res.workunit_id);
                        auto wu_it = wu_map.find(wu_key);
                        if (wu_it == wu_map.end()) continue;

                        const auto batch_key =
                            boost::uuids::to_string(wu_it->second.batch_id);
                        auto b_it = batch_map.find(batch_key);
                        if (b_it == batch_map.end()) continue;

                        compute_task t;
                        t.result        = std::move(res);
                        t.workunit      = wu_it->second;
                        t.batch         = b_it->second;
                        t.batch_ordinal = batch_ordinals[batch_key];
                        t.task_ordinal  = ++task_counters[batch_key];
                        tasks.push_back(std::move(t));
                    }

                    BOOST_LOG_SEV(lg(), debug)
                        << "Joined " << tasks.size() << " tasks from "
                        << batch_map.size() << " batches";

                    return {.success = true, .tasks = std::move(tasks)};
                }, "compute tasks");
        }));
}

void ComputeTaskViewModel::onTasksLoaded() {
    is_fetching_ = false;
    const auto result = watcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load tasks: "
            << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }
    const int n = static_cast<int>(result.tasks.size());
    if (n > 0) {
        beginResetModel();
        tasks_ = std::move(result.tasks);
        endResetModel();
    }
    BOOST_LOG_SEV(lg(), info) << "Loaded " << n << " compute tasks.";
    emit dataLoaded();
}

const compute_task* ComputeTaskViewModel::get_task(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= tasks_.size()) return nullptr;
    return &tasks_[idx];
}

void ComputeTaskViewModel::set_host_name_cache(HostDisplayNameCache* cache) {
    host_name_cache_ = cache;
}

} // namespace ores::qt

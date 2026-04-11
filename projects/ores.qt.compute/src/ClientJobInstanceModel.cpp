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
#include "ores.qt/ClientJobInstanceModel.hpp"

#include <QColor>
#include <QtConcurrent>
#include "ores.scheduler.api/rfl/reflectors.hpp"
#include "ores.qt/ExceptionHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientJobInstanceModel::ClientJobInstanceModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientJobInstanceModel::onInstancesLoaded);
}

int ClientJobInstanceModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(instances_.size());
}

int ClientJobInstanceModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientJobInstanceModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid()) return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= instances_.size()) return {};

    const auto& inst = instances_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case JobName:    return QString::fromStdString(inst.job_name);
        case Status:     return QString::fromStdString(inst.status);
        case TriggeredAt: return QString::fromStdString(inst.triggered_at);
        case StartedAt:  return QString::fromStdString(inst.started_at);
        case Duration:
            if (inst.duration_ms)
                return tr("%1 ms").arg(*inst.duration_ms);
            return tr("—");
        case ActionType: return QString::fromStdString(inst.action_type);
        case ErrorMessage:
            return inst.error_message.empty()
                ? QVariant{}
                : QString::fromStdString(inst.error_message);
        default: return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        if (index.column() == Status) {
            if (inst.status == "succeeded")
                return QColor(Qt::darkGreen);
            if (inst.status == "failed")
                return QColor(Qt::red);
        }
    }

    return {};
}

QVariant ClientJobInstanceModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole) return {};

    switch (section) {
    case JobName:     return tr("Job Name");
    case Status:      return tr("Status");
    case TriggeredAt: return tr("Triggered At");
    case StartedAt:   return tr("Started At");
    case Duration:    return tr("Duration");
    case ActionType:  return tr("Action Type");
    case ErrorMessage:return tr("Error");
    default:          return {};
    }
}

void ClientJobInstanceModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Refreshing job instances.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError(tr("Not connected to server"));
        return;
    }

    if (!instances_.empty()) {
        beginResetModel();
        instances_.clear();
        endResetModel();
    }

    fetch_instances();
}

void ClientJobInstanceModel::fetch_instances() {
    is_fetching_ = true;
    QPointer<ClientJobInstanceModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                if (!self || !self->clientManager_)
                    return {false, {}, tr("Model destroyed"), {}};

                scheduler::messaging::get_job_instances_request req;
                req.limit = 200;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(req));

                if (!result)
                    return {false, {}, QString::fromStdString(
                        "Failed to fetch job instances: " + result.error()), {}};

                return {true, std::move(result->instances), {}, {}};
            }, "job instances");
        });

    watcher_->setFuture(future);
}

void ClientJobInstanceModel::onInstancesLoaded() {
    is_fetching_ = false;
    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load job instances: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    instances_ = std::move(result.instances);
    endResetModel();

    BOOST_LOG_SEV(lg(), info) << "Loaded " << instances_.size() << " job instances.";
    emit dataLoaded();
}

const scheduler::messaging::job_instance_summary*
ClientJobInstanceModel::getInstance(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= instances_.size()) return nullptr;
    return &instances_[idx];
}

}

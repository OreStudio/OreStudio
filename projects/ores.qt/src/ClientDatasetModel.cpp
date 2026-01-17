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
#include "ores.qt/ClientDatasetModel.hpp"

#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientDatasetModel::ClientDatasetModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      pulse_timer_(new QTimer(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientDatasetModel::onDatasetsLoaded);

    pulse_timer_->setInterval(pulse_interval_ms_);
    connect(pulse_timer_, &QTimer::timeout,
            this, &ClientDatasetModel::onPulseTimerTimeout);
}

int ClientDatasetModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(datasets_.size());
}

int ClientDatasetModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientDatasetModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(datasets_.size()))
        return {};

    const auto& dataset = datasets_[index.row()];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Name: return QString::fromStdString(dataset.name);
        case Catalog:
            return dataset.catalog_name
                ? QString::fromStdString(*dataset.catalog_name)
                : QString();
        case SubjectArea: return QString::fromStdString(dataset.subject_area_name);
        case Domain: return QString::fromStdString(dataset.domain_name);
        case Origin: return QString::fromStdString(dataset.origin_code);
        case Nature: return QString::fromStdString(dataset.nature_code);
        case Treatment: return QString::fromStdString(dataset.treatment_code);
        case SourceSystem: return QString::fromStdString(dataset.source_system_id);
        case AsOfDate: return relative_time_helper::format(dataset.as_of_date);
        case Version: return dataset.version;
        case RecordedBy: return QString::fromStdString(dataset.recorded_by);
        case RecordedAt: return relative_time_helper::format(dataset.recorded_at);
        default: return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(dataset.id);
    }

    return {};
}

QVariant ClientDatasetModel::headerData(int section,
    Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Name: return tr("Name");
    case Catalog: return tr("Catalog");
    case SubjectArea: return tr("Subject Area");
    case Domain: return tr("Domain");
    case Origin: return tr("Origin");
    case Nature: return tr("Nature");
    case Treatment: return tr("Treatment");
    case SourceSystem: return tr("Source System");
    case AsOfDate: return tr("As Of Date");
    case Version: return tr("Version");
    case RecordedBy: return tr("Recorded By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientDatasetModel::refresh() {
    if (!clientManager_ || !clientManager_->isConnected() || is_fetching_)
        return;

    is_fetching_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Fetching datasets...";

    auto task = [cm = clientManager_]() -> FetchResult {
        dq::messaging::get_datasets_request request;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_datasets_request,
            0, std::move(payload));

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) {
            return {false, {}};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, {}};
        }

        auto response = dq::messaging::get_datasets_response::deserialize(
            *payload_result);
        if (!response) {
            return {false, {}};
        }

        return {true, std::move(response->datasets)};
    };

    watcher_->setFuture(QtConcurrent::run(task));
}

void ClientDatasetModel::onDatasetsLoaded() {
    is_fetching_ = false;

    FetchResult result;
    try {
        result = watcher_->result();
    } catch (const std::exception& e) {
        exception_helper::handle_fetch_exception(e, tr("datasets"), lg(),
            [this](const QString& msg, const QString& details) {
                emit loadError(msg, details);
            });
        return;
    }

    if (result.success) {
        beginResetModel();
        auto old_datasets = std::move(datasets_);
        datasets_ = std::move(result.datasets);
        update_recent_datasets();
        endResetModel();

        BOOST_LOG_SEV(lg(), debug) << "Loaded " << datasets_.size()
                                   << " datasets";
        emit dataLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load datasets";
        emit loadError(tr("Failed to load datasets"));
    }
}

const dq::domain::dataset* ClientDatasetModel::getDataset(
    int row) const {
    if (row < 0 || row >= static_cast<int>(datasets_.size()))
        return nullptr;
    return &datasets_[row];
}

void ClientDatasetModel::update_recent_datasets() {
    recent_dataset_ids_.clear();
    QDateTime now = QDateTime::currentDateTime();
    last_reload_time_ = now;

    for (const auto& dataset : datasets_) {
        auto recorded = QDateTime::fromSecsSinceEpoch(
            std::chrono::duration_cast<std::chrono::seconds>(
                dataset.recorded_at.time_since_epoch()).count());

        if (recorded.secsTo(now) <= 60) {
            recent_dataset_ids_.insert(boost::uuids::to_string(dataset.id));
        }
    }

    if (!recent_dataset_ids_.empty()) {
        pulse_count_ = 0;
        pulse_state_ = true;
        pulse_timer_->start();
    }
}

void ClientDatasetModel::onPulseTimerTimeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    if (pulse_count_ >= max_pulse_cycles_) {
        pulse_timer_->stop();
        recent_dataset_ids_.clear();
    }

    emit dataChanged(index(0, 0),
                     index(rowCount() - 1, columnCount() - 1),
                     {Qt::ForegroundRole});
}

QVariant ClientDatasetModel::recency_foreground_color(
    const boost::uuids::uuid& id) const {
    if (recent_dataset_ids_.contains(boost::uuids::to_string(id)) && pulse_state_) {
        return QColor(100, 200, 100);
    }
    return {};
}

}

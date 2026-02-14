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

namespace {
    std::string dataset_key_extractor(const dq::domain::dataset& e) {
        return boost::uuids::to_string(e.id);
    }
}

ClientDatasetModel::ClientDatasetModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(dataset_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientDatasetModel::onDatasetsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientDatasetModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientDatasetModel::onPulsingComplete);
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
        case Code: return QString::fromStdString(dataset.code);
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
        case ModifiedBy: return QString::fromStdString(dataset.modified_by);
        case RecordedAt: return relative_time_helper::format(dataset.recorded_at);
        case Tags: return QString();
        default: return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(dataset.id);
    }

    // Custom roles for Tags column - return origin/nature/treatment data
    if (index.column() == Tags) {
        if (role == OriginRole) {
            return QString::fromStdString(dataset.origin_code);
        }
        if (role == NatureRole) {
            return QString::fromStdString(dataset.nature_code);
        }
        if (role == TreatmentRole) {
            return QString::fromStdString(dataset.treatment_code);
        }
    }

    return {};
}

QVariant ClientDatasetModel::headerData(int section,
    Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Name: return tr("Name");
    case Code: return tr("Code");
    case Catalog: return tr("Catalog");
    case SubjectArea: return tr("Subject Area");
    case Domain: return tr("Domain");
    case Origin: return tr("Origin");
    case Nature: return tr("Nature");
    case Treatment: return tr("Treatment");
    case SourceSystem: return tr("Source");
    case AsOfDate: return tr("As Of Date");
    case Version: return tr("Version");
    case ModifiedBy: return tr("Modified By");
    case RecordedAt: return tr("Recorded At");
    case Tags: return tr("Dimensions");
    default: return {};
    }
}

void ClientDatasetModel::refresh() {
    if (!clientManager_ || !clientManager_->isConnected() || is_fetching_)
        return;

    is_fetching_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Fetching datasets...";

    QPointer<ClientDatasetModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .datasets = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_datasets_request request;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                comms::messaging::message_type::get_datasets_request,
                0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .datasets = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .datasets = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .datasets = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = dq::messaging::get_datasets_response::deserialize(
                *payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .datasets = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->datasets.size()
                                       << " datasets";
            return {.success = true, .datasets = std::move(response->datasets),
                    .error_message = {}, .error_details = {}};
        }, "datasets");
    });

    watcher_->setFuture(future);
}

void ClientDatasetModel::onDatasetsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch datasets: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    datasets_ = std::move(result.datasets);
    endResetModel();

    const bool has_recent = recencyTracker_.update(datasets_);
    if (has_recent && !pulseManager_->is_pulsing()) {
        pulseManager_->start_pulsing();
        BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                   << " datasets newer than last reload";
    }

    BOOST_LOG_SEV(lg(), debug) << "Loaded " << datasets_.size()
                               << " datasets";
    emit dataLoaded();
}

const dq::domain::dataset* ClientDatasetModel::getDataset(
    int row) const {
    if (row < 0 || row >= static_cast<int>(datasets_.size()))
        return nullptr;
    return &datasets_[row];
}

void ClientDatasetModel::onPulseStateChanged(bool /*isOn*/) {
    if (!datasets_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientDatasetModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

QVariant ClientDatasetModel::recency_foreground_color(
    const boost::uuids::uuid& id) const {
    if (recencyTracker_.is_recent(boost::uuids::to_string(id)) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

}

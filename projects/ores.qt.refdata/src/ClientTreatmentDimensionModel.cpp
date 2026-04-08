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
#include "ores.qt/ClientTreatmentDimensionModel.hpp"

#include <QtConcurrent>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq.api/messaging/dimension_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string treatment_dimension_key_extractor(const dq::domain::treatment_dimension& e) {
        return e.code;
    }
}

ClientTreatmentDimensionModel::ClientTreatmentDimensionModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(treatment_dimension_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientTreatmentDimensionModel::onDimensionsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientTreatmentDimensionModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientTreatmentDimensionModel::onPulsingComplete);
}

int ClientTreatmentDimensionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(dimensions_.size());
}

int ClientTreatmentDimensionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientTreatmentDimensionModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(dimensions_.size()))
        return {};

    const auto& dim = dimensions_[index.row()];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code: return QString::fromStdString(dim.code);
        case Name: return QString::fromStdString(dim.name);
        case Description: return QString::fromStdString(dim.description);
        case Version: return dim.version;
        case ModifiedBy: return QString::fromStdString(dim.modified_by);
        case RecordedAt: return relative_time_helper::format(dim.recorded_at);
        default: return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(dim.code);
    }

    return {};
}

QVariant ClientTreatmentDimensionModel::headerData(int section,
    Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Code: return tr("Code");
    case Name: return tr("Name");
    case Description: return tr("Description");
    case Version: return tr("Version");
    case ModifiedBy: return tr("Modified By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientTreatmentDimensionModel::refresh() {
    if (!clientManager_ || !clientManager_->isConnected() || is_fetching_)
        return;

    is_fetching_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Fetching treatment dimensions...";

    QPointer<ClientTreatmentDimensionModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .dimensions = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_treatment_dimensions_request request;
            auto response_result = self->clientManager_->process_authenticated_request(std::move(request));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .dimensions = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response_result->treatment_dimensions.size()
                                       << " treatment dimensions";
            return {.success = true, .dimensions = std::move(response_result->treatment_dimensions),
                    .error_message = {}, .error_details = {}};
        }, "treatment dimensions");
    });

    watcher_->setFuture(future);
}

void ClientTreatmentDimensionModel::onDimensionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch treatment dimensions: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    dimensions_ = std::move(result.dimensions);
    endResetModel();

    const bool has_recent = recencyTracker_.update(dimensions_);
    if (has_recent && !pulseManager_->is_pulsing()) {
        pulseManager_->start_pulsing();
        BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                   << " dimensions newer than last reload";
    }

    BOOST_LOG_SEV(lg(), debug) << "Loaded " << dimensions_.size()
                               << " treatment dimensions";
    emit dataLoaded();
}

const dq::domain::treatment_dimension* ClientTreatmentDimensionModel::getDimension(
    int row) const {
    if (row < 0 || row >= static_cast<int>(dimensions_.size()))
        return nullptr;
    return &dimensions_[row];
}

QVariant ClientTreatmentDimensionModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientTreatmentDimensionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!dimensions_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientTreatmentDimensionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

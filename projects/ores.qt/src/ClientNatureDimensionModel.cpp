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
#include "ores.qt/ClientNatureDimensionModel.hpp"

#include <QtConcurrent>
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

namespace {
    std::string nature_dimension_key_extractor(const dq::domain::nature_dimension& e) {
        return e.code;
    }
}

ClientNatureDimensionModel::ClientNatureDimensionModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(nature_dimension_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientNatureDimensionModel::onDimensionsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientNatureDimensionModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientNatureDimensionModel::onPulsingComplete);
}

int ClientNatureDimensionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(dimensions_.size());
}

int ClientNatureDimensionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientNatureDimensionModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= dimensions_.size())
        return {};

    const auto& dimension = dimensions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code:
            return QString::fromStdString(dimension.code);
        case Name:
            return QString::fromStdString(dimension.name);
        case Description:
            return QString::fromStdString(dimension.description);
        case Version:
            return dimension.version;
        case ModifiedBy:
            return QString::fromStdString(dimension.modified_by);
        case RecordedAt:
            return relative_time_helper::format(dimension.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(dimension.code);
    }

    return {};
}

QVariant ClientNatureDimensionModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Code:
        return tr("Code");
    case Name:
        return tr("Name");
    case Description:
        return tr("Description");
    case Version:
        return tr("Version");
    case ModifiedBy:
        return tr("Modified By");
    case RecordedAt:
        return tr("Recorded At");
    default:
        return {};
    }
}

void ClientNatureDimensionModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, skipping refresh";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting nature dimension fetch";
    is_fetching_ = true;

    QPointer<ClientNatureDimensionModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .dimensions = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_nature_dimensions_request request;
            auto payload = request.serialize();

            frame request_frame(
                message_type::get_nature_dimensions_request,
                0, std::move(payload)
            );

            auto response_result = self->clientManager_->sendRequest(
                std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .dimensions = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .dimensions = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .dimensions = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = dq::messaging::get_nature_dimensions_response::
                deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .dimensions = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->dimensions.size()
                                       << " nature dimensions";
            return {.success = true, .dimensions = std::move(response->dimensions),
                    .error_message = {}, .error_details = {}};
        }, "nature dimensions");
    });

    watcher_->setFuture(future);
}

void ClientNatureDimensionModel::onDimensionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch nature dimensions: "
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

    BOOST_LOG_SEV(lg(), info) << "Loaded " << dimensions_.size() << " nature dimensions";
    emit dataLoaded();
}

const dq::domain::nature_dimension*
ClientNatureDimensionModel::getDimension(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= dimensions_.size())
        return nullptr;
    return &dimensions_[idx];
}

QVariant ClientNatureDimensionModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientNatureDimensionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!dimensions_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientNatureDimensionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

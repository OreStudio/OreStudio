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
#include "ores.qt/ClientPartyStatusModel.hpp"

#include <QtConcurrent>
#include "ores.refdata/messaging/party_status_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

namespace {
    std::string party_status_key_extractor(const refdata::domain::party_status& e) {
        return e.code;
    }
}

ClientPartyStatusModel::ClientPartyStatusModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(party_status_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientPartyStatusModel::onStatussLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientPartyStatusModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientPartyStatusModel::onPulsingComplete);
}

int ClientPartyStatusModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(statuses_.size());
}

int ClientPartyStatusModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientPartyStatusModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= statuses_.size())
        return {};

    const auto& status = statuses_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code:
            return QString::fromStdString(status.code);
        case Name:
            return QString::fromStdString(status.name);
        case Description:
            return QString::fromStdString(status.description);
        case DisplayOrder:
            return status.display_order;
        case Version:
            return status.version;
        case ModifiedBy:
            return QString::fromStdString(status.modified_by);
        case RecordedAt:
            return relative_time_helper::format(status.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(status.code);
    }

    return {};
}

QVariant ClientPartyStatusModel::headerData(
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
    case DisplayOrder:
        return tr("Order");
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

void ClientPartyStatusModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, skipping refresh";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting party status fetch";
    is_fetching_ = true;

    QPointer<ClientPartyStatusModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .statuses = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            refdata::messaging::get_party_statuses_request request;
            auto payload = request.serialize();

            frame request_frame(
                message_type::get_party_statuses_request,
                0, std::move(payload)
            );

            auto response_result = self->clientManager_->sendRequest(
                std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .statuses = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .statuses = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .statuses = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = refdata::messaging::get_party_statuses_response::
                deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .statuses = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->statuses.size()
                                       << " party statuses";
            return {.success = true, .statuses = std::move(response->statuses),
                    .error_message = {}, .error_details = {}};
        }, "party statuses");
    });

    watcher_->setFuture(future);
}

void ClientPartyStatusModel::onStatussLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch party statuses: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    statuses_ = std::move(result.statuses);
    endResetModel();

    const bool has_recent = recencyTracker_.update(statuses_);
    if (has_recent && !pulseManager_->is_pulsing()) {
        pulseManager_->start_pulsing();
        BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                   << " party statuses newer than last reload";
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << statuses_.size() << " party statuses";
    emit dataLoaded();
}

const refdata::domain::party_status*
ClientPartyStatusModel::getStatus(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= statuses_.size())
        return nullptr;
    return &statuses_[idx];
}

QVariant ClientPartyStatusModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientPartyStatusModel::onPulseStateChanged(bool /*isOn*/) {
    if (!statuses_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientPartyStatusModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

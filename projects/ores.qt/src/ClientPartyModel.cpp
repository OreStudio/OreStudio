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
#include "ores.qt/ClientPartyModel.hpp"

#include <QtConcurrent>
#include "ores.refdata/messaging/party_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

namespace {
    std::string party_key_extractor(const refdata::domain::party& e) {
        return e.short_code;
    }
}

ClientPartyModel::ClientPartyModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(party_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientPartyModel::onPartysLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientPartyModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientPartyModel::onPulsingComplete);
}

int ClientPartyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(parties_.size());
}

int ClientPartyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientPartyModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= parties_.size())
        return {};

    const auto& party = parties_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case ShortCode:
            return QString::fromStdString(party.short_code);
        case FullName:
            return QString::fromStdString(party.full_name);
        case PartyType:
            return QString::fromStdString(party.party_type);
        case Status:
            return QString::fromStdString(party.status);
        case BusinessCenterCode:
            return QString::fromStdString(party.business_center_code);
        case Version:
            return party.version;
        case RecordedBy:
            return QString::fromStdString(party.recorded_by);
        case RecordedAt:
            return relative_time_helper::format(party.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(party.short_code);
    }

    return {};
}

QVariant ClientPartyModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case ShortCode:
        return tr("Code");
    case FullName:
        return tr("Name");
    case PartyType:
        return tr("Type");
    case Status:
        return tr("Status");
    case BusinessCenterCode:
        return tr("Business Center");
    case Version:
        return tr("Version");
    case RecordedBy:
        return tr("Recorded By");
    case RecordedAt:
        return tr("Recorded At");
    default:
        return {};
    }
}

void ClientPartyModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, skipping refresh";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting party fetch";
    is_fetching_ = true;

    QPointer<ClientPartyModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .parties = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            refdata::messaging::get_parties_request request;
            auto payload = request.serialize();

            frame request_frame(
                message_type::get_parties_request,
                0, std::move(payload)
            );

            auto response_result = self->clientManager_->sendRequest(
                std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .parties = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .parties = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .parties = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = refdata::messaging::get_parties_response::
                deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .parties = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->parties.size()
                                       << " parties";
            return {.success = true, .parties = std::move(response->parties),
                    .error_message = {}, .error_details = {}};
        }, "parties");
    });

    watcher_->setFuture(future);
}

void ClientPartyModel::onPartysLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch parties: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    parties_ = std::move(result.parties);
    endResetModel();

    const bool has_recent = recencyTracker_.update(parties_);
    if (has_recent && !pulseManager_->is_pulsing()) {
        pulseManager_->start_pulsing();
        BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                   << " parties newer than last reload";
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << parties_.size() << " parties";
    emit dataLoaded();
}

const refdata::domain::party*
ClientPartyModel::getParty(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= parties_.size())
        return nullptr;
    return &parties_[idx];
}

QVariant ClientPartyModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientPartyModel::onPulseStateChanged(bool /*isOn*/) {
    if (!parties_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientPartyModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

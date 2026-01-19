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
#include "ores.qt/ClientCodingSchemeAuthorityTypeModel.hpp"

#include <QtConcurrent>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string coding_scheme_authority_type_key_extractor(const dq::domain::coding_scheme_authority_type& e) {
        return e.code;
    }
}

ClientCodingSchemeAuthorityTypeModel::ClientCodingSchemeAuthorityTypeModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(coding_scheme_authority_type_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientCodingSchemeAuthorityTypeModel::onAuthorityTypesLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
        this, &ClientCodingSchemeAuthorityTypeModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
        this, &ClientCodingSchemeAuthorityTypeModel::onPulsingComplete);
}

int ClientCodingSchemeAuthorityTypeModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(authorityTypes_.size());
}

int ClientCodingSchemeAuthorityTypeModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientCodingSchemeAuthorityTypeModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(authorityTypes_.size()))
        return {};

    const auto& at = authorityTypes_[index.row()];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code: return QString::fromStdString(at.code);
        case Name: return QString::fromStdString(at.name);
        case Description: return QString::fromStdString(at.description);
        case Version: return at.version;
        case RecordedBy: return QString::fromStdString(at.recorded_by);
        case RecordedAt: return relative_time_helper::format(at.recorded_at);
        default: return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(at.code);
    }

    return {};
}

QVariant ClientCodingSchemeAuthorityTypeModel::headerData(int section,
    Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Code: return tr("Code");
    case Name: return tr("Name");
    case Description: return tr("Description");
    case Version: return tr("Version");
    case RecordedBy: return tr("Recorded By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientCodingSchemeAuthorityTypeModel::refresh() {
    if (!clientManager_ || !clientManager_->isConnected() || is_fetching_)
        return;

    is_fetching_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Fetching coding scheme authority types...";

    QPointer<ClientCodingSchemeAuthorityTypeModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .authority_types = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_coding_scheme_authority_types_request request;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                comms::messaging::message_type::get_coding_scheme_authority_types_request,
                0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .authority_types = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .authority_types = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .authority_types = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = dq::messaging::get_coding_scheme_authority_types_response::deserialize(
                *payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .authority_types = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->authority_types.size()
                                       << " coding scheme authority types";
            return {.success = true, .authority_types = std::move(response->authority_types),
                    .error_message = {}, .error_details = {}};
        }, "coding scheme authority types");
    });

    watcher_->setFuture(future);
}

void ClientCodingSchemeAuthorityTypeModel::onAuthorityTypesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch coding scheme authority types: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    authorityTypes_ = std::move(result.authority_types);
    const bool has_recent = recencyTracker_.update(authorityTypes_);
    if (has_recent && !pulseManager_->is_pulsing()) {
        pulseManager_->start_pulsing();
        BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                   << " authority_types newer than last reload";
    }
    endResetModel();

    BOOST_LOG_SEV(lg(), debug) << "Loaded " << authorityTypes_.size()
                               << " coding scheme authority types";
    emit dataLoaded();
}

const dq::domain::coding_scheme_authority_type* ClientCodingSchemeAuthorityTypeModel::getAuthorityType(
    int row) const {
    if (row < 0 || row >= static_cast<int>(authorityTypes_.size()))
        return nullptr;
    return &authorityTypes_[row];
}

void ClientCodingSchemeAuthorityTypeModel::onPulseStateChanged(bool /*isOn*/) {
    if (!authorityTypes_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientCodingSchemeAuthorityTypeModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

QVariant ClientCodingSchemeAuthorityTypeModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

}

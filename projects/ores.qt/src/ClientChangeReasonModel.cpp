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
#include "ores.qt/ClientChangeReasonModel.hpp"

#include <QtConcurrent>
#include <QBrush>
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

namespace {
    std::string change_reason_key_extractor(const dq::domain::change_reason& r) {
        return r.code;
    }
}

ClientChangeReasonModel::ClientChangeReasonModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(change_reason_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientChangeReasonModel::onReasonsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
        this, &ClientChangeReasonModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
        this, &ClientChangeReasonModel::onPulsingComplete);
}

int ClientChangeReasonModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(reasons_.size());
}

int ClientChangeReasonModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientChangeReasonModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= reasons_.size())
        return {};

    const auto& reason = reasons_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code:
            return QString::fromStdString(reason.code);
        case CategoryCode:
            return QString::fromStdString(reason.category_code);
        case AppliesToAmend:
            return reason.applies_to_amend ? tr("Yes") : tr("No");
        case AppliesToDelete:
            return reason.applies_to_delete ? tr("Yes") : tr("No");
        case RequiresCommentary:
            return reason.requires_commentary ? tr("Yes") : tr("No");
        case DisplayOrder:
            return reason.display_order;
        case Version:
            return reason.version;
        case RecordedBy:
            return QString::fromStdString(reason.recorded_by);
        case RecordedAt:
            return relative_time_helper::format(reason.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(reason.code);
    }

    return {};
}

QVariant ClientChangeReasonModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Code:
        return tr("Code");
    case CategoryCode:
        return tr("Category");
    case AppliesToAmend:
        return tr("Amend");
    case AppliesToDelete:
        return tr("Delete");
    case RequiresCommentary:
        return tr("Commentary");
    case DisplayOrder:
        return tr("Order");
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

void ClientChangeReasonModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, skipping refresh";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting reason fetch";
    is_fetching_ = true;

    QPointer<ClientChangeReasonModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .reasons = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_change_reasons_request request;
            auto payload = request.serialize();

            frame request_frame(
                message_type::get_change_reasons_request,
                0, std::move(payload)
            );

            auto response_result = self->clientManager_->sendRequest(
                std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .reasons = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .reasons = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .reasons = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = dq::messaging::get_change_reasons_response::
                deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .reasons = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->reasons.size()
                                       << " change reasons";
            return {.success = true, .reasons = std::move(response->reasons),
                    .error_message = {}, .error_details = {}};
        }, "change reasons");
    });

    watcher_->setFuture(future);
}

void ClientChangeReasonModel::onReasonsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch change reasons: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    reasons_ = std::move(result.reasons);
    endResetModel();

    const bool has_recent = recencyTracker_.update(reasons_);
    if (has_recent && !pulseManager_->is_pulsing()) {
        pulseManager_->start_pulsing();
        BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                   << " reasons newer than last reload";
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << reasons_.size() << " change reasons";
    emit dataLoaded();
}

const dq::domain::change_reason*
ClientChangeReasonModel::getReason(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= reasons_.size())
        return nullptr;
    return &reasons_[idx];
}

QVariant ClientChangeReasonModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientChangeReasonModel::onPulseStateChanged(bool /*isOn*/) {
    if (!reasons_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientChangeReasonModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

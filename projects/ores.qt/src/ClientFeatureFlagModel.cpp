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
#include "ores.qt/ClientFeatureFlagModel.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;

namespace {
    std::string feature_flag_key_extractor(const variability::domain::feature_flags& e) {
        return e.name;
    }
}

ClientFeatureFlagModel::ClientFeatureFlagModel(ClientManager* clientManager,
                                               QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(feature_flag_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientFeatureFlagModel::onFeatureFlagsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientFeatureFlagModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientFeatureFlagModel::onPulsingComplete);
}

int ClientFeatureFlagModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(flags_.size());
}

int ClientFeatureFlagModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientFeatureFlagModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = index.row();
    if (row < 0 || row >= static_cast<int>(flags_.size()))
        return {};

    const auto& flag = flags_[row];
    const auto column = static_cast<Column>(index.column());

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(flag.name);
    }

    if (role == Qt::DisplayRole) {
        switch (column) {
        case Name:
            return QString::fromStdString(flag.name);
        case Enabled:
            return flag.enabled ? tr("Yes") : tr("No");
        case Version:
            return QString::number(flag.version);
        case RecordedBy:
            return QString::fromStdString(flag.recorded_by);
        case RecordedAt:
            return relative_time_helper::format(flag.recorded_at);
        default:
            return {};
        }
    }

    return {};
}

QVariant ClientFeatureFlagModel::headerData(int section, Qt::Orientation orientation,
                                            int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (static_cast<Column>(section)) {
    case Name:
        return tr("Name");
    case Enabled:
        return tr("Enabled");
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

void ClientFeatureFlagModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, ignoring refresh request";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Not connected, cannot refresh";
        emit loadError("Not connected to server");
        return;
    }

    is_fetching_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Starting feature flags fetch";

    QPointer<ClientFeatureFlagModel> self = this;
    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .flags = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            variability::messaging::get_feature_flags_request request;
            auto payload = request.serialize();

            frame request_frame(message_type::get_feature_flags_request,
                0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(
                std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .flags = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .flags = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .flags = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = variability::messaging::get_feature_flags_response::
                deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .flags = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->feature_flags.size()
                                       << " feature flags";
            return {.success = true, .flags = std::move(response->feature_flags),
                    .error_message = {}, .error_details = {}};
        }, "feature flags");
    });

    watcher_->setFuture(future);
}

void ClientFeatureFlagModel::onFeatureFlagsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch feature flags: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    flags_ = std::move(result.flags);
    endResetModel();

    const bool has_recent = recencyTracker_.update(flags_);
    if (has_recent && !pulseManager_->is_pulsing()) {
        pulseManager_->start_pulsing();
        BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                   << " feature flags newer than last reload";
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << flags_.size() << " feature flags";
    emit dataLoaded();
}

const variability::domain::feature_flags*
ClientFeatureFlagModel::getFeatureFlag(int row) const {
    if (row < 0 || row >= static_cast<int>(flags_.size()))
        return nullptr;
    return &flags_[row];
}

void ClientFeatureFlagModel::onPulseStateChanged(bool /*isOn*/) {
    if (!flags_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientFeatureFlagModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

QVariant ClientFeatureFlagModel::recency_foreground_color(const std::string& name) const {
    if (recencyTracker_.is_recent(name) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

}

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
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::telemetry::log;

ClientFeatureFlagModel::ClientFeatureFlagModel(ClientManager* clientManager,
                                               QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      pulse_timer_(new QTimer(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientFeatureFlagModel::onFeatureFlagsLoaded);

    // Setup pulse timer for recency color updates
    connect(pulse_timer_, &QTimer::timeout,
            this, &ClientFeatureFlagModel::onPulseTimerTimeout);
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
        if (!self) return {false, {}};

        variability::messaging::list_feature_flags_request request;
        auto payload = request.serialize();

        frame request_frame(message_type::list_feature_flags_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send request";
            return {false, {}};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
            return {false, {}};
        }

        auto response = variability::messaging::list_feature_flags_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
            return {false, {}};
        }

        return {true, std::move(response->feature_flags)};
    });

    watcher_->setFuture(future);
}

void ClientFeatureFlagModel::onFeatureFlagsLoaded() {
    is_fetching_ = false;
    auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load feature flags";
        emit loadError("Failed to load feature flags from server");
        return;
    }

    beginResetModel();
    flags_ = std::move(result.flags);
    endResetModel();

    // Update the set of recent flags for recency coloring
    update_recent_flags();

    // Start the pulse timer if there are recent flags to highlight
    if (!recent_flag_names_.empty() && !pulse_timer_->isActive()) {
        pulse_count_ = 0;
        pulse_state_ = true;  // Start with highlight on
        pulse_timer_->start(pulse_interval_ms_);
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

void ClientFeatureFlagModel::update_recent_flags() {
    recent_flag_names_.clear();

    const QDateTime now = QDateTime::currentDateTime();

    // First load: set baseline timestamp, no highlighting
    if (!last_reload_time_.isValid()) {
        last_reload_time_ = now;
        BOOST_LOG_SEV(lg(), info) << "First load - setting baseline timestamp: "
                                  << last_reload_time_.toString(Qt::ISODate).toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Checking flags newer than last reload: "
                              << last_reload_time_.toString(Qt::ISODate).toStdString();

    // Find flags that have been modified since the last reload
    for (const auto& flag : flags_) {
        const auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
            flag.recorded_at.time_since_epoch()).count();
        QDateTime recordedAt = QDateTime::fromMSecsSinceEpoch(msecs);

        BOOST_LOG_SEV(lg(), debug) << "Flag " << flag.name << " recorded_at: "
                                   << recordedAt.toString(Qt::ISODate).toStdString()
                                   << " (msecs=" << msecs << ")";

        if (recordedAt.isValid() && recordedAt > last_reload_time_) {
            recent_flag_names_.insert(flag.name);
            BOOST_LOG_SEV(lg(), info) << "Flag " << flag.name << " is recent (recorded_at > last_reload)";
        }
    }

    // Update the reload timestamp for next comparison
    last_reload_time_ = now;

    BOOST_LOG_SEV(lg(), info) << "Found " << recent_flag_names_.size()
                              << " flags newer than last reload";
}

QVariant ClientFeatureFlagModel::recency_foreground_color(const std::string& name) const {
    if (recent_flag_names_.empty() ||
        recent_flag_names_.find(name) == recent_flag_names_.end()) {
        return {};
    }

    // Only show color when pulse_state_ is true (pulsing on)
    if (!pulse_state_) {
        return {};
    }

    // Use the stale indicator color (same as reload button)
    return color_constants::stale_indicator;
}

void ClientFeatureFlagModel::onPulseTimerTimeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    // Stop pulsing after max cycles but keep yellow color on
    if (pulse_count_ >= max_pulse_cycles_) {
        pulse_timer_->stop();
        pulse_state_ = true;  // Keep highlight on after pulsing stops
        BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete, staying highlighted";
    }

    // Emit dataChanged to update the view with new colors
    if (!flags_.empty()) {
        emit dataChanged(
            index(0, 0),
            index(static_cast<int>(flags_.size()) - 1, ColumnCount - 1),
            {Qt::ForegroundRole});
    }
}

}

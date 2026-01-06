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
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientFeatureFlagModel::onFeatureFlagsLoaded);
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

    if (role == Qt::DisplayRole) {
        switch (column) {
        case Name:
            return QString::fromStdString(flag.name);
        case Enabled:
            return flag.enabled ? tr("Yes") : tr("No");
        case Description:
            return QString::fromStdString(flag.description);
        case RecordedBy:
            return QString::fromStdString(flag.recorded_by);
        default:
            return {};
        }
    }

    if (role == Qt::CheckStateRole && column == Enabled) {
        return flag.enabled ? Qt::Checked : Qt::Unchecked;
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
    case Description:
        return tr("Description");
    case RecordedBy:
        return tr("Modified By");
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

    BOOST_LOG_SEV(lg(), info) << "Loaded " << flags_.size() << " feature flags";
    emit dataLoaded();
}

const variability::domain::feature_flags*
ClientFeatureFlagModel::getFeatureFlag(int row) const {
    if (row < 0 || row >= static_cast<int>(flags_.size()))
        return nullptr;
    return &flags_[row];
}

}

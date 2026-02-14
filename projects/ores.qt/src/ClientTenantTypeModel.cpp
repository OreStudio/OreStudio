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
#include "ores.qt/ClientTenantTypeModel.hpp"

#include <QtConcurrent>
#include "ores.iam/messaging/tenant_type_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

namespace {
    std::string tenant_type_key_extractor(const iam::domain::tenant_type& e) {
        return e.type;
    }
}

ClientTenantTypeModel::ClientTenantTypeModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(tenant_type_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientTenantTypeModel::onTypesLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientTenantTypeModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientTenantTypeModel::onPulsingComplete);
}

int ClientTenantTypeModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(tenant_types_.size());
}

int ClientTenantTypeModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientTenantTypeModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= tenant_types_.size())
        return {};

    const auto& tenant_type = tenant_types_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Type:
            return QString::fromStdString(tenant_type.type);
        case Name:
            return QString::fromStdString(tenant_type.name);
        case Description:
            return QString::fromStdString(tenant_type.description);
        case DisplayOrder:
            return tenant_type.display_order;
        case Version:
            return tenant_type.version;
        case ModifiedBy:
            return QString::fromStdString(tenant_type.modified_by);
        case RecordedAt:
            return relative_time_helper::format(tenant_type.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(tenant_type.type);
    }

    return {};
}

QVariant ClientTenantTypeModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Type:
        return tr("Type");
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

void ClientTenantTypeModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, skipping refresh";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting tenant type fetch";
    is_fetching_ = true;

    QPointer<ClientTenantTypeModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .tenant_types = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            iam::messaging::get_tenant_types_request request;
            auto payload = request.serialize();

            frame request_frame(
                message_type::get_tenant_types_request,
                0, std::move(payload)
            );

            auto response_result = self->clientManager_->sendRequest(
                std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .tenant_types = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .tenant_types = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .tenant_types = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = iam::messaging::get_tenant_types_response::
                deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .tenant_types = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->types.size()
                                       << " tenant types";
            return {.success = true, .tenant_types = std::move(response->types),
                    .error_message = {}, .error_details = {}};
        }, "tenant types");
    });

    watcher_->setFuture(future);
}

void ClientTenantTypeModel::onTypesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch tenant types: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    tenant_types_ = std::move(result.tenant_types);
    endResetModel();

    const bool has_recent = recencyTracker_.update(tenant_types_);
    if (has_recent && !pulseManager_->is_pulsing()) {
        pulseManager_->start_pulsing();
        BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                   << " tenant types newer than last reload";
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << tenant_types_.size() << " tenant types";
    emit dataLoaded();
}

const iam::domain::tenant_type*
ClientTenantTypeModel::getType(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= tenant_types_.size())
        return nullptr;
    return &tenant_types_[idx];
}

QVariant ClientTenantTypeModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientTenantTypeModel::onPulseStateChanged(bool /*isOn*/) {
    if (!tenant_types_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientTenantTypeModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}

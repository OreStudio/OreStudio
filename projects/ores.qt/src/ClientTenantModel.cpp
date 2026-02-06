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
#include "ores.qt/ClientTenantModel.hpp"

#include <QtConcurrent>
#include <QPointer>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientTenantModel::ClientTenantModel(ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientTenantModel::onTenantsLoaded);
}

int ClientTenantModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(tenants_.size());
}

int ClientTenantModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return Column::ColumnCount;
}

QVariant ClientTenantModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= tenants_.size())
        return {};

    const auto& tenant = tenants_[row];

    if (role != Qt::DisplayRole)
        return {};

    switch (index.column()) {
    case Column::Code: return QString::fromStdString(tenant.code);
    case Column::Name: return QString::fromStdString(tenant.name);
    case Column::Type: return QString::fromStdString(tenant.type);
    case Column::Hostname: return QString::fromStdString(tenant.hostname);
    case Column::Status: return QString::fromStdString(tenant.status);
    case Column::Version: return tenant.version;
    case Column::RecordedBy: return QString::fromStdString(tenant.recorded_by);
    case Column::RecordedAt: return relative_time_helper::format(tenant.recorded_at);
    default: return {};
    }
}

QVariant ClientTenantModel::headerData(int section, Qt::Orientation orientation,
                                        int role) const {
    if (role != Qt::DisplayRole)
        return {};

    if (orientation == Qt::Horizontal) {
        switch (section) {
        case Column::Code: return tr("Code");
        case Column::Name: return tr("Name");
        case Column::Type: return tr("Type");
        case Column::Hostname: return tr("Hostname");
        case Column::Status: return tr("Status");
        case Column::Version: return tr("Version");
        case Column::RecordedBy: return tr("Recorded By");
        case Column::RecordedAt: return tr("Recorded At");
        default: return {};
        }
    }

    return {};
}

const iam::domain::tenant* ClientTenantModel::getTenant(int row) const {
    if (row < 0 || static_cast<std::size_t>(row) >= tenants_.size())
        return nullptr;
    return &tenants_[static_cast<std::size_t>(row)];
}

void ClientTenantModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh tenant model: disconnected.";
        return;
    }

    if (!tenants_.empty()) {
        beginResetModel();
        tenants_.clear();
        endResetModel();
    }

    fetch_tenants();
}

void ClientTenantModel::clear() {
    if (!tenants_.empty()) {
        beginResetModel();
        tenants_.clear();
        endResetModel();
    }
}

void ClientTenantModel::fetch_tenants() {
    is_fetching_ = true;
    QPointer<ClientTenantModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making a tenants request";

                if (!self || !self->clientManager_) {
                    return {.success = false, .tenants = {},
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                iam::messaging::get_tenants_request request;
                request.include_deleted = false;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch tenants: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .tenants = {},
                            .error_message = QString::fromStdString(
                                "Failed to fetch tenants: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Received " << result->tenants.size()
                                           << " tenants";

                return {.success = true, .tenants = std::move(result->tenants),
                        .error_message = {}, .error_details = {}};
            }, "tenants");
        });

    watcher_->setFuture(future);
}

void ClientTenantModel::onTenantsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On tenants loaded event.";
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch tenants: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    tenants_ = std::move(result.tenants);
    endResetModel();

    BOOST_LOG_SEV(lg(), info) << "Loaded " << tenants_.size() << " tenants";
    emit dataLoaded();
}

}

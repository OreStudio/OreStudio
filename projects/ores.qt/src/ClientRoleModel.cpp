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
#include "ores.qt/ClientRoleModel.hpp"

#include <algorithm>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientRoleModel::
ClientRoleModel(ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent), clientManager_(clientManager),
      watcher_(new QFutureWatcher<FutureWatcherResult>(this)) {

    connect(watcher_,
        &QFutureWatcher<FutureWatcherResult>::finished,
        this, &ClientRoleModel::onRolesLoaded);
}

int ClientRoleModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(roles_.size());
}

int ClientRoleModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return Column::ColumnCount;
}

QVariant ClientRoleModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= roles_.size())
        return {};

    const auto& r = roles_[row];

    if (role != Qt::DisplayRole)
        return {};

    switch (index.column()) {
    case Column::Name: return QString::fromStdString(r.name);
    case Column::Description: return QString::fromStdString(r.description);
    case Column::PermissionCount: return static_cast<int>(r.permission_codes.size());
    case Column::Version: return r.version;
    case Column::ModifiedBy: return QString::fromStdString(r.modified_by);
    case Column::RecordedAt: return relative_time_helper::format(r.recorded_at);
    default: return {};
    }
}

QVariant ClientRoleModel::
headerData(int section, Qt::Orientation orientation, int role) const {
    if (role != Qt::DisplayRole)
        return {};

    if (orientation == Qt::Horizontal) {
        switch (section) {
        case Column::Name: return tr("Name");
        case Column::Description: return tr("Description");
        case Column::PermissionCount: return tr("Permissions");
        case Column::Version: return tr("Version");
        case Column::ModifiedBy: return tr("Modified By");
        case Column::RecordedAt: return tr("Recorded At");
        default: return {};
        }
    }

    return {};
}

void ClientRoleModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh role model: disconnected.";
        return;
    }

    if (!roles_.empty()) {
        beginResetModel();
        roles_.clear();
        endResetModel();
    }

    is_fetching_ = true;
    QPointer<ClientRoleModel> self = this;

    QFuture<FutureWatcherResult> future =
        QtConcurrent::run([self]() -> FutureWatcherResult {
            return exception_helper::wrap_async_fetch<FutureWatcherResult>([&]() -> FutureWatcherResult {
                BOOST_LOG_SEV(lg(), debug) << "Making a roles request";
                if (!self || !self->clientManager_) {
                    return {.success = false, .roles = {},
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                iam::messaging::list_roles_request request;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch roles: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .roles = {},
                            .error_message = QString::fromStdString(
                                "Failed to fetch roles: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Received " << result->roles.size()
                                           << " roles";

                return {.success = true, .roles = std::move(result->roles),
                        .error_message = {}, .error_details = {}};
            }, "roles");
        });

     watcher_->setFuture(future);
}

void ClientRoleModel::onRolesLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On roles loaded event.";
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch roles: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    roles_ = std::move(result.roles);

    // Sort by name
    std::ranges::sort(roles_, [](auto const& a, auto const& b) {
        return a.name < b.name;
    });
    endResetModel();

    BOOST_LOG_SEV(lg(), info) << "Loaded " << roles_.size() << " roles";

    emit dataLoaded();
}

const iam::domain::role* ClientRoleModel::getRole(int row) const {
    if (row < 0 || row >= static_cast<int>(roles_.size()))
        return nullptr;

    return &roles_[row];
}

std::vector<iam::domain::role> ClientRoleModel::getRoles() const {
    return roles_;
}

}

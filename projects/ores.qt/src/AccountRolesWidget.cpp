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
#include "ores.qt/AccountRolesWidget.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QListWidgetItem>
#include <QInputDialog>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::utility::log;

AccountRolesWidget::AccountRolesWidget(QWidget* parent)
    : QWidget(parent),
      groupBox_(new QGroupBox("Assigned Roles", this)),
      rolesList_(new QListWidget(this)),
      assignButton_(new QToolButton(this)),
      revokeButton_(new QToolButton(this)),
      clientManager_(nullptr) {

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    auto* groupLayout = new QVBoxLayout(groupBox_);

    // Buttons layout
    auto* buttonsLayout = new QHBoxLayout();
    const QColor iconColor(220, 220, 220);

    assignButton_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_add_20_regular.svg", iconColor));
    assignButton_->setToolTip("Assign a role to this account");
    assignButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(assignButton_, &QToolButton::clicked, this,
        &AccountRolesWidget::onAssignRoleClicked);

    revokeButton_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    revokeButton_->setToolTip("Revoke selected role from this account");
    revokeButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(revokeButton_, &QToolButton::clicked, this,
        &AccountRolesWidget::onRevokeRoleClicked);

    buttonsLayout->addWidget(assignButton_);
    buttonsLayout->addWidget(revokeButton_);
    buttonsLayout->addStretch();

    rolesList_->setAlternatingRowColors(true);
    rolesList_->setSelectionMode(QAbstractItemView::SingleSelection);
    connect(rolesList_, &QListWidget::itemSelectionChanged,
        this, &AccountRolesWidget::onRoleSelectionChanged);

    groupLayout->addLayout(buttonsLayout);
    groupLayout->addWidget(rolesList_);

    mainLayout->addWidget(groupBox_);

    updateButtonStates();
}

void AccountRolesWidget::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AccountRolesWidget::setAccountId(const boost::uuids::uuid& accountId) {
    accountId_ = accountId;
}

void AccountRolesWidget::loadRoles() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load roles: not connected";
        return;
    }

    if (accountId_.is_nil()) {
        BOOST_LOG_SEV(lg(), debug) << "No account ID set, not loading roles";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading roles for account: "
                               << boost::uuids::to_string(accountId_);

    QPointer<AccountRolesWidget> self = this;
    const auto accountId = accountId_;

    struct LoadResult {
        bool success;
        std::vector<iam::domain::role> assignedRoles;
        std::vector<iam::domain::role> allRoles;
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
        [self, watcher]() {
            auto result = watcher->result();
            watcher->deleteLater();

            if (!self) return;

            if (result.success) {
                self->assignedRoles_ = std::move(result.assignedRoles);
                self->allRoles_ = std::move(result.allRoles);
                self->refreshRolesList();
                BOOST_LOG_SEV(lg(), debug) << "Loaded " << self->assignedRoles_.size()
                                           << " assigned roles";
            } else {
                emit self->errorMessage("Failed to load roles");
            }
        });

    QFuture<LoadResult> future = QtConcurrent::run([self, accountId]() -> LoadResult {
        if (!self) return {false, {}, {}};

        // Fetch assigned roles for this account
        iam::messaging::get_account_roles_request accountRolesRequest;
        accountRolesRequest.account_id = accountId;

        auto accountRolesResult = self->clientManager_->
            process_authenticated_request(std::move(accountRolesRequest));

        if (!accountRolesResult) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch account roles: "
                                       << comms::net::to_string(accountRolesResult.error());
            return {false, {}, {}};
        }

        // Fetch all roles for the assign dropdown
        iam::messaging::list_roles_request allRolesRequest;
        auto allRolesResult = self->clientManager_->
            process_authenticated_request(std::move(allRolesRequest));

        if (!allRolesResult) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch all roles: "
                                       << comms::net::to_string(allRolesResult.error());
            return {false, {}, {}};
        }

        return {true, std::move(accountRolesResult->roles),
                std::move(allRolesResult->roles)};
    });

    watcher->setFuture(future);
}

void AccountRolesWidget::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateButtonStates();
}

std::vector<boost::uuids::uuid> AccountRolesWidget::getAssignedRoleIds() const {
    std::vector<boost::uuids::uuid> ids;
    ids.reserve(assignedRoles_.size());
    for (const auto& role : assignedRoles_) {
        ids.push_back(role.id);
    }
    return ids;
}

void AccountRolesWidget::onAssignRoleClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Not Connected",
            "Cannot assign role while disconnected.");
        return;
    }

    // Build list of available roles (not already assigned)
    QStringList availableRoleNames;
    std::vector<iam::domain::role> availableRoles;

    for (const auto& role : allRoles_) {
        bool alreadyAssigned = std::ranges::any_of(assignedRoles_,
            [&role](const auto& ar) { return ar.id == role.id; });

        if (!alreadyAssigned) {
            availableRoleNames << QString::fromStdString(role.name);
            availableRoles.push_back(role);
        }
    }

    if (availableRoleNames.isEmpty()) {
        MessageBoxHelper::information(this, "No Roles Available",
            "All available roles are already assigned to this account.");
        return;
    }

    bool ok;
    QString selectedName = QInputDialog::getItem(this, "Assign Role",
        "Select a role to assign:", availableRoleNames, 0, false, &ok);

    if (!ok || selectedName.isEmpty()) {
        return;
    }

    // Find the selected role
    auto it = std::ranges::find_if(availableRoles,
        [&selectedName](const auto& r) {
            return QString::fromStdString(r.name) == selectedName;
        });

    if (it == availableRoles.end()) {
        return;
    }

    const auto roleId = it->id;
    const auto roleName = it->name;

    BOOST_LOG_SEV(lg(), info) << "Assigning role " << roleName
                              << " to account " << boost::uuids::to_string(accountId_);

    QPointer<AccountRolesWidget> self = this;
    const auto accountId = accountId_;

    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished, this,
        [self, watcher, roleName]() {
            auto [success, message] = watcher->result();
            watcher->deleteLater();

            if (!self) return;

            if (success) {
                emit self->statusMessage(QString("Role '%1' assigned successfully")
                    .arg(QString::fromStdString(roleName)));
                self->loadRoles();  // Refresh the list
                emit self->rolesChanged();
            } else {
                emit self->errorMessage(QString("Failed to assign role: %1")
                    .arg(QString::fromStdString(message)));
                MessageBoxHelper::critical(self, "Assign Role Failed",
                    QString::fromStdString(message));
            }
        });

    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([self, accountId, roleId]() -> std::pair<bool, std::string> {
            if (!self) return {false, "Widget destroyed"};

            iam::messaging::assign_role_request request;
            request.account_id = accountId;
            request.role_id = roleId;

            auto result = self->clientManager_->
                process_authenticated_request(std::move(request));

            if (!result) {
                return {false, comms::net::to_string(result.error())};
            }

            return {result->success, result->error_message};
        });

    watcher->setFuture(future);
}

void AccountRolesWidget::onRevokeRoleClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Not Connected",
            "Cannot revoke role while disconnected.");
        return;
    }

    auto selected = rolesList_->selectedItems();
    if (selected.isEmpty()) {
        return;
    }

    const int row = rolesList_->row(selected.first());
    if (row < 0 || row >= static_cast<int>(assignedRoles_.size())) {
        return;
    }

    const auto& role = assignedRoles_[row];
    const auto roleId = role.id;
    const auto roleName = role.name;

    auto reply = MessageBoxHelper::question(this, "Revoke Role",
        QString("Are you sure you want to revoke role '%1' from this account?")
            .arg(QString::fromStdString(roleName)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Revoking role " << roleName
                              << " from account " << boost::uuids::to_string(accountId_);

    QPointer<AccountRolesWidget> self = this;
    const auto accountId = accountId_;

    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished, this,
        [self, watcher, roleName]() {
            auto [success, message] = watcher->result();
            watcher->deleteLater();

            if (!self) return;

            if (success) {
                emit self->statusMessage(QString("Role '%1' revoked successfully")
                    .arg(QString::fromStdString(roleName)));
                self->loadRoles();  // Refresh the list
                emit self->rolesChanged();
            } else {
                emit self->errorMessage(QString("Failed to revoke role: %1")
                    .arg(QString::fromStdString(message)));
                MessageBoxHelper::critical(self, "Revoke Role Failed",
                    QString::fromStdString(message));
            }
        });

    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([self, accountId, roleId]() -> std::pair<bool, std::string> {
            if (!self) return {false, "Widget destroyed"};

            iam::messaging::revoke_role_request request;
            request.account_id = accountId;
            request.role_id = roleId;

            auto result = self->clientManager_->
                process_authenticated_request(std::move(request));

            if (!result) {
                return {false, comms::net::to_string(result.error())};
            }

            return {result->success, result->error_message};
        });

    watcher->setFuture(future);
}

void AccountRolesWidget::onRoleSelectionChanged() {
    updateButtonStates();
}

void AccountRolesWidget::updateButtonStates() {
    const bool hasSelection = !rolesList_->selectedItems().isEmpty();
    const bool isConnected = clientManager_ && clientManager_->isConnected();

    assignButton_->setEnabled(!isReadOnly_ && isConnected);
    revokeButton_->setEnabled(!isReadOnly_ && isConnected && hasSelection);
}

void AccountRolesWidget::refreshRolesList() {
    rolesList_->clear();

    // Sort roles by name
    std::vector<iam::domain::role> sortedRoles = assignedRoles_;
    std::ranges::sort(sortedRoles, [](const auto& a, const auto& b) {
        return a.name < b.name;
    });

    for (const auto& role : sortedRoles) {
        auto* item = new QListWidgetItem(QString::fromStdString(role.name));
        item->setToolTip(QString::fromStdString(role.description));
        rolesList_->addItem(item);
    }

    // Update group box title with count
    groupBox_->setTitle(QString("Assigned Roles (%1)").arg(assignedRoles_.size()));

    updateButtonStates();
}

}

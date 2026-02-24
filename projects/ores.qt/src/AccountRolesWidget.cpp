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
#include <QtConcurrent>
#include <QFutureWatcher>
#include <algorithm>
#include <future>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::logging;

AccountRolesWidget::AccountRolesWidget(QWidget* parent)
    : QWidget(parent),
      groupBox_(new QGroupBox("Assigned Roles", this)),
      rolesList_(new QListWidget(this)),
      roleCombo_(new QComboBox(this)),
      assignButton_(new QToolButton(this)),
      revokeButton_(new QToolButton(this)) {

    setupUi();
    updateButtonStates();
}

void AccountRolesWidget::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    auto* groupLayout = new QVBoxLayout(groupBox_);

    rolesList_->setAlternatingRowColors(true);
    rolesList_->setSelectionMode(QAbstractItemView::SingleSelection);
    connect(rolesList_, &QListWidget::itemSelectionChanged,
        this, &AccountRolesWidget::onRoleSelectionChanged);

    groupLayout->addWidget(rolesList_);

    auto* buttonsLayout = new QHBoxLayout();

    assignButton_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Add, IconUtils::DefaultIconColor));
    assignButton_->setToolTip("Assign selected role");
    assignButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(assignButton_, &QToolButton::clicked,
        this, &AccountRolesWidget::onAssignRoleClicked);

    revokeButton_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Delete, IconUtils::DefaultIconColor));
    revokeButton_->setToolTip("Revoke selected role");
    revokeButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(revokeButton_, &QToolButton::clicked,
        this, &AccountRolesWidget::onRevokeRoleClicked);

    buttonsLayout->addWidget(roleCombo_);
    buttonsLayout->addWidget(assignButton_);
    buttonsLayout->addWidget(revokeButton_);
    buttonsLayout->addStretch();

    groupLayout->addLayout(buttonsLayout);

    mainLayout->addWidget(groupBox_);
}

void AccountRolesWidget::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AccountRolesWidget::setAccountId(const boost::uuids::uuid& accountId) {
    accountId_ = accountId;
}

bool AccountRolesWidget::hasPendingChanges() const {
    return !pendingAdds_.empty() || !pendingRemoves_.empty();
}

const std::vector<boost::uuids::uuid>& AccountRolesWidget::pendingAdds() const {
    return pendingAdds_;
}

const std::vector<boost::uuids::uuid>& AccountRolesWidget::pendingRemoves() const {
    return pendingRemoves_;
}

void AccountRolesWidget::load() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load roles: not connected";
        return;
    }

    const bool has_account = !accountId_.is_nil();
    if (has_account) {
        BOOST_LOG_SEV(lg(), debug) << "Loading roles for account: "
                                   << boost::uuids::to_string(accountId_);
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Loading available roles for new account";
    }

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
                self->allRoles_      = std::move(result.allRoles);
                self->pendingAdds_.clear();
                self->pendingRemoves_.clear();
                self->refreshRolesList();
                BOOST_LOG_SEV(lg(), debug) << "Loaded " << self->assignedRoles_.size()
                                           << " assigned, " << self->allRoles_.size()
                                           << " total roles";
            } else {
                emit self->errorMessage("Failed to load roles");
            }
        });

    QFuture<LoadResult> future =
        QtConcurrent::run([self, accountId, has_account]() -> LoadResult {
            if (!self) return {.success = false};

            if (has_account) {
                auto accountRolesFuture = std::async(std::launch::async,
                    [&self, &accountId]() {
                        iam::messaging::get_account_roles_request request;
                        request.account_id = accountId;
                        return self->clientManager_->
                            process_authenticated_request(std::move(request));
                    });

                auto allRolesFuture = std::async(std::launch::async,
                    [&self]() {
                        iam::messaging::list_roles_request request;
                        return self->clientManager_->
                            process_authenticated_request(std::move(request));
                    });

                auto accountRolesResult = accountRolesFuture.get();
                auto allRolesResult     = allRolesFuture.get();

                if (!accountRolesResult) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch account roles: "
                                               << comms::net::to_string(accountRolesResult.error());
                    return {.success = false};
                }
                if (!allRolesResult) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch all roles: "
                                               << comms::net::to_string(allRolesResult.error());
                    return {.success = false};
                }
                return {.success = true,
                    .assignedRoles = std::move(accountRolesResult->roles),
                    .allRoles      = std::move(allRolesResult->roles)};
            }

            // Create mode: only fetch all roles
            iam::messaging::list_roles_request request;
            auto result = self->clientManager_->
                process_authenticated_request(std::move(request));

            if (!result) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to fetch roles: "
                    << comms::net::to_string(result.error());
                return {.success = false};
            }
            return {.success = true, .allRoles = std::move(result->roles)};
        });

    watcher->setFuture(future);
}

void AccountRolesWidget::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateButtonStates();
}

void AccountRolesWidget::onAssignRoleClicked() {
    if (roleCombo_->count() == 0) return;

    const auto roleIdStr = roleCombo_->currentData().toString().toStdString();
    if (roleIdStr.empty()) return;

    const auto roleId = boost::lexical_cast<boost::uuids::uuid>(roleIdStr);

    // If staged for removal, un-stage the removal instead
    auto it = std::find(pendingRemoves_.begin(), pendingRemoves_.end(), roleId);
    if (it != pendingRemoves_.end()) {
        pendingRemoves_.erase(it);
    } else {
        pendingAdds_.push_back(roleId);
    }

    refreshRolesList();
    updateButtonStates();
    emit roleListChanged();
}

void AccountRolesWidget::onRevokeRoleClicked() {
    auto selected = rolesList_->selectedItems();
    if (selected.isEmpty()) return;

    const auto roleIdStr =
        selected.first()->data(Qt::UserRole).toString().toStdString();
    if (roleIdStr.empty()) return;

    const auto roleId = boost::lexical_cast<boost::uuids::uuid>(roleIdStr);

    // If staged for addition, un-stage the addition instead
    auto it = std::find(pendingAdds_.begin(), pendingAdds_.end(), roleId);
    if (it != pendingAdds_.end()) {
        pendingAdds_.erase(it);
    } else {
        pendingRemoves_.push_back(roleId);
    }

    refreshRolesList();
    updateButtonStates();
    emit roleListChanged();
}

void AccountRolesWidget::onRoleSelectionChanged() {
    updateButtonStates();
}

void AccountRolesWidget::refreshRolesList() {
    rolesList_->clear();

    auto isRemoved = [&](const boost::uuids::uuid& id) {
        return std::find(pendingRemoves_.begin(), pendingRemoves_.end(), id)
            != pendingRemoves_.end();
    };

    auto findRoleName = [&](const boost::uuids::uuid& id) -> QString {
        auto it = std::ranges::find_if(allRoles_,
            [&id](const auto& r) { return r.id == id; });
        if (it != allRoles_.end())
            return QString::fromStdString(it->name);
        return QString::fromStdString(boost::uuids::to_string(id));
    };

    auto findRoleDescription = [&](const boost::uuids::uuid& id) -> QString {
        auto it = std::ranges::find_if(allRoles_,
            [&id](const auto& r) { return r.id == id; });
        if (it != allRoles_.end())
            return QString::fromStdString(it->description);
        return {};
    };

    std::vector<boost::uuids::uuid> effectiveIds;

    // Assigned roles not pending removal
    for (const auto& role : assignedRoles_) {
        if (!isRemoved(role.id)) {
            effectiveIds.push_back(role.id);
            auto* item = new QListWidgetItem(findRoleName(role.id));
            item->setData(Qt::UserRole,
                QString::fromStdString(boost::uuids::to_string(role.id)));
            item->setToolTip(findRoleDescription(role.id));
            rolesList_->addItem(item);
        }
    }

    // Staged additions
    for (const auto& roleId : pendingAdds_) {
        effectiveIds.push_back(roleId);
        auto* item = new QListWidgetItem(findRoleName(roleId));
        item->setData(Qt::UserRole,
            QString::fromStdString(boost::uuids::to_string(roleId)));
        item->setToolTip(findRoleDescription(roleId));
        rolesList_->addItem(item);
    }

    // Populate combo with roles not in effective set
    roleCombo_->clear();
    for (const auto& role : allRoles_) {
        bool inEffective = std::ranges::any_of(effectiveIds,
            [&role](const auto& id) { return id == role.id; });
        if (!inEffective) {
            roleCombo_->addItem(QString::fromStdString(role.name),
                QVariant(QString::fromStdString(
                    boost::uuids::to_string(role.id))));
        }
    }

    groupBox_->setTitle(
        QString("Assigned Roles (%1)").arg(static_cast<int>(effectiveIds.size())));

    updateButtonStates();
}

void AccountRolesWidget::updateButtonStates() {
    const bool hasSelection = !rolesList_->selectedItems().isEmpty();
    const bool isConnected  = clientManager_ && clientManager_->isConnected();
    const bool hasComboItem = roleCombo_->count() > 0;

    assignButton_->setEnabled(!isReadOnly_ && isConnected && hasComboItem);
    revokeButton_->setEnabled(!isReadOnly_ && isConnected && hasSelection);
}

}

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
#include "ores.qt/RoleController.hpp"

#include <QPointer>
#include "ores.qt/RoleMdiWindow.hpp"
#include "ores.qt/RoleDetailDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.iam/eventing/role_assigned_event.hpp"

namespace ores::qt {

using namespace ores::utility::log;

namespace {
    // Event type names for role changes
    constexpr std::string_view role_assigned_event_name =
        eventing::domain::event_traits<iam::eventing::role_assigned_event>::name;
}

RoleController::RoleController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QList<DetachableMdiSubWindow*>& allDetachableWindows,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      allDetachableWindows_(allDetachableWindows),
      roleListWindow_(nullptr) {
    BOOST_LOG_SEV(lg(), debug) << "Role controller created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &RoleController::onNotificationReceived);

        // Subscribe to events when connected
        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to role change events";
            clientManager_->subscribeToEvent(std::string{role_assigned_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to role change events after reconnect";
            clientManager_->subscribeToEvent(std::string{role_assigned_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to role change events";
            clientManager_->subscribeToEvent(std::string{role_assigned_event_name});
        }
    }
}

RoleController::~RoleController() {
    BOOST_LOG_SEV(lg(), debug) << "Role controller destroyed";

    // Unsubscribe from role change events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from role change events";
        clientManager_->unsubscribeFromEvent(std::string{role_assigned_event_name});
    }
}

void RoleController::showListWindow() {
    // Reuse existing window if it exists
    if (roleListWindow_) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing roles window";

        // Bring window to front
        if (roleListWindow_->isDetached()) {
            roleListWindow_->setVisible(true);
            roleListWindow_->show();
            roleListWindow_->raise();
            roleListWindow_->activateWindow();
        } else {
            roleListWindow_->setVisible(true);
            mdiArea_->setActiveSubWindow(roleListWindow_);
            roleListWindow_->show();
            roleListWindow_->raise();
        }
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new roles MDI window";
    const QColor iconColor(220, 220, 220);

    auto* roleWidget = new RoleMdiWindow(clientManager_, username_, mainWindow_);

    // Connect status signals
    connect(roleWidget, &RoleMdiWindow::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(roleWidget, &RoleMdiWindow::errorOccurred,
            this, [this](const QString& err_msg) {
        emit errorMessage("Error: " + err_msg);
    });

    // Connect role operations
    connect(roleWidget, &RoleMdiWindow::showRoleDetails,
            this, &RoleController::onShowRoleDetails);

    roleListWindow_ = new DetachableMdiSubWindow();
    roleListWindow_->setAttribute(Qt::WA_DeleteOnClose);
    roleListWindow_->setWidget(roleWidget);
    roleListWindow_->setWindowTitle("Roles");
    roleListWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_lock_closed_20_regular.svg", iconColor));

    // Track window for detach/reattach operations
    allDetachableWindows_.append(roleListWindow_);
    QPointer<RoleController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = roleListWindow_;
    connect(roleListWindow_, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self)
            return;

        if (!windowBeingDestroyed.isNull()) {
            self->allDetachableWindows_.removeAll(windowBeingDestroyed.data());
        }

        if (self->roleListWindow_ == windowBeingDestroyed)
            self->roleListWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(roleListWindow_);
    roleListWindow_->adjustSize();
    roleListWindow_->show();
}

void RoleController::closeAllWindows() {
    if (roleListWindow_) {
        roleListWindow_->close();
    }
}

void RoleController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp) {
    // Check if this is a role-related event
    if (eventType != QString::fromStdString(std::string{role_assigned_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received role change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString();

    markRoleListAsStale();
}

void RoleController::markRoleListAsStale() {
    if (roleListWindow_) {
        auto* roleWidget = qobject_cast<RoleMdiWindow*>(
            roleListWindow_->widget());
        if (roleWidget) {
            roleWidget->markAsStale();
            BOOST_LOG_SEV(lg(), debug) << "Marked role window as stale";
        }
    }
}

void RoleController::onShowRoleDetails(const iam::domain::role& role) {
    BOOST_LOG_SEV(lg(), info) << "Showing role details for: " << role.name;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new RoleDetailDialog(mainWindow_);
    detailDialog->setRole(role);

    // Connect common signals
    connect(detailDialog, &RoleDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &RoleDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    // Create and configure window
    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Role: %1")
        .arg(QString::fromStdString(role.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_lock_closed_20_regular.svg", iconColor));

    // Track window for cleanup
    allDetachableWindows_.append(detailWindow);
    QPointer<RoleController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = detailWindow;
    connect(detailWindow, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self) return;
        if (!windowBeingDestroyed.isNull()) {
            self->allDetachableWindows_.removeAll(windowBeingDestroyed.data());
        }
    });

    mdiArea_->addSubWindow(detailWindow);
    detailWindow->adjustSize();
    detailWindow->show();
}

}

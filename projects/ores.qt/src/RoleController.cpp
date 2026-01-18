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

namespace ores::qt {

using namespace ores::logging;

RoleController::RoleController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      roleListWindow_(nullptr) {
    BOOST_LOG_SEV(lg(), debug) << "Role controller created";
}

RoleController::~RoleController() {
    BOOST_LOG_SEV(lg(), debug) << "Role controller destroyed";
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
    register_detachable_window(roleListWindow_);
    QPointer<RoleController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = roleListWindow_;
    connect(roleListWindow_, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self)
            return;

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

void RoleController::reloadListWindow() {
    if (roleListWindow_) {
        if (auto* widget = qobject_cast<RoleMdiWindow*>(roleListWindow_->widget())) {
            widget->reload();
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
    register_detachable_window(detailWindow);

    mdiArea_->addSubWindow(detailWindow);
    detailWindow->adjustSize();
    detailWindow->show();
}

}

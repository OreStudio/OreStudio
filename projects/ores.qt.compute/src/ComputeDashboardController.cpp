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
#include "ores.qt/ComputeDashboardController.hpp"

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ComputeDashboardMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

ComputeDashboardController::ComputeDashboardController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    QObject* parent)
    : QObject(parent),
      mainWindow_(mainWindow),
      mdiArea_(mdiArea),
      clientManager_(clientManager),
      dashboardWindow_(nullptr),
      dashboardSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ComputeDashboardController created";
}

void ComputeDashboardController::showDashboard() {
    // Reuse existing window if still open
    if (dashboardSubWindow_) {
        mdiArea_->setActiveSubWindow(dashboardSubWindow_);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Opening compute dashboard";

    dashboardWindow_ = new ComputeDashboardMdiWindow(clientManager_);

    connect(dashboardWindow_, &ComputeDashboardMdiWindow::statusChanged,
            this, [self = QPointer<ComputeDashboardController>(this)](const QString& msg) {
        if (!self) return;
        emit self->statusMessage(msg);
    });
    connect(dashboardWindow_, &ComputeDashboardMdiWindow::errorOccurred,
            this, [self = QPointer<ComputeDashboardController>(this)](const QString& err) {
        if (!self) return;
        emit self->errorMessage(err);
    });

    dashboardSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    dashboardSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    dashboardSubWindow_->setWidget(dashboardWindow_);
    dashboardSubWindow_->setWindowTitle(tr("Compute Dashboard"));
    dashboardSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));

    connect(dashboardSubWindow_, &QObject::destroyed, this,
            [self = QPointer<ComputeDashboardController>(this)]() {
        if (!self) return;
        self->dashboardWindow_ = nullptr;
        self->dashboardSubWindow_ = nullptr;
    });

    emit detachableWindowCreated(dashboardSubWindow_);

    mdiArea_->addSubWindow(dashboardSubWindow_);
    dashboardSubWindow_->adjustSize();
    dashboardSubWindow_->show();
}

void ComputeDashboardController::closeAllWindows() {
    if (dashboardSubWindow_) {
        dashboardSubWindow_->close();
        dashboardSubWindow_ = nullptr;
        dashboardWindow_ = nullptr;
    }
}

}

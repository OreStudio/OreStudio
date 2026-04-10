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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/WorkflowController.hpp"

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WorkflowMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkflowController::WorkflowController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    QObject* parent)
    : QObject(parent),
      mainWindow_(mainWindow),
      mdiArea_(mdiArea),
      clientManager_(clientManager),
      listWindow_(nullptr),
      listSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "WorkflowController created";
}

void WorkflowController::showListWindow() {
    if (listSubWindow_) {
        mdiArea_->setActiveSubWindow(listSubWindow_);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Opening workflow execution list";

    listWindow_ = new WorkflowMdiWindow(clientManager_);

    connect(listWindow_, &WorkflowMdiWindow::statusChanged,
            this, [self = QPointer<WorkflowController>(this)](const QString& msg) {
        if (!self) return;
        emit self->statusMessage(msg);
    });
    connect(listWindow_, &WorkflowMdiWindow::errorOccurred,
            this, [self = QPointer<WorkflowController>(this)](const QString& err) {
        if (!self) return;
        emit self->errorMessage(err);
    });

    listSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listSubWindow_->setWidget(listWindow_);
    listSubWindow_->setWindowTitle(tr("Workflows"));
    listSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::TasksApp, IconUtils::DefaultIconColor));

    connect(listSubWindow_, &QObject::destroyed, this,
            [self = QPointer<WorkflowController>(this)]() {
        if (!self) return;
        self->listWindow_ = nullptr;
        self->listSubWindow_ = nullptr;
    });

    emit detachableWindowCreated(listSubWindow_);

    mdiArea_->addSubWindow(listSubWindow_);
    listSubWindow_->adjustSize();
    listSubWindow_->show();
}

void WorkflowController::closeAllWindows() {
    if (listSubWindow_) {
        listSubWindow_->close();
        listSubWindow_ = nullptr;
        listWindow_ = nullptr;
    }
}

} // namespace ores::qt

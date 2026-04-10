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
#include "ores.qt/JobInstanceController.hpp"

#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/JobInstanceMdiWindow.hpp"
#include "ores.qt/JobInstanceDetailDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

JobInstanceController::JobInstanceController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, {},
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "JobInstanceController created";
}

void JobInstanceController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow";

    const QString key = build_window_key("list", "job_instances");
    if (try_reuse_window(key)) return;

    listWindow_ = new JobInstanceMdiWindow(clientManager_);

    connect(listWindow_, &JobInstanceMdiWindow::statusChanged,
            this, &JobInstanceController::statusMessage);
    connect(listWindow_, &JobInstanceMdiWindow::errorOccurred,
            this, &JobInstanceController::errorMessage);
    connect(listWindow_, &JobInstanceMdiWindow::showInstanceDetails,
            this, &JobInstanceController::onShowDetails);

    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Job Instances");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::TasksApp, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed,
            this, [self = QPointer<JobInstanceController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });
}

void JobInstanceController::closeAllWindows() {
    QList<QString> keys = managed_windows_.keys();
    for (const QString& k : keys)
        if (auto* w = managed_windows_.value(k)) w->close();
    managed_windows_.clear();
    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void JobInstanceController::reloadListWindow() {
    if (listWindow_) listWindow_->reload();
}

EntityListMdiWindow* JobInstanceController::listWindow() const {
    return listWindow_;
}

void JobInstanceController::onShowDetails(
    const scheduler::messaging::job_instance_summary& instance) {
    showDetailWindow(instance);
}

void JobInstanceController::showDetailWindow(
    const scheduler::messaging::job_instance_summary& instance) {
    auto* dlg = new JobInstanceDetailDialog(instance, mainWindow_);
    dlg->exec();
}

}

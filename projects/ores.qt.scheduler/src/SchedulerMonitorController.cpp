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
#include "ores.qt/SchedulerMonitorController.hpp"

#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/SchedulerMonitorMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

SchedulerMonitorController::SchedulerMonitorController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    QObject* parent)
    : QObject(parent),
      mainWindow_(mainWindow),
      mdiArea_(mdiArea),
      clientManager_(clientManager) {

    connect(clientManager_, &ClientManager::notificationReceived,
        this, [this](const QString& eventType, const QDateTime&,
                     const QStringList&, const QString&) {
            if (eventType == QString::fromUtf8(event_subject.data(),
                    static_cast<int>(event_subject.size())) && window_) {
                window_->refresh();
            }
        });

    connect(clientManager_, &ClientManager::loggedIn, this, [this]() {
        clientManager_->subscribeToEvent(std::string(event_subject));
    });
    connect(clientManager_, &ClientManager::reconnected, this, [this]() {
        clientManager_->subscribeToEvent(std::string(event_subject));
    });
    if (clientManager_->isConnected())
        clientManager_->subscribeToEvent(std::string(event_subject));
}

SchedulerMonitorController::~SchedulerMonitorController() {
    if (clientManager_)
        clientManager_->unsubscribeFromEvent(std::string(event_subject));
}

void SchedulerMonitorController::showWindow() {
    // Singleton: if the window already exists, bring it to front.
    if (mdiSubWindow_) {
        mdiArea_->setActiveSubWindow(mdiSubWindow_);
        mdiSubWindow_->showNormal();
        return;
    }

    window_ = new SchedulerMonitorMdiWindow(clientManager_);

    connect(window_, &SchedulerMonitorMdiWindow::statusChanged,
            this, &SchedulerMonitorController::statusMessage);
    connect(window_, &SchedulerMonitorMdiWindow::errorOccurred,
            this, &SchedulerMonitorController::errorMessage);

    mdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    mdiSubWindow_->setWidget(window_);
    mdiSubWindow_->setWindowTitle(tr("Scheduler Monitor"));
    mdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Clock, IconUtils::DefaultIconColor));
    mdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    mdiSubWindow_->resize(window_->sizeHint());

    mdiArea_->addSubWindow(mdiSubWindow_);
    mdiSubWindow_->show();

    connect(mdiSubWindow_, &QObject::destroyed,
            this, [self = QPointer<SchedulerMonitorController>(this)]() {
        if (!self) return;
        self->window_ = nullptr;
        self->mdiSubWindow_ = nullptr;
    });
}

void SchedulerMonitorController::closeWindow() {
    if (mdiSubWindow_) {
        mdiSubWindow_->close();
        mdiSubWindow_ = nullptr;
        window_ = nullptr;
    }
}

}

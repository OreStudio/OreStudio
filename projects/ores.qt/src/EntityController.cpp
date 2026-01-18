/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/EntityController.hpp"

#include <QPointer>
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

EntityController::EntityController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : QObject(parent),
      mainWindow_(mainWindow),
      mdiArea_(mdiArea),
      clientManager_(clientManager),
      username_(username) {
}

void EntityController::setClientManager(ClientManager* clientManager,
    const QString& username) {
    clientManager_ = clientManager;
    username_ = username;
}

QString EntityController::build_window_key(const QString& windowType,
    const QString& identifier) const {
    return QString("%1.%2").arg(windowType, identifier);
}

bool EntityController::try_reuse_window(const QString& key) {
    if (managed_windows_.contains(key)) {
        auto existing = managed_windows_[key];
        if (existing) {
            bring_window_to_front(existing);
            return true;
        }
    }
    return false;
}

void EntityController::bring_window_to_front(DetachableMdiSubWindow* window) {
    if (window->isDetached()) {
        window->setVisible(true);
        window->show();
        window->raise();
        window->activateWindow();
    } else {
        window->setVisible(true);
        mdiArea_->setActiveSubWindow(window);
        window->show();
        window->raise();
    }
}

void EntityController::track_window(const QString& key,
    DetachableMdiSubWindow* window) {
    managed_windows_[key] = window;
}

void EntityController::untrack_window(const QString& key) {
    managed_windows_.remove(key);
}

void EntityController::show_managed_window(DetachableMdiSubWindow* window,
    DetachableMdiSubWindow* referenceWindow, QPoint offset) {
    mdiArea_->addSubWindow(window);
    window->setWindowFlags(window->windowFlags() & ~Qt::WindowMaximizeButtonHint);
    window->adjustSize();

    if (referenceWindow && referenceWindow->isDetached()) {
        window->show();
        window->detach();
        QPoint parentPos = referenceWindow->pos();
        window->move(parentPos.x() + offset.x(), parentPos.y() + offset.y());
    } else {
        window->show();
    }
}

void EntityController::connect_dialog_close(DetailDialogBase* dialog,
    DetachableMdiSubWindow* window) {
    connect(dialog, &DetailDialogBase::closeRequested, window, &QWidget::close);
}

void EntityController::register_detachable_window(DetachableMdiSubWindow* window) {
    emit detachableWindowCreated(window);

    QPointer<EntityController> self = this;
    QPointer<DetachableMdiSubWindow> windowPtr = window;
    connect(window, &QObject::destroyed, this, [self, windowPtr]() {
        if (self && windowPtr) {
            emit self->detachableWindowDestroyed(windowPtr.data());
        }
    });
}

void EntityController::handleEntitySaved() {
    if (autoReloadOnSave_) {
        reloadListWindow();
    }
}

void EntityController::handleEntityDeleted() {
    if (autoReloadOnSave_) {
        reloadListWindow();
    }
}

}

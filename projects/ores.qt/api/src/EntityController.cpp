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
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.qt/WorkspaceContext.hpp"
#include <QDynamicPropertyChangeEvent>
#include <QEvent>
#include <QPointer>
#include <QVariant>

namespace ores::qt {

using namespace ores::logging;

EntityController::EntityController(QMainWindow* mainWindow,
                                   QMdiArea* mdiArea,
                                   ClientManager* clientManager,
                                   const QString& username,
                                   std::string_view eventName,
                                   QObject* parent)
    : QObject(parent)
    , mainWindow_(mainWindow)
    , mdiArea_(mdiArea)
    , clientManager_(clientManager)
    , username_(username)
    , eventName_(eventName) {

    if (!eventName_.empty()) {
        setupEventSubscription();
    }

    // Watch the MDI area for workspace context changes and forward them to
    // the client manager so every NATS request carries the right X-Workspace-Id.
    mdiArea_->installEventFilter(this);
    const auto wvar = mdiArea_->property("ores_workspace_context");
    if (wvar.isValid()) {
        setWorkspaceContext(wvar.value<WorkspaceContext>());
    }
}

EntityController::~EntityController() {
    if (!eventName_.empty()) {
        teardownEventSubscription();
    }
}

void EntityController::setupEventSubscription() {
    if (!clientManager_ || eventName_.empty()) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Setting up event subscription for: " << eventName_;

    connect(clientManager_,
            &ClientManager::notificationReceived,
            this,
            &EntityController::onNotificationReceived);

    connect(clientManager_,
            &ClientManager::loggedIn,
            this,
            [self = QPointer<EntityController>(this)]() {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Subscribing to " << self->eventName_ << " events";
                self->clientManager_->subscribeToEvent(self->eventName_);
            });

    connect(clientManager_,
            &ClientManager::reconnected,
            this,
            [self = QPointer<EntityController>(this)]() {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Re-subscribing to " << self->eventName_ << " events";
                self->clientManager_->subscribeToEvent(self->eventName_);
            });

    if (clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to " << eventName_
                                  << " events";
        clientManager_->subscribeToEvent(eventName_);
    }
}

void EntityController::teardownEventSubscription() {
    if (!clientManager_ || eventName_.empty()) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from " << eventName_ << " events";
    clientManager_->unsubscribeFromEvent(eventName_);
}

void EntityController::onNotificationReceived(const QString& eventType,
                                              const QDateTime& timestamp,
                                              const QStringList& entityIds,
                                              const QString& tenantId) {

    if (eventType != QString::fromStdString(eventName_)) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received " << eventName_ << " notification at "
                              << timestamp.toString(Qt::ISODate).toStdString() << " with "
                              << entityIds.size() << " entities"
                              << ", tenant: " << tenantId.toStdString();

    if (auto* window = listWindow(); window != nullptr) {
        window->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked list window as stale";
    }

    notifyOpenDialogs(entityIds);
}

void EntityController::setClientManager(ClientManager* clientManager, const QString& username) {
    clientManager_ = clientManager;
    username_ = username;
}

void EntityController::setWorkspaceContext(const WorkspaceContext& ctx) {
    if (clientManager_) {
        clientManager_->setWorkspaceContext(ctx);
    }
}

bool EntityController::eventFilter(QObject* watched, QEvent* event) {
    if (watched == mdiArea_ && event->type() == QEvent::DynamicPropertyChange) {
        const auto* pe = static_cast<QDynamicPropertyChangeEvent*>(event);
        if (pe->propertyName() == "ores_workspace_context") {
            const auto wvar = mdiArea_->property("ores_workspace_context");
            if (wvar.isValid()) {
                setWorkspaceContext(wvar.value<WorkspaceContext>());
            }
        }
    }
    return QObject::eventFilter(watched, event);
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

void EntityController::track_window(const QString& key, DetachableMdiSubWindow* window) {
    managed_windows_[key] = window;
}

void EntityController::untrack_window(const QString& key) {
    managed_windows_.remove(key);
}

void EntityController::show_managed_window(DetachableMdiSubWindow* window,
                                           DetachableMdiSubWindow* referenceWindow,
                                           QPoint offset) {
    // Set flags before addSubWindow so the MDI area decorates the subwindow
    // exactly once with the correct flags.  Setting flags after addSubWindow
    // triggers a re-decoration that leaves a blank black strip above the content.
    window->setWindowFlags(window->windowFlags() & ~Qt::WindowMaximizeButtonHint);
    mdiArea_->addSubWindow(window);

    auto wvar = mdiArea_->property("ores_workspace_context");
    if (wvar.isValid()) {
        const auto wctx = wvar.value<WorkspaceContext>();
        if (!wctx.is_live()) {
            window->setWindowTitle(window->windowTitle() + " [" + wctx.name + "]");
        }
    }

    if (referenceWindow && referenceWindow->isDetached()) {
        window->show();
        window->detach();
        QPoint parentPos = referenceWindow->pos();
        window->move(parentPos.x() + offset.x(), parentPos.y() + offset.y());
    } else {
        window->show();
        // Restore saved geometry AFTER addSubWindow()+show(): the MDI area
        // positions a subwindow when it is added, so restoring earlier (in the
        // caller, before this point) is clobbered. Keyed off the window's
        // geometryKey so every managed window persists size/position uniformly.
        bool restored = false;
        if (!window->geometryKey().isEmpty())
            restored = UiPersistence::restoreMdiGeometry(window->geometryKey(), window);
        // No saved geometry (first-ever open, or it was cleared): size to the
        // content's own sizeHint() rather than whatever size the .ui's static
        // minimumSize happened to specify. sizeHint() is computed from the
        // *live* widget tree, so it reflects anything added at runtime after
        // setupUi() (e.g. a version-nav toolbar) that a static XML size can't
        // know about. Cheap here — a handful of widgets in a QFormLayout, not
        // the QHeaderView::ResizeToContents per-cell cost that caused the
        // 24-second list-window freeze (sprint 19); see that story before
        // reaching for ResizeToContents-style per-row/per-cell measurement.
        if (!restored) {
            if (auto* widget = window->widget())
                window->resize(widget->sizeHint().expandedTo(window->size()));
        }
    }

    if (referenceWindow) {
        QPointer<EntityController> self = this;
        QPointer<DetachableMdiSubWindow> refPtr = referenceWindow;
        connect(window, &QObject::destroyed, this, [self, refPtr]() {
            if (self && refPtr)
                self->bring_window_to_front(refPtr.data());
        });
    }
}

void EntityController::connect_dialog_close(DetailDialogBase* dialog,
                                            DetachableMdiSubWindow* window) {
    connect(dialog, &DetailDialogBase::closeRequested, window, &QWidget::close);
}

void EntityController::connect_dialog_close(HistoryDialogBase* dialog,
                                            DetachableMdiSubWindow* window) {
    connect(dialog, &HistoryDialogBase::closeRequested, window, &QWidget::close);
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

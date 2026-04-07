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
#include "ores.qt/SystemSettingController.hpp"
#include "ores.qt/ChangeReasonCache.hpp"

#include <QMdiSubWindow>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/SystemSettingMdiWindow.hpp"
#include "ores.qt/SystemSettingDetailDialog.hpp"
#include "ores.qt/SystemSettingHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.variability.api/eventing/system_setting_changed_event.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"
#include "ores.qt/ExceptionHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    // Event type name for system setting changes
    constexpr std::string_view system_setting_event_name =
        eventing::domain::event_traits<
            variability::eventing::system_setting_changed_event>::name;
}

SystemSettingController::SystemSettingController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, {}, parent),
      changeReasonCache_(changeReasonCache),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "SystemSettingController created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &SystemSettingController::onNotificationReceived);

        // Subscribe to events when logged in
        connect(clientManager_, &ClientManager::loggedIn,
                this, [self = QPointer<SystemSettingController>(this)]() {
            if (!self) return;
            BOOST_LOG_SEV(lg(), info) << "Subscribing to system setting change events";
            self->clientManager_->subscribeToEvent(std::string{system_setting_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [self = QPointer<SystemSettingController>(this)]() {
            if (!self) return;
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to system setting change events after reconnect";
            self->clientManager_->subscribeToEvent(std::string{system_setting_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to system setting change events";
            clientManager_->subscribeToEvent(std::string{system_setting_event_name});
        }
    }
}

SystemSettingController::~SystemSettingController() {
    BOOST_LOG_SEV(lg(), debug) << "SystemSettingController destroyed";

    // Unsubscribe from system setting change events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from system setting change events";
        clientManager_->unsubscribeFromEvent(std::string{system_setting_event_name});
    }
}

void SystemSettingController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "system_settings");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing system settings list window";
        return;
    }

    // Create new window
    listWindow_ = new SystemSettingMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &SystemSettingMdiWindow::statusChanged,
            this, &SystemSettingController::statusMessage);
    connect(listWindow_, &SystemSettingMdiWindow::errorOccurred,
            this, &SystemSettingController::errorMessage);
    connect(listWindow_, &SystemSettingMdiWindow::addNewRequested,
            this, &SystemSettingController::onAddNewRequested);
    connect(listWindow_, &SystemSettingMdiWindow::showSystemSettingDetails,
            this, &SystemSettingController::onShowDetails);
    connect(listWindow_, &SystemSettingMdiWindow::showHistoryRequested,
            this, &SystemSettingController::onShowHistory);
    connect(listWindow_, &SystemSettingMdiWindow::systemSettingDeleted,
            this, &SystemSettingController::onSystemSettingDeleted);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("System Settings");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Flag, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<SystemSettingController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "System settings list window created";
}

void SystemSettingController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    // Close all managed windows
    QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key)) {
            window->close();
        }
    }
    managed_windows_.clear();

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void SystemSettingController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void SystemSettingController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), debug) << "Add new system setting requested";
    showDetailWindow({}, true);
}

void SystemSettingController::onShowDetails(
    const variability::domain::system_setting& flag) {
    BOOST_LOG_SEV(lg(), debug) << "Show details requested for: " << flag.name;
    showDetailWindow(flag, false);
}

void SystemSettingController::showDetailWindow(
    const variability::domain::system_setting& flag, bool createMode) {

    const QString identifier = createMode ? "new" :
        QString::fromStdString(flag.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window for: "
                                   << identifier.toStdString();
        return;
    }

    // Create detail dialog
    auto* detailDialog = new SystemSettingDetailDialog();
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    if (createMode) {
        detailDialog->setSystemSetting({});
        detailDialog->setCreateMode(true);
    } else {
        detailDialog->setSystemSetting(flag);
    }

    // Connect signals
    connect(detailDialog, &SystemSettingDetailDialog::statusMessage,
            this, &SystemSettingController::statusMessage);
    connect(detailDialog, &SystemSettingDetailDialog::errorMessage,
            this, &SystemSettingController::errorMessage);
    connect(detailDialog, &SystemSettingDetailDialog::systemSettingSaved,
            this, &SystemSettingController::onSystemSettingSaved);
    connect(detailDialog, &SystemSettingDetailDialog::systemSettingDeleted,
            this, &SystemSettingController::onSystemSettingDeleted);

    // Create MDI subwindow
    auto* subWindow = new DetachableMdiSubWindow(mainWindow_);
    subWindow->setWidget(detailDialog);

    const QString title = createMode ? "New System Setting" :
        QString("System Setting: %1").arg(QString::fromStdString(flag.name));
    subWindow->setWindowTitle(title);
    subWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Flag, IconUtils::DefaultIconColor));
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->resize(detailDialog->sizeHint());

    mdiArea_->addSubWindow(subWindow);
    subWindow->show();

    // Track window
    track_window(key, subWindow);
    register_detachable_window(subWindow);

    // Cleanup when closed
    connect(subWindow, &QObject::destroyed, this, [self = QPointer<SystemSettingController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
    });

    BOOST_LOG_SEV(lg(), debug) << "Detail window created for: "
                               << identifier.toStdString();
}

void SystemSettingController::onSystemSettingSaved(const QString& name) {
    BOOST_LOG_SEV(lg(), info) << "System setting saved: " << name.toStdString();
    if (listWindow_) {
        listWindow_->markAsStale();
    }
}

void SystemSettingController::onSystemSettingDeleted(const QString& name) {
    BOOST_LOG_SEV(lg(), info) << "System setting deleted: " << name.toStdString();
    if (listWindow_) {
        listWindow_->markAsStale();
    }
}

void SystemSettingController::refreshListWindow() {
    if (listWindow_) {
        BOOST_LOG_SEV(lg(), debug) << "Refreshing system settings list";
        handleEntitySaved();
    }
}

void SystemSettingController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds, const QString& /*tenantId*/) {
    // Check if this is a system setting change event
    if (eventType != QString::fromStdString(std::string{system_setting_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received system setting change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " flag names";

    // Mark the list window as stale if it's open
    if (listWindow_) {
        listWindow_->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked system setting window as stale";
    }

    // Notify open history dialogs for affected system settings
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        const QString& key = it.key();
        auto* window = it.value();
        if (!window)
            continue;

        // Check if this is a history window for an affected system setting
        if (key.startsWith("history:")) {
            QString windowFlagName = key.mid(8);  // Remove "history:" prefix
            if (entityIds.isEmpty() || entityIds.contains(windowFlagName)) {
                // Mark history dialog as stale
                auto* historyDialog = qobject_cast<SystemSettingHistoryDialog*>(
                    window->widget());
                if (historyDialog) {
                    historyDialog->markAsStale();
                    BOOST_LOG_SEV(lg(), debug) << "Marked history dialog as stale for: "
                                               << windowFlagName.toStdString();
                }
            }
        }
    }
}

void SystemSettingController::onShowHistory(const QString& name) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << name.toStdString();
    showHistoryWindow(name);
}

void SystemSettingController::showHistoryWindow(const QString& name) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for system setting: "
                             << name.toStdString();

    const QString windowKey = build_window_key("history", name);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << name.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << name.toStdString();

    auto* historyDialog = new SystemSettingHistoryDialog(name, clientManager_, mainWindow_);

    connect(historyDialog, &SystemSettingHistoryDialog::statusChanged,
            this, [self = QPointer<SystemSettingController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &SystemSettingHistoryDialog::errorOccurred,
            this, [self = QPointer<SystemSettingController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &SystemSettingHistoryDialog::revertVersionRequested,
            this, &SystemSettingController::onRevertSystemSetting);
    connect(historyDialog, &SystemSettingHistoryDialog::openVersionRequested,
            this, &SystemSettingController::onOpenSystemSettingVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow();
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("System Setting History: %1").arg(name));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<SystemSettingController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    mdiArea_->addSubWindow(historyWindow);
    historyWindow->adjustSize();

    // If the parent list window is detached, detach this window too
    if (listMdiSubWindow_ && listMdiSubWindow_->isDetached()) {
        historyWindow->show();
        historyWindow->detach();

        QPoint parentPos = listMdiSubWindow_->pos();
        historyWindow->move(parentPos.x() + 30, parentPos.y() + 30);
    } else {
        historyWindow->show();
    }
}

void SystemSettingController::onOpenSystemSettingVersion(
    const variability::domain::system_setting& flag, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for system setting: " << flag.name;

    const QString flagName = QString::fromStdString(flag.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(flagName).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new SystemSettingDetailDialog();
    if (clientManager_) {
        detailDialog->setClientManager(clientManager_);
        detailDialog->setUsername(username_.toStdString());
    }

    connect(detailDialog, &SystemSettingDetailDialog::statusMessage,
            this, [self = QPointer<SystemSettingController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &SystemSettingDetailDialog::errorMessage,
            this, [self = QPointer<SystemSettingController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    // Try to get history from the sender (history dialog) for version navigation
    auto* historyDialog = qobject_cast<SystemSettingHistoryDialog*>(sender());
    if (historyDialog && !historyDialog->getHistory().empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Using history from sender for version navigation";
        detailDialog->setHistory(historyDialog->getHistory(), versionNumber);
    } else {
        // Fallback: just show single version without navigation
        detailDialog->setSystemSetting(flag);
        detailDialog->setReadOnly(true, versionNumber);
    }

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("System Setting: %1 (Version %2 - Read Only)")
        .arg(flagName).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Flag, IconUtils::DefaultIconColor));

    // Track this version window
    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<SystemSettingController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    mdiArea_->addSubWindow(detailWindow);
    detailWindow->adjustSize();
    detailWindow->show();
}

void SystemSettingController::onRevertSystemSetting(
    const variability::domain::system_setting& flag) {
    BOOST_LOG_SEV(lg(), info) << "Reverting system setting: " << flag.name
                              << " to version " << flag.version;

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorMessage("Cannot revert - not connected to server");
        return;
    }

    // Save the flag (which creates a new version with the old data)
    variability::messaging::save_setting_request request;
    request.data = flag;
    auto response_result = clientManager_->process_authenticated_request(std::move(request));
    if (!response_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to send revert request";
        emit errorMessage("Failed to communicate with server");
        return;
    }

    if (response_result->success) {
        BOOST_LOG_SEV(lg(), info) << "System setting reverted successfully";
        emit statusMessage(QString("System setting '%1' reverted successfully")
            .arg(QString::fromStdString(flag.name)));

        // Mark list and history windows as stale
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    } else {
        const std::string msg = response_result->message;
        BOOST_LOG_SEV(lg(), error) << "Revert failed: " << msg;
        emit errorMessage(QString("Failed to revert system setting: %1")
            .arg(QString::fromStdString(msg)));
    }
}

}

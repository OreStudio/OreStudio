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
#include "ores.qt/AppVersionController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/AppVersionMdiWindow.hpp"
#include "ores.qt/AppVersionDetailDialog.hpp"
#include "ores.qt/AppVersionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.compute/eventing/app_version_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view app_version_event_name =
        eventing::domain::event_traits<
            compute::eventing::app_version_changed_event>::name;
}

AppVersionController::AppVersionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          app_version_event_name, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "AppVersionController created";
}

void AppVersionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "app_versions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new AppVersionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &AppVersionMdiWindow::statusChanged,
            this, &AppVersionController::statusMessage);
    connect(listWindow_, &AppVersionMdiWindow::errorOccurred,
            this, &AppVersionController::errorMessage);
    connect(listWindow_, &AppVersionMdiWindow::showVersionDetails,
            this, &AppVersionController::onShowDetails);
    connect(listWindow_, &AppVersionMdiWindow::addNewRequested,
            this, &AppVersionController::onAddNewRequested);
    connect(listWindow_, &AppVersionMdiWindow::showVersionHistory,
            this, &AppVersionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("App Versions");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Code, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<AppVersionController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "App Version list window created";
}

void AppVersionController::closeAllWindows() {
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

void AppVersionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void AppVersionController::onShowDetails(
    const compute::domain::app_version& app_version) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << app_version.wrapper_version;
    showDetailWindow(app_version);
}

void AppVersionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new app version requested";
    showAddWindow();
}

void AppVersionController::onShowHistory(
    const compute::domain::app_version& app_version) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << app_version.wrapper_version;
    showHistoryWindow(app_version);
}

void AppVersionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new app version";

    auto* detailDialog = new AppVersionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &AppVersionDetailDialog::statusMessage,
            this, &AppVersionController::statusMessage);
    connect(detailDialog, &AppVersionDetailDialog::errorMessage,
            this, &AppVersionController::errorMessage);
    connect(detailDialog, &AppVersionDetailDialog::app_versionSaved,
            this, [self = QPointer<AppVersionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "App Version saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New App Version");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Code, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void AppVersionController::showDetailWindow(
    const compute::domain::app_version& app_version) {

    const QString identifier = QString::fromStdString(app_version.wrapper_version);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << app_version.wrapper_version;

    auto* detailDialog = new AppVersionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setVersion(app_version);

    connect(detailDialog, &AppVersionDetailDialog::statusMessage,
            this, &AppVersionController::statusMessage);
    connect(detailDialog, &AppVersionDetailDialog::errorMessage,
            this, &AppVersionController::errorMessage);
    connect(detailDialog, &AppVersionDetailDialog::app_versionSaved,
            this, [self = QPointer<AppVersionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "App Version saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &AppVersionDetailDialog::app_versionDeleted,
            this, [self = QPointer<AppVersionController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "App Version deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("App Version: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Code, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<AppVersionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void AppVersionController::showHistoryWindow(
    const compute::domain::app_version& app_version) {
    const QString code = QString::fromStdString(app_version.wrapper_version);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for app version: "
                              << app_version.wrapper_version;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << app_version.wrapper_version;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << app_version.wrapper_version;

    auto* historyDialog = new AppVersionHistoryDialog(
        app_version.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &AppVersionHistoryDialog::statusChanged,
            this, [self = QPointer<AppVersionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &AppVersionHistoryDialog::errorOccurred,
            this, [self = QPointer<AppVersionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &AppVersionHistoryDialog::revertVersionRequested,
            this, &AppVersionController::onRevertVersion);
    connect(historyDialog, &AppVersionHistoryDialog::openVersionRequested,
            this, &AppVersionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("App Version History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<AppVersionController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void AppVersionController::onOpenVersion(
    const compute::domain::app_version& app_version, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for app version: " << app_version.wrapper_version;

    const QString code = QString::fromStdString(app_version.wrapper_version);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new AppVersionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setVersion(app_version);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &AppVersionDetailDialog::statusMessage,
            this, [self = QPointer<AppVersionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &AppVersionDetailDialog::errorMessage,
            this, [self = QPointer<AppVersionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("App Version: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<AppVersionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void AppVersionController::onRevertVersion(
    const compute::domain::app_version& app_version) {
    BOOST_LOG_SEV(lg(), info) << "Reverting app version to version: "
                              << app_version.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new AppVersionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setVersion(app_version);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &AppVersionDetailDialog::statusMessage,
            this, &AppVersionController::statusMessage);
    connect(detailDialog, &AppVersionDetailDialog::errorMessage,
            this, &AppVersionController::errorMessage);
    connect(detailDialog, &AppVersionDetailDialog::app_versionSaved,
            this, [self = QPointer<AppVersionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "App Version reverted: " << code.toStdString();
        emit self->statusMessage(QString("App Version '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert App Version: %1")
        .arg(QString::fromStdString(app_version.wrapper_version)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* AppVersionController::listWindow() const {
    return listWindow_;
}

}

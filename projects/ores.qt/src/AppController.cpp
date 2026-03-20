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
#include "ores.qt/AppController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/AppMdiWindow.hpp"
#include "ores.qt/AppDetailDialog.hpp"
#include "ores.qt/AppHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.compute/eventing/app_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view app_event_name =
        eventing::domain::event_traits<
            compute::eventing::app_changed_event>::name;
}

AppController::AppController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          app_event_name, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "AppController created";
}

void AppController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "apps");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new AppMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &AppMdiWindow::statusChanged,
            this, &AppController::statusMessage);
    connect(listWindow_, &AppMdiWindow::errorOccurred,
            this, &AppController::errorMessage);
    connect(listWindow_, &AppMdiWindow::showAppDetails,
            this, &AppController::onShowDetails);
    connect(listWindow_, &AppMdiWindow::addNewRequested,
            this, &AppController::onAddNewRequested);
    connect(listWindow_, &AppMdiWindow::showAppHistory,
            this, &AppController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Compute Apps");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::TasksApp, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<AppController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "App list window created";
}

void AppController::closeAllWindows() {
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

void AppController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void AppController::onShowDetails(
    const compute::domain::app& app) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << app.name;
    showDetailWindow(app);
}

void AppController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new compute app requested";
    showAddWindow();
}

void AppController::onShowHistory(
    const compute::domain::app& app) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << app.name;
    showHistoryWindow(app);
}

void AppController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new compute app";

    auto* detailDialog = new AppDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &AppDetailDialog::statusMessage,
            this, &AppController::statusMessage);
    connect(detailDialog, &AppDetailDialog::errorMessage,
            this, &AppController::errorMessage);
    connect(detailDialog, &AppDetailDialog::appSaved,
            this, [self = QPointer<AppController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "App saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New App");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::TasksApp, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void AppController::showDetailWindow(
    const compute::domain::app& app) {

    const QString identifier = QString::fromStdString(app.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << app.name;

    auto* detailDialog = new AppDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setApp(app);

    connect(detailDialog, &AppDetailDialog::statusMessage,
            this, &AppController::statusMessage);
    connect(detailDialog, &AppDetailDialog::errorMessage,
            this, &AppController::errorMessage);
    connect(detailDialog, &AppDetailDialog::appSaved,
            this, [self = QPointer<AppController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "App saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &AppDetailDialog::appDeleted,
            this, [self = QPointer<AppController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "App deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("App: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::TasksApp, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<AppController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void AppController::showHistoryWindow(
    const compute::domain::app& app) {
    const QString code = QString::fromStdString(app.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for compute app: "
                              << app.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << app.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << app.name;

    auto* historyDialog = new AppHistoryDialog(
        app.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &AppHistoryDialog::statusChanged,
            this, [self = QPointer<AppController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &AppHistoryDialog::errorOccurred,
            this, [self = QPointer<AppController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &AppHistoryDialog::revertVersionRequested,
            this, &AppController::onRevertVersion);
    connect(historyDialog, &AppHistoryDialog::openVersionRequested,
            this, &AppController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("App History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<AppController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void AppController::onOpenVersion(
    const compute::domain::app& app, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for compute app: " << app.name;

    const QString code = QString::fromStdString(app.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new AppDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setApp(app);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &AppDetailDialog::statusMessage,
            this, [self = QPointer<AppController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &AppDetailDialog::errorMessage,
            this, [self = QPointer<AppController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("App: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<AppController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void AppController::onRevertVersion(
    const compute::domain::app& app) {
    BOOST_LOG_SEV(lg(), info) << "Reverting compute app to version: "
                              << app.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new AppDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setApp(app);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &AppDetailDialog::statusMessage,
            this, &AppController::statusMessage);
    connect(detailDialog, &AppDetailDialog::errorMessage,
            this, &AppController::errorMessage);
    connect(detailDialog, &AppDetailDialog::appSaved,
            this, [self = QPointer<AppController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "App reverted: " << code.toStdString();
        emit self->statusMessage(QString("App '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert App: %1")
        .arg(QString::fromStdString(app.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* AppController::listWindow() const {
    return listWindow_;
}

}

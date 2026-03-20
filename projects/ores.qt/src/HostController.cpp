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
#include "ores.qt/HostController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/HostMdiWindow.hpp"
#include "ores.qt/HostDetailDialog.hpp"
#include "ores.qt/HostHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

HostController::HostController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "HostController created";
}

void HostController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "hosts");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new HostMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &HostMdiWindow::statusChanged,
            this, &HostController::statusMessage);
    connect(listWindow_, &HostMdiWindow::errorOccurred,
            this, &HostController::errorMessage);
    connect(listWindow_, &HostMdiWindow::showHostDetails,
            this, &HostController::onShowDetails);
    connect(listWindow_, &HostMdiWindow::addNewRequested,
            this, &HostController::onAddNewRequested);
    connect(listWindow_, &HostMdiWindow::showHostHistory,
            this, &HostController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Compute Hosts");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Server, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<HostController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Host list window created";
}

void HostController::closeAllWindows() {
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

void HostController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void HostController::onShowDetails(
    const compute::domain::host& host) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << host.external_id;
    showDetailWindow(host);
}

void HostController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new compute host requested";
    showAddWindow();
}

void HostController::onShowHistory(
    const compute::domain::host& host) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << host.external_id;
    showHistoryWindow(host);
}

void HostController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new compute host";

    auto* detailDialog = new HostDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &HostDetailDialog::statusMessage,
            this, &HostController::statusMessage);
    connect(detailDialog, &HostDetailDialog::errorMessage,
            this, &HostController::errorMessage);
    connect(detailDialog, &HostDetailDialog::hostSaved,
            this, [self = QPointer<HostController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Host saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Host");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Server, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void HostController::showDetailWindow(
    const compute::domain::host& host) {

    const QString identifier = QString::fromStdString(host.external_id);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << host.external_id;

    auto* detailDialog = new HostDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setHost(host);

    connect(detailDialog, &HostDetailDialog::statusMessage,
            this, &HostController::statusMessage);
    connect(detailDialog, &HostDetailDialog::errorMessage,
            this, &HostController::errorMessage);
    connect(detailDialog, &HostDetailDialog::hostSaved,
            this, [self = QPointer<HostController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Host saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &HostDetailDialog::hostDeleted,
            this, [self = QPointer<HostController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Host deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Host: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Server, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<HostController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void HostController::showHistoryWindow(
    const compute::domain::host& host) {
    const QString code = QString::fromStdString(host.external_id);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for compute host: "
                              << host.external_id;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << host.external_id;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << host.external_id;

    auto* historyDialog = new HostHistoryDialog(
        host.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &HostHistoryDialog::statusChanged,
            this, [self = QPointer<HostController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &HostHistoryDialog::errorOccurred,
            this, [self = QPointer<HostController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &HostHistoryDialog::revertVersionRequested,
            this, &HostController::onRevertVersion);
    connect(historyDialog, &HostHistoryDialog::openVersionRequested,
            this, &HostController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Host History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<HostController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void HostController::onOpenVersion(
    const compute::domain::host& host, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for compute host: " << host.external_id;

    const QString code = QString::fromStdString(host.external_id);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new HostDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setHost(host);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &HostDetailDialog::statusMessage,
            this, [self = QPointer<HostController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &HostDetailDialog::errorMessage,
            this, [self = QPointer<HostController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Host: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<HostController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void HostController::onRevertVersion(
    const compute::domain::host& host) {
    BOOST_LOG_SEV(lg(), info) << "Reverting compute host to version: "
                              << host.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new HostDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setHost(host);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &HostDetailDialog::statusMessage,
            this, &HostController::statusMessage);
    connect(detailDialog, &HostDetailDialog::errorMessage,
            this, &HostController::errorMessage);
    connect(detailDialog, &HostDetailDialog::hostSaved,
            this, [self = QPointer<HostController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Host reverted: " << code.toStdString();
        emit self->statusMessage(QString("Host '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Host: %1")
        .arg(QString::fromStdString(host.external_id)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* HostController::listWindow() const {
    return listWindow_;
}

}

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
#include "ores.qt/PartyStatusController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PartyStatusMdiWindow.hpp"
#include "ores.qt/PartyStatusDetailDialog.hpp"
#include "ores.qt/PartyStatusHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

PartyStatusController::PartyStatusController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PartyStatusController created";
}

void PartyStatusController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "party_statuses");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PartyStatusMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &PartyStatusMdiWindow::statusChanged,
            this, &PartyStatusController::statusMessage);
    connect(listWindow_, &PartyStatusMdiWindow::errorOccurred,
            this, &PartyStatusController::errorMessage);
    connect(listWindow_, &PartyStatusMdiWindow::showStatusDetails,
            this, &PartyStatusController::onShowDetails);
    connect(listWindow_, &PartyStatusMdiWindow::addNewRequested,
            this, &PartyStatusController::onAddNewRequested);
    connect(listWindow_, &PartyStatusMdiWindow::showStatusHistory,
            this, &PartyStatusController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Party Statuses");
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
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<PartyStatusController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Party Status list window created";
}

void PartyStatusController::closeAllWindows() {
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

void PartyStatusController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PartyStatusController::onShowDetails(
    const refdata::domain::party_status& status) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << status.code;
    showDetailWindow(status);
}

void PartyStatusController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new party status requested";
    showAddWindow();
}

void PartyStatusController::onShowHistory(
    const refdata::domain::party_status& status) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << status.code;
    showHistoryWindow(QString::fromStdString(status.code));
}

void PartyStatusController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new party status";

    auto* detailDialog = new PartyStatusDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &PartyStatusDetailDialog::statusMessage,
            this, &PartyStatusController::statusMessage);
    connect(detailDialog, &PartyStatusDetailDialog::errorMessage,
            this, &PartyStatusController::errorMessage);
    connect(detailDialog, &PartyStatusDetailDialog::statusSaved,
            this, [self = QPointer<PartyStatusController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party Status saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Party Status");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Flag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyStatusController::showDetailWindow(
    const refdata::domain::party_status& status) {

    const QString identifier = QString::fromStdString(status.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << status.code;

    auto* detailDialog = new PartyStatusDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setStatus(status);

    connect(detailDialog, &PartyStatusDetailDialog::statusMessage,
            this, &PartyStatusController::statusMessage);
    connect(detailDialog, &PartyStatusDetailDialog::errorMessage,
            this, &PartyStatusController::errorMessage);
    connect(detailDialog, &PartyStatusDetailDialog::statusSaved,
            this, [self = QPointer<PartyStatusController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party Status saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &PartyStatusDetailDialog::statusDeleted,
            this, [self = QPointer<PartyStatusController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party Status deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Party Status: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Flag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PartyStatusController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyStatusController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for party status: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << code.toStdString();

    auto* historyDialog = new PartyStatusHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &PartyStatusHistoryDialog::statusChanged,
            this, [self = QPointer<PartyStatusController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &PartyStatusHistoryDialog::errorOccurred,
            this, [self = QPointer<PartyStatusController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &PartyStatusHistoryDialog::revertVersionRequested,
            this, &PartyStatusController::onRevertVersion);
    connect(historyDialog, &PartyStatusHistoryDialog::openVersionRequested,
            this, &PartyStatusController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Party Status History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<PartyStatusController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PartyStatusController::onOpenVersion(
    const refdata::domain::party_status& status, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for party status: " << status.code;

    const QString code = QString::fromStdString(status.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PartyStatusDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setStatus(status);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &PartyStatusDetailDialog::statusMessage,
            this, [self = QPointer<PartyStatusController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &PartyStatusDetailDialog::errorMessage,
            this, [self = QPointer<PartyStatusController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Party Status: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PartyStatusController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PartyStatusController::onRevertVersion(
    const refdata::domain::party_status& status) {
    BOOST_LOG_SEV(lg(), info) << "Reverting party status to version: "
                              << status.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PartyStatusDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setStatus(status);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &PartyStatusDetailDialog::statusMessage,
            this, &PartyStatusController::statusMessage);
    connect(detailDialog, &PartyStatusDetailDialog::errorMessage,
            this, &PartyStatusController::errorMessage);
    connect(detailDialog, &PartyStatusDetailDialog::statusSaved,
            this, [self = QPointer<PartyStatusController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party Status reverted: " << code.toStdString();
        emit self->statusMessage(QString("Party Status '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Party Status: %1")
        .arg(QString::fromStdString(status.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PartyStatusController::listWindow() const {
    return listWindow_;
}

}

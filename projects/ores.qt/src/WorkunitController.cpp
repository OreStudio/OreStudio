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
#include "ores.qt/WorkunitController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WorkunitMdiWindow.hpp"
#include "ores.qt/WorkunitDetailDialog.hpp"
#include "ores.qt/WorkunitHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkunitController::WorkunitController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "WorkunitController created";
}

void WorkunitController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "workunits");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new WorkunitMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &WorkunitMdiWindow::statusChanged,
            this, &WorkunitController::statusMessage);
    connect(listWindow_, &WorkunitMdiWindow::errorOccurred,
            this, &WorkunitController::errorMessage);
    connect(listWindow_, &WorkunitMdiWindow::showWorkunitDetails,
            this, &WorkunitController::onShowDetails);
    connect(listWindow_, &WorkunitMdiWindow::addNewRequested,
            this, &WorkunitController::onAddNewRequested);
    connect(listWindow_, &WorkunitMdiWindow::showWorkunitHistory,
            this, &WorkunitController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Workunits");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentCode, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<WorkunitController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Workunit list window created";
}

void WorkunitController::closeAllWindows() {
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

void WorkunitController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void WorkunitController::onShowDetails(
    const compute::domain::workunit& workunit) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << workunit.input_uri;
    showDetailWindow(workunit);
}

void WorkunitController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new workunit requested";
    showAddWindow();
}

void WorkunitController::onShowHistory(
    const compute::domain::workunit& workunit) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << workunit.input_uri;
    showHistoryWindow(workunit);
}

void WorkunitController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new workunit";

    auto* detailDialog = new WorkunitDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &WorkunitDetailDialog::statusMessage,
            this, &WorkunitController::statusMessage);
    connect(detailDialog, &WorkunitDetailDialog::errorMessage,
            this, &WorkunitController::errorMessage);
    connect(detailDialog, &WorkunitDetailDialog::workunitSaved,
            this, [self = QPointer<WorkunitController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Workunit saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Workunit");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentCode, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void WorkunitController::showDetailWindow(
    const compute::domain::workunit& workunit) {

    const QString identifier = QString::fromStdString(workunit.input_uri);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << workunit.input_uri;

    auto* detailDialog = new WorkunitDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setWorkunit(workunit);

    connect(detailDialog, &WorkunitDetailDialog::statusMessage,
            this, &WorkunitController::statusMessage);
    connect(detailDialog, &WorkunitDetailDialog::errorMessage,
            this, &WorkunitController::errorMessage);
    connect(detailDialog, &WorkunitDetailDialog::workunitSaved,
            this, [self = QPointer<WorkunitController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Workunit saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &WorkunitDetailDialog::workunitDeleted,
            this, [self = QPointer<WorkunitController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Workunit deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Workunit: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentCode, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<WorkunitController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void WorkunitController::showHistoryWindow(
    const compute::domain::workunit& workunit) {
    const QString code = QString::fromStdString(workunit.input_uri);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for workunit: "
                              << workunit.input_uri;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << workunit.input_uri;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << workunit.input_uri;

    auto* historyDialog = new WorkunitHistoryDialog(
        workunit.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &WorkunitHistoryDialog::statusChanged,
            this, [self = QPointer<WorkunitController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &WorkunitHistoryDialog::errorOccurred,
            this, [self = QPointer<WorkunitController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &WorkunitHistoryDialog::revertVersionRequested,
            this, &WorkunitController::onRevertVersion);
    connect(historyDialog, &WorkunitHistoryDialog::openVersionRequested,
            this, &WorkunitController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Workunit History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<WorkunitController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void WorkunitController::onOpenVersion(
    const compute::domain::workunit& workunit, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for workunit: " << workunit.input_uri;

    const QString code = QString::fromStdString(workunit.input_uri);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new WorkunitDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setWorkunit(workunit);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &WorkunitDetailDialog::statusMessage,
            this, [self = QPointer<WorkunitController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &WorkunitDetailDialog::errorMessage,
            this, [self = QPointer<WorkunitController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Workunit: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<WorkunitController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void WorkunitController::onRevertVersion(
    const compute::domain::workunit& workunit) {
    BOOST_LOG_SEV(lg(), info) << "Reverting workunit to version: "
                              << workunit.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new WorkunitDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setWorkunit(workunit);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &WorkunitDetailDialog::statusMessage,
            this, &WorkunitController::statusMessage);
    connect(detailDialog, &WorkunitDetailDialog::errorMessage,
            this, &WorkunitController::errorMessage);
    connect(detailDialog, &WorkunitDetailDialog::workunitSaved,
            this, [self = QPointer<WorkunitController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Workunit reverted: " << code.toStdString();
        emit self->statusMessage(QString("Workunit '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Workunit: %1")
        .arg(QString::fromStdString(workunit.input_uri)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* WorkunitController::listWindow() const {
    return listWindow_;
}

}

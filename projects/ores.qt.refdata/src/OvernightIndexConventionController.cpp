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
#include "ores.qt/OvernightIndexConventionController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/OvernightIndexConventionMdiWindow.hpp"
#include "ores.qt/OvernightIndexConventionDetailDialog.hpp"
#include "ores.qt/OvernightIndexConventionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

OvernightIndexConventionController::OvernightIndexConventionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "OvernightIndexConventionController created";
}

void OvernightIndexConventionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "overnight_index_conventions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new OvernightIndexConventionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &OvernightIndexConventionMdiWindow::statusChanged,
            this, &OvernightIndexConventionController::statusMessage);
    connect(listWindow_, &OvernightIndexConventionMdiWindow::errorOccurred,
            this, &OvernightIndexConventionController::errorMessage);
    connect(listWindow_, &OvernightIndexConventionMdiWindow::showConventionDetails,
            this, &OvernightIndexConventionController::onShowDetails);
    connect(listWindow_, &OvernightIndexConventionMdiWindow::addNewRequested,
            this, &OvernightIndexConventionController::onAddNewRequested);
    connect(listWindow_, &OvernightIndexConventionMdiWindow::showConventionHistory,
            this, &OvernightIndexConventionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Overnight Index Conventions");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<OvernightIndexConventionController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Overnight Index Convention list window created";
}

void OvernightIndexConventionController::closeAllWindows() {
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

void OvernightIndexConventionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void OvernightIndexConventionController::onShowDetails(
    const refdata::domain::overnight_index_convention& ni) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << ni.id;
    showDetailWindow(ni);
}

void OvernightIndexConventionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new overnight index convention requested";
    showAddWindow();
}

void OvernightIndexConventionController::onShowHistory(
    const refdata::domain::overnight_index_convention& ni) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << ni.id;
    showHistoryWindow(QString::fromStdString(ni.id));
}

void OvernightIndexConventionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new overnight index convention";

    auto* detailDialog = new OvernightIndexConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &OvernightIndexConventionDetailDialog::statusMessage,
            this, &OvernightIndexConventionController::statusMessage);
    connect(detailDialog, &OvernightIndexConventionDetailDialog::errorMessage,
            this, &OvernightIndexConventionController::errorMessage);
    connect(detailDialog, &OvernightIndexConventionDetailDialog::niSaved,
            this, [self = QPointer<OvernightIndexConventionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Overnight Index Convention saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Overnight Index Convention");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void OvernightIndexConventionController::showDetailWindow(
    const refdata::domain::overnight_index_convention& ni) {

    const QString identifier = QString::fromStdString(ni.id);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << ni.id;

    auto* detailDialog = new OvernightIndexConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConvention(ni);

    connect(detailDialog, &OvernightIndexConventionDetailDialog::statusMessage,
            this, &OvernightIndexConventionController::statusMessage);
    connect(detailDialog, &OvernightIndexConventionDetailDialog::errorMessage,
            this, &OvernightIndexConventionController::errorMessage);
    connect(detailDialog, &OvernightIndexConventionDetailDialog::niSaved,
            this, [self = QPointer<OvernightIndexConventionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Overnight Index Convention saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &OvernightIndexConventionDetailDialog::niDeleted,
            this, [self = QPointer<OvernightIndexConventionController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Overnight Index Convention deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Overnight Index Convention: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<OvernightIndexConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void OvernightIndexConventionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for overnight index convention: "
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

    auto* historyDialog = new OvernightIndexConventionHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &OvernightIndexConventionHistoryDialog::statusChanged,
            this, [self = QPointer<OvernightIndexConventionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &OvernightIndexConventionHistoryDialog::errorOccurred,
            this, [self = QPointer<OvernightIndexConventionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &OvernightIndexConventionHistoryDialog::revertVersionRequested,
            this, &OvernightIndexConventionController::onRevertVersion);
    connect(historyDialog, &OvernightIndexConventionHistoryDialog::openVersionRequested,
            this, &OvernightIndexConventionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Overnight Index Convention History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<OvernightIndexConventionController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void OvernightIndexConventionController::onOpenVersion(
    const refdata::domain::overnight_index_convention& ni, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for overnight index convention: " << ni.id;

    const QString code = QString::fromStdString(ni.id);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new OvernightIndexConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(ni);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &OvernightIndexConventionDetailDialog::statusMessage,
            this, [self = QPointer<OvernightIndexConventionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &OvernightIndexConventionDetailDialog::errorMessage,
            this, [self = QPointer<OvernightIndexConventionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Overnight Index Convention: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<OvernightIndexConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void OvernightIndexConventionController::onRevertVersion(
    const refdata::domain::overnight_index_convention& ni) {
    BOOST_LOG_SEV(lg(), info) << "Reverting overnight index convention to version: "
                              << ni.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new OvernightIndexConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(ni);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &OvernightIndexConventionDetailDialog::statusMessage,
            this, &OvernightIndexConventionController::statusMessage);
    connect(detailDialog, &OvernightIndexConventionDetailDialog::errorMessage,
            this, &OvernightIndexConventionController::errorMessage);
    connect(detailDialog, &OvernightIndexConventionDetailDialog::niSaved,
            this, [self = QPointer<OvernightIndexConventionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Overnight Index Convention reverted: " << code.toStdString();
        emit self->statusMessage(QString("Overnight Index Convention '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Overnight Index Convention: %1")
        .arg(QString::fromStdString(ni.id)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* OvernightIndexConventionController::listWindow() const {
    return listWindow_;
}

}

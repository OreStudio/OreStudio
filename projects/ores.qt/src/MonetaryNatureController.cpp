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
#include "ores.qt/MonetaryNatureController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MonetaryNatureMdiWindow.hpp"
#include "ores.qt/MonetaryNatureDetailDialog.hpp"
#include "ores.qt/MonetaryNatureHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

MonetaryNatureController::MonetaryNatureController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "MonetaryNatureController created";
}

void MonetaryNatureController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "monetary_natures");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new MonetaryNatureMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &MonetaryNatureMdiWindow::statusChanged,
            this, &MonetaryNatureController::statusMessage);
    connect(listWindow_, &MonetaryNatureMdiWindow::errorOccurred,
            this, &MonetaryNatureController::errorMessage);
    connect(listWindow_, &MonetaryNatureMdiWindow::showClassDetails,
            this, &MonetaryNatureController::onShowDetails);
    connect(listWindow_, &MonetaryNatureMdiWindow::addNewRequested,
            this, &MonetaryNatureController::onAddNewRequested);
    connect(listWindow_, &MonetaryNatureMdiWindow::showClassHistory,
            this, &MonetaryNatureController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Monetary Natures");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Classification, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<MonetaryNatureController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Monetary Nature list window created";
}

void MonetaryNatureController::closeAllWindows() {
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

void MonetaryNatureController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void MonetaryNatureController::onShowDetails(
    const refdata::domain::monetary_nature& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void MonetaryNatureController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new monetary nature requested";
    showAddWindow();
}

void MonetaryNatureController::onShowHistory(
    const refdata::domain::monetary_nature& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void MonetaryNatureController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new monetary nature";

    auto* detailDialog = new MonetaryNatureDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &MonetaryNatureDetailDialog::statusMessage,
            this, &MonetaryNatureController::statusMessage);
    connect(detailDialog, &MonetaryNatureDetailDialog::errorMessage,
            this, &MonetaryNatureController::errorMessage);
    connect(detailDialog, &MonetaryNatureDetailDialog::typeSaved,
            this, [self = QPointer<MonetaryNatureController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Monetary Nature saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Monetary Nature");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Classification, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void MonetaryNatureController::showDetailWindow(
    const refdata::domain::monetary_nature& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new MonetaryNatureDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setClass(type);

    connect(detailDialog, &MonetaryNatureDetailDialog::statusMessage,
            this, &MonetaryNatureController::statusMessage);
    connect(detailDialog, &MonetaryNatureDetailDialog::errorMessage,
            this, &MonetaryNatureController::errorMessage);
    connect(detailDialog, &MonetaryNatureDetailDialog::typeSaved,
            this, [self = QPointer<MonetaryNatureController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Monetary Nature saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &MonetaryNatureDetailDialog::typeDeleted,
            this, [self = QPointer<MonetaryNatureController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Monetary Nature deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Monetary Nature: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Classification, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<MonetaryNatureController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void MonetaryNatureController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for monetary nature: "
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

    auto* historyDialog = new MonetaryNatureHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &MonetaryNatureHistoryDialog::statusChanged,
            this, [self = QPointer<MonetaryNatureController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &MonetaryNatureHistoryDialog::errorOccurred,
            this, [self = QPointer<MonetaryNatureController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &MonetaryNatureHistoryDialog::revertVersionRequested,
            this, &MonetaryNatureController::onRevertVersion);
    connect(historyDialog, &MonetaryNatureHistoryDialog::openVersionRequested,
            this, &MonetaryNatureController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Monetary Nature History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<MonetaryNatureController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void MonetaryNatureController::onOpenVersion(
    const refdata::domain::monetary_nature& type, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for monetary nature: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new MonetaryNatureDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setClass(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &MonetaryNatureDetailDialog::statusMessage,
            this, [self = QPointer<MonetaryNatureController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &MonetaryNatureDetailDialog::errorMessage,
            this, [self = QPointer<MonetaryNatureController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Monetary Nature: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<MonetaryNatureController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void MonetaryNatureController::onRevertVersion(
    const refdata::domain::monetary_nature& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting monetary nature to version: "
                              << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new MonetaryNatureDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setClass(type);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &MonetaryNatureDetailDialog::statusMessage,
            this, &MonetaryNatureController::statusMessage);
    connect(detailDialog, &MonetaryNatureDetailDialog::errorMessage,
            this, &MonetaryNatureController::errorMessage);
    connect(detailDialog, &MonetaryNatureDetailDialog::typeSaved,
            this, [self = QPointer<MonetaryNatureController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Monetary Nature reverted: " << code.toStdString();
        emit self->statusMessage(QString("Monetary Nature '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Monetary Nature: %1")
        .arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* MonetaryNatureController::listWindow() const {
    return listWindow_;
}

}

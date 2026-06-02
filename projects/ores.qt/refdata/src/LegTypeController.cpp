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
#include "ores.qt/LegTypeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LegTypeMdiWindow.hpp"
#include "ores.qt/LegTypeDetailDialog.hpp"
#include "ores.qt/LegTypeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

LegTypeController::LegTypeController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "LegTypeController created";
}

void LegTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "leg_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new LegTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &LegTypeMdiWindow::statusChanged,
            this, &LegTypeController::statusMessage);
    connect(listWindow_, &LegTypeMdiWindow::errorOccurred,
            this, &LegTypeController::errorMessage);
    connect(listWindow_, &LegTypeMdiWindow::showTypeDetails,
            this, &LegTypeController::onShowDetails);
    connect(listWindow_, &LegTypeMdiWindow::addNewRequested,
            this, &LegTypeController::onAddNewRequested);
    connect(listWindow_, &LegTypeMdiWindow::showTypeHistory,
            this, &LegTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Leg Types");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<LegTypeController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Leg Type list window created";
}

void LegTypeController::closeAllWindows() {
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

void LegTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void LegTypeController::onShowDetails(
    const trading::domain::leg_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void LegTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new leg type requested";
    showAddWindow();
}

void LegTypeController::onShowHistory(
    const trading::domain::leg_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void LegTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new leg type";

    auto* detailDialog = new LegTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &LegTypeDetailDialog::statusMessage,
            this, &LegTypeController::statusMessage);
    connect(detailDialog, &LegTypeDetailDialog::errorMessage,
            this, &LegTypeController::errorMessage);
    connect(detailDialog, &LegTypeDetailDialog::typeSaved,
            this, [self = QPointer<LegTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Leg Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Leg Type");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void LegTypeController::showDetailWindow(
    const trading::domain::leg_type& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new LegTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(type);

    connect(detailDialog, &LegTypeDetailDialog::statusMessage,
            this, &LegTypeController::statusMessage);
    connect(detailDialog, &LegTypeDetailDialog::errorMessage,
            this, &LegTypeController::errorMessage);
    connect(detailDialog, &LegTypeDetailDialog::typeSaved,
            this, [self = QPointer<LegTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Leg Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &LegTypeDetailDialog::typeDeleted,
            this, [self = QPointer<LegTypeController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Leg Type deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Leg Type: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<LegTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void LegTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for leg type: "
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

    auto* historyDialog = new LegTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &LegTypeHistoryDialog::statusChanged,
            this, [self = QPointer<LegTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &LegTypeHistoryDialog::errorOccurred,
            this, [self = QPointer<LegTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &LegTypeHistoryDialog::revertVersionRequested,
            this, &LegTypeController::onRevertVersion);
    connect(historyDialog, &LegTypeHistoryDialog::openVersionRequested,
            this, &LegTypeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Leg Type History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<LegTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void LegTypeController::onOpenVersion(
    const trading::domain::leg_type& type, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for leg type: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new LegTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &LegTypeDetailDialog::statusMessage,
            this, [self = QPointer<LegTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &LegTypeDetailDialog::errorMessage,
            this, [self = QPointer<LegTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Leg Type: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<LegTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void LegTypeController::onRevertVersion(
    const trading::domain::leg_type& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting leg type to version: "
                              << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new LegTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &LegTypeDetailDialog::statusMessage,
            this, &LegTypeController::statusMessage);
    connect(detailDialog, &LegTypeDetailDialog::errorMessage,
            this, &LegTypeController::errorMessage);
    connect(detailDialog, &LegTypeDetailDialog::typeSaved,
            this, [self = QPointer<LegTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Leg Type reverted: " << code.toStdString();
        emit self->statusMessage(QString("Leg Type '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Leg Type: %1")
        .arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* LegTypeController::listWindow() const {
    return listWindow_;
}

}

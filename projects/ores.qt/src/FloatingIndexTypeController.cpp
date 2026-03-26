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
#include "ores.qt/FloatingIndexTypeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/FloatingIndexTypeMdiWindow.hpp"
#include "ores.qt/FloatingIndexTypeDetailDialog.hpp"
#include "ores.qt/FloatingIndexTypeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

FloatingIndexTypeController::FloatingIndexTypeController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "FloatingIndexTypeController created";
}

void FloatingIndexTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "floating_index_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new FloatingIndexTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &FloatingIndexTypeMdiWindow::statusChanged,
            this, &FloatingIndexTypeController::statusMessage);
    connect(listWindow_, &FloatingIndexTypeMdiWindow::errorOccurred,
            this, &FloatingIndexTypeController::errorMessage);
    connect(listWindow_, &FloatingIndexTypeMdiWindow::showTypeDetails,
            this, &FloatingIndexTypeController::onShowDetails);
    connect(listWindow_, &FloatingIndexTypeMdiWindow::addNewRequested,
            this, &FloatingIndexTypeController::onAddNewRequested);
    connect(listWindow_, &FloatingIndexTypeMdiWindow::showTypeHistory,
            this, &FloatingIndexTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Floating Index Types");
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
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<FloatingIndexTypeController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Floating Index Type list window created";
}

void FloatingIndexTypeController::closeAllWindows() {
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

void FloatingIndexTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void FloatingIndexTypeController::onShowDetails(
    const trading::domain::floating_index_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void FloatingIndexTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new floating index type requested";
    showAddWindow();
}

void FloatingIndexTypeController::onShowHistory(
    const trading::domain::floating_index_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void FloatingIndexTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new floating index type";

    auto* detailDialog = new FloatingIndexTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &FloatingIndexTypeDetailDialog::statusMessage,
            this, &FloatingIndexTypeController::statusMessage);
    connect(detailDialog, &FloatingIndexTypeDetailDialog::errorMessage,
            this, &FloatingIndexTypeController::errorMessage);
    connect(detailDialog, &FloatingIndexTypeDetailDialog::typeSaved,
            this, [self = QPointer<FloatingIndexTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Floating Index Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Floating Index Type");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FloatingIndexTypeController::showDetailWindow(
    const trading::domain::floating_index_type& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new FloatingIndexTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(type);

    connect(detailDialog, &FloatingIndexTypeDetailDialog::statusMessage,
            this, &FloatingIndexTypeController::statusMessage);
    connect(detailDialog, &FloatingIndexTypeDetailDialog::errorMessage,
            this, &FloatingIndexTypeController::errorMessage);
    connect(detailDialog, &FloatingIndexTypeDetailDialog::typeSaved,
            this, [self = QPointer<FloatingIndexTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Floating Index Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &FloatingIndexTypeDetailDialog::typeDeleted,
            this, [self = QPointer<FloatingIndexTypeController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Floating Index Type deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Floating Index Type: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<FloatingIndexTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FloatingIndexTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for floating index type: "
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

    auto* historyDialog = new FloatingIndexTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &FloatingIndexTypeHistoryDialog::statusChanged,
            this, [self = QPointer<FloatingIndexTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &FloatingIndexTypeHistoryDialog::errorOccurred,
            this, [self = QPointer<FloatingIndexTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &FloatingIndexTypeHistoryDialog::revertVersionRequested,
            this, &FloatingIndexTypeController::onRevertVersion);
    connect(historyDialog, &FloatingIndexTypeHistoryDialog::openVersionRequested,
            this, &FloatingIndexTypeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Floating Index Type History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<FloatingIndexTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void FloatingIndexTypeController::onOpenVersion(
    const trading::domain::floating_index_type& type, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for floating index type: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new FloatingIndexTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &FloatingIndexTypeDetailDialog::statusMessage,
            this, [self = QPointer<FloatingIndexTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &FloatingIndexTypeDetailDialog::errorMessage,
            this, [self = QPointer<FloatingIndexTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Floating Index Type: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<FloatingIndexTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void FloatingIndexTypeController::onRevertVersion(
    const trading::domain::floating_index_type& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting floating index type to version: "
                              << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new FloatingIndexTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &FloatingIndexTypeDetailDialog::statusMessage,
            this, &FloatingIndexTypeController::statusMessage);
    connect(detailDialog, &FloatingIndexTypeDetailDialog::errorMessage,
            this, &FloatingIndexTypeController::errorMessage);
    connect(detailDialog, &FloatingIndexTypeDetailDialog::typeSaved,
            this, [self = QPointer<FloatingIndexTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Floating Index Type reverted: " << code.toStdString();
        emit self->statusMessage(QString("Floating Index Type '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Floating Index Type: %1")
        .arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* FloatingIndexTypeController::listWindow() const {
    return listWindow_;
}

}

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
#include "ores.qt/TenantTypeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TenantTypeMdiWindow.hpp"
#include "ores.qt/TenantTypeDetailDialog.hpp"
#include "ores.qt/TenantTypeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

TenantTypeController::TenantTypeController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "TenantTypeController created";
}

void TenantTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "tenant_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new TenantTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &TenantTypeMdiWindow::statusChanged,
            this, &TenantTypeController::statusMessage);
    connect(listWindow_, &TenantTypeMdiWindow::errorOccurred,
            this, &TenantTypeController::errorMessage);
    connect(listWindow_, &TenantTypeMdiWindow::showTypeDetails,
            this, &TenantTypeController::onShowDetails);
    connect(listWindow_, &TenantTypeMdiWindow::addNewRequested,
            this, &TenantTypeController::onAddNewRequested);
    connect(listWindow_, &TenantTypeMdiWindow::showTypeHistory,
            this, &TenantTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Tenant Types");
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
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<TenantTypeController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Tenant Type list window created";
}

void TenantTypeController::closeAllWindows() {
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

void TenantTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TenantTypeController::onShowDetails(
    const iam::domain::tenant_type& tenant_type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << tenant_type.type;
    showDetailWindow(tenant_type);
}

void TenantTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new tenant type requested";
    showAddWindow();
}

void TenantTypeController::onShowHistory(
    const iam::domain::tenant_type& tenant_type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << tenant_type.type;
    showHistoryWindow(QString::fromStdString(tenant_type.type));
}

void TenantTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new tenant type";

    auto* detailDialog = new TenantTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &TenantTypeDetailDialog::statusMessage,
            this, &TenantTypeController::statusMessage);
    connect(detailDialog, &TenantTypeDetailDialog::errorMessage,
            this, &TenantTypeController::errorMessage);
    connect(detailDialog, &TenantTypeDetailDialog::tenant_typeSaved,
            this, [self = QPointer<TenantTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Tenant Type");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenantTypeController::showDetailWindow(
    const iam::domain::tenant_type& tenant_type) {

    const QString identifier = QString::fromStdString(tenant_type.type);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << tenant_type.type;

    auto* detailDialog = new TenantTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(tenant_type);

    connect(detailDialog, &TenantTypeDetailDialog::statusMessage,
            this, &TenantTypeController::statusMessage);
    connect(detailDialog, &TenantTypeDetailDialog::errorMessage,
            this, &TenantTypeController::errorMessage);
    connect(detailDialog, &TenantTypeDetailDialog::tenant_typeSaved,
            this, [self = QPointer<TenantTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &TenantTypeDetailDialog::tenant_typeDeleted,
            this, [self = QPointer<TenantTypeController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant Type deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenant Type: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TenantTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenantTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for tenant type: "
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

    auto* historyDialog = new TenantTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &TenantTypeHistoryDialog::statusChanged,
            this, [self = QPointer<TenantTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &TenantTypeHistoryDialog::errorOccurred,
            this, [self = QPointer<TenantTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &TenantTypeHistoryDialog::revertVersionRequested,
            this, &TenantTypeController::onRevertVersion);
    connect(historyDialog, &TenantTypeHistoryDialog::openVersionRequested,
            this, &TenantTypeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Tenant Type History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<TenantTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void TenantTypeController::onOpenVersion(
    const iam::domain::tenant_type& tenant_type, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for tenant type: " << tenant_type.type;

    const QString code = QString::fromStdString(tenant_type.type);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new TenantTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(tenant_type);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &TenantTypeDetailDialog::statusMessage,
            this, [self = QPointer<TenantTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &TenantTypeDetailDialog::errorMessage,
            this, [self = QPointer<TenantTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenant Type: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TenantTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void TenantTypeController::onRevertVersion(
    const iam::domain::tenant_type& tenant_type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting tenant type to version: "
                              << tenant_type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new TenantTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(tenant_type);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &TenantTypeDetailDialog::statusMessage,
            this, &TenantTypeController::statusMessage);
    connect(detailDialog, &TenantTypeDetailDialog::errorMessage,
            this, &TenantTypeController::errorMessage);
    connect(detailDialog, &TenantTypeDetailDialog::tenant_typeSaved,
            this, [self = QPointer<TenantTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant Type reverted: " << code.toStdString();
        emit self->statusMessage(QString("Tenant Type '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Tenant Type: %1")
        .arg(QString::fromStdString(tenant_type.type)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* TenantTypeController::listWindow() const {
    return listWindow_;
}

}

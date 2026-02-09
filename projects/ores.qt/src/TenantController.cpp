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
#include "ores.qt/TenantController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TenantMdiWindow.hpp"
#include "ores.qt/TenantDetailDialog.hpp"
#include "ores.qt/TenantHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

TenantController::TenantController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "TenantController created";
}

void TenantController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "tenants");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new TenantMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &TenantMdiWindow::statusChanged,
            this, &TenantController::statusMessage);
    connect(listWindow_, &TenantMdiWindow::errorOccurred,
            this, &TenantController::errorMessage);
    connect(listWindow_, &TenantMdiWindow::showTenantDetails,
            this, &TenantController::onShowDetails);
    connect(listWindow_, &TenantMdiWindow::addNewRequested,
            this, &TenantController::onAddNewRequested);
    connect(listWindow_, &TenantMdiWindow::onboardRequested,
            this, &TenantController::onboardRequested);
    connect(listWindow_, &TenantMdiWindow::showTenantHistory,
            this, &TenantController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Tenants");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Building, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<TenantController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Tenant list window created";
}

void TenantController::closeAllWindows() {
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

void TenantController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TenantController::onShowDetails(
    const iam::domain::tenant& tenant) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << tenant.code;
    showDetailWindow(tenant);
}

void TenantController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new tenant requested";
    showAddWindow();
}

void TenantController::onShowHistory(
    const iam::domain::tenant& tenant) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << tenant.code;
    showHistoryWindow(tenant);
}

void TenantController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new tenant";

    auto* detailDialog = new TenantDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &TenantDetailDialog::statusMessage,
            this, &TenantController::statusMessage);
    connect(detailDialog, &TenantDetailDialog::errorMessage,
            this, &TenantController::errorMessage);
    connect(detailDialog, &TenantDetailDialog::tenantSaved,
            this, [self = QPointer<TenantController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Tenant");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Building, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenantController::showDetailWindow(
    const iam::domain::tenant& tenant) {

    const QString identifier = QString::fromStdString(tenant.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << tenant.code;

    auto* detailDialog = new TenantDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setTenant(tenant);

    connect(detailDialog, &TenantDetailDialog::statusMessage,
            this, &TenantController::statusMessage);
    connect(detailDialog, &TenantDetailDialog::errorMessage,
            this, &TenantController::errorMessage);
    connect(detailDialog, &TenantDetailDialog::tenantSaved,
            this, [self = QPointer<TenantController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &TenantDetailDialog::tenantDeleted,
            this, [self = QPointer<TenantController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenant: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Building, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TenantController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenantController::showHistoryWindow(
    const iam::domain::tenant& tenant) {
    const QString code = QString::fromStdString(tenant.code);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for tenant: "
                              << tenant.code;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << tenant.code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << tenant.code;

    auto* historyDialog = new TenantHistoryDialog(
        tenant.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &TenantHistoryDialog::statusChanged,
            this, [self = QPointer<TenantController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &TenantHistoryDialog::errorOccurred,
            this, [self = QPointer<TenantController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &TenantHistoryDialog::revertVersionRequested,
            this, &TenantController::onRevertVersion);
    connect(historyDialog, &TenantHistoryDialog::openVersionRequested,
            this, &TenantController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Tenant History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<TenantController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void TenantController::onOpenVersion(
    const iam::domain::tenant& tenant, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for tenant: " << tenant.code;

    const QString code = QString::fromStdString(tenant.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new TenantDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setTenant(tenant);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &TenantDetailDialog::statusMessage,
            this, [self = QPointer<TenantController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &TenantDetailDialog::errorMessage,
            this, [self = QPointer<TenantController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenant: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TenantController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void TenantController::onRevertVersion(
    const iam::domain::tenant& tenant) {
    BOOST_LOG_SEV(lg(), info) << "Reverting tenant to version: "
                              << tenant.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new TenantDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setTenant(tenant);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &TenantDetailDialog::statusMessage,
            this, &TenantController::statusMessage);
    connect(detailDialog, &TenantDetailDialog::errorMessage,
            this, &TenantController::errorMessage);
    connect(detailDialog, &TenantDetailDialog::tenantSaved,
            this, [self = QPointer<TenantController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant reverted: " << code.toStdString();
        emit self->statusMessage(QString("Tenant '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Tenant: %1")
        .arg(QString::fromStdString(tenant.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* TenantController::listWindow() const {
    return listWindow_;
}

}

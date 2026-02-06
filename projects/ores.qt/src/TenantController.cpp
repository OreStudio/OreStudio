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

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/TenantMdiWindow.hpp"
#include "ores.qt/TenantDetailDialog.hpp"
#include "ores.qt/TenantHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::logging;

TenantController::TenantController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, {}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {
    BOOST_LOG_SEV(lg(), debug) << "Tenant controller created";
}

TenantController::~TenantController() {
    BOOST_LOG_SEV(lg(), debug) << "Tenant controller destroyed";
}

EntityListMdiWindow* TenantController::listWindow() const {
    return listWindow_;
}

void TenantController::showListWindow() {
    // Reuse existing window if it exists
    if (listMdiSubWindow_) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing tenants window";
        bring_window_to_front(listMdiSubWindow_);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new tenants MDI window";

    listWindow_ = new TenantMdiWindow(clientManager_, username_, mainWindow_);

    // Connect status signals
    connect(listWindow_, &TenantMdiWindow::statusChanged,
            this, [self = QPointer<TenantController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(listWindow_, &TenantMdiWindow::errorOccurred,
            this, [self = QPointer<TenantController>(this)](const QString& err_msg) {
        if (!self) return;
        emit self->errorMessage("Error: " + err_msg);
    });

    // Connect tenant operations
    connect(listWindow_, &TenantMdiWindow::addNewRequested,
            this, &TenantController::onAddNewRequested);
    connect(listWindow_, &TenantMdiWindow::showTenantDetails,
            this, &TenantController::onShowTenantDetails);
    connect(listWindow_, &TenantMdiWindow::showTenantHistory,
            this, &TenantController::onShowTenantHistory);
    connect(listWindow_, &TenantMdiWindow::tenantDeleted,
            this, &TenantController::onTenantDeleted);

    listMdiSubWindow_ = new DetachableMdiSubWindow();
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Tenants");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Building, IconUtils::DefaultIconColor));

    // Track window for detach/reattach operations
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed, this,
        [self = QPointer<TenantController>(this)]() {
        if (!self) return;
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->adjustSize();
    listMdiSubWindow_->show();

    listWindow_->reload();
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

    if (listMdiSubWindow_) {
        listMdiSubWindow_->close();
    }

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void TenantController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TenantController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new tenant requested";
    showDetailWindow(nullptr, true);
}

void TenantController::onShowTenantDetails(const iam::domain::tenant& tenant) {
    BOOST_LOG_SEV(lg(), info) << "Show tenant details requested for: " << tenant.code;
    showDetailWindow(&tenant, false);
}

void TenantController::onShowTenantHistory(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Show tenant history requested for: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    if (try_reuse_window(windowKey)) {
        return;
    }

    auto* historyDialog = new TenantHistoryDialog(code, clientManager_, mainWindow_);

    // Connect signals
    connect(historyDialog, &TenantHistoryDialog::statusChanged,
            this, &TenantController::statusMessage);
    connect(historyDialog, &TenantHistoryDialog::errorOccurred,
            this, &TenantController::errorMessage);
    connect(historyDialog, &TenantHistoryDialog::openVersionRequested,
            this, &TenantController::onOpenTenantVersion);
    connect(historyDialog, &TenantHistoryDialog::revertVersionRequested,
            this, &TenantController::onRevertTenant);

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Tenant History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    register_detachable_window(historyWindow);

    connect(historyWindow, &QObject::destroyed, this,
            [self = QPointer<TenantController>(this), windowKey]() {
        if (!self) return;
        self->managed_windows_.remove(windowKey);
    });

    managed_windows_[windowKey] = historyWindow;

    show_managed_window(historyWindow, listMdiSubWindow_);

    historyDialog->loadHistory();
}

void TenantController::onTenantDeleted(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Tenant deleted: " << code.toStdString();

    // Close any open detail or history windows for this tenant
    const QString detailKey = build_window_key("details", code);
    const QString historyKey = build_window_key("history", code);

    if (auto* window = managed_windows_.value(detailKey)) {
        window->close();
    }
    if (auto* window = managed_windows_.value(historyKey)) {
        window->close();
    }
}

void TenantController::onOpenTenantVersion(const iam::domain::tenant& tenant,
                                            int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Open tenant version " << versionNumber
                              << " in read-only mode for: " << tenant.code;
    showDetailWindow(&tenant, false, true, versionNumber);
}

void TenantController::onRevertTenant(const iam::domain::tenant& tenant) {
    BOOST_LOG_SEV(lg(), info) << "Revert tenant requested for: " << tenant.code;

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorMessage("Not connected to server");
        return;
    }

    QPointer<TenantController> self = this;
    const QString code = QString::fromStdString(tenant.code);

    struct SaveResult {
        bool success;
        QString error_message;
        QString error_details;
    };

    QFuture<SaveResult> future = QtConcurrent::run([self, tenant]() -> SaveResult {
        return exception_helper::wrap_async_fetch<SaveResult>([&]() -> SaveResult {
            if (!self || !self->clientManager_) {
                return {false, "Controller closed"};
            }

            iam::messaging::save_tenant_request request;
            request.tenant = tenant;

            auto result = self->clientManager_->
                process_authenticated_request(std::move(request));

            if (!result) {
                return {false, QString::fromStdString(
                    comms::net::to_string(result.error()))};
            }

            if (!result->success) {
                return {false, QString::fromStdString(result->message)};
            }

            return {true, {}};
        }, "tenant revert");
    });

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self,
        [self, watcher, code]() {
        if (!self) return;

        const auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Tenant reverted successfully";
            emit self->statusMessage(QString("Tenant '%1' reverted successfully").arg(code));

            // Reload list window
            if (self->listWindow_) {
                self->listWindow_->reload();
            }

            // Reload any open history window
            const QString historyKey = self->build_window_key("history", code);
            if (auto* window = self->managed_windows_.value(historyKey)) {
                if (auto* dialog = qobject_cast<TenantHistoryDialog*>(window->widget())) {
                    dialog->loadHistory();
                }
            }
        } else {
            BOOST_LOG_SEV(lg(), error) << "Tenant revert failed: "
                                       << result.error_message.toStdString();
            emit self->errorMessage(QString("Failed to revert tenant: %1")
                .arg(result.error_message));
            MessageBoxHelper::critical(nullptr, "Revert Failed",
                result.error_message);
        }
    });

    watcher->setFuture(future);
}

void TenantController::showDetailWindow(const iam::domain::tenant* tenant,
                                         bool createMode, bool readOnly,
                                         int versionNumber) {

    const QString identifier = tenant ? QString::fromStdString(tenant->code) : "new";
    QString windowKey = build_window_key("details", identifier);

    // For read-only versions, add version to key
    if (readOnly && versionNumber > 0) {
        windowKey = build_window_key("details",
            QString("%1_v%2").arg(identifier).arg(versionNumber));
    }

    if (!createMode && !readOnly && try_reuse_window(windowKey)) {
        return;
    }

    auto* detailDialog = new TenantDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    if (createMode) {
        iam::domain::tenant emptyTenant;
        detailDialog->setTenant(emptyTenant);
    } else if (tenant) {
        detailDialog->setTenant(*tenant);
    }

    if (readOnly) {
        detailDialog->setReadOnly(true, versionNumber);
    }

    // Connect signals
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
            this, [self = QPointer<TenantController>(this), windowKey](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Tenant deleted: " << code.toStdString();
        self->handleEntityDeleted();
        self->onTenantDeleted(code);
    });

    connect(detailDialog, &TenantDetailDialog::revertRequested,
            this, &TenantController::onRevertTenant);

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);

    QString title;
    if (createMode) {
        title = "New Tenant";
    } else if (readOnly && versionNumber > 0) {
        title = QString("Tenant: %1 (v%2)").arg(identifier).arg(versionNumber);
    } else {
        title = QString("Tenant: %1").arg(identifier);
    }
    detailWindow->setWindowTitle(title);
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Building, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect(detailWindow, &QObject::destroyed, this,
            [self = QPointer<TenantController>(this), windowKey]() {
        if (!self) return;
        self->managed_windows_.remove(windowKey);
    });

    managed_windows_[windowKey] = detailWindow;

    connect_dialog_close(detailDialog, detailWindow);

    show_managed_window(detailWindow, listMdiSubWindow_);
}

}

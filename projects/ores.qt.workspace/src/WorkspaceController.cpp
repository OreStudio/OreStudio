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
#include "ores.qt/WorkspaceController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QVariant>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WorkspaceContext.hpp"
#include "ores.qt/WorkspaceMdiWindow.hpp"
#include "ores.qt/WorkspaceDetailDialog.hpp"
#include "ores.qt/WorkspaceHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkspaceController::WorkspaceController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "WorkspaceController created";
}

void WorkspaceController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "workspaces");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new WorkspaceMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &WorkspaceMdiWindow::statusChanged,
            this, &WorkspaceController::statusMessage);
    connect(listWindow_, &WorkspaceMdiWindow::errorOccurred,
            this, &WorkspaceController::errorMessage);
    connect(listWindow_, &WorkspaceMdiWindow::showWorkspaceDetails,
            this, &WorkspaceController::onShowDetails);
    connect(listWindow_, &WorkspaceMdiWindow::addNewRequested,
            this, &WorkspaceController::onAddNewRequested);
    connect(listWindow_, &WorkspaceMdiWindow::showWorkspaceHistory,
            this, &WorkspaceController::onShowHistory);
    connect(listWindow_, &WorkspaceMdiWindow::workspaceActivated,
            this, &WorkspaceController::onWorkspaceActivated);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Workspaces");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Layers, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<WorkspaceController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Workspace list window created";
}

void WorkspaceController::closeAllWindows() {
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

void WorkspaceController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void WorkspaceController::onShowDetails(
    const workspace::domain::workspace& workspace) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << workspace.name;
    showDetailWindow(workspace);
}

void WorkspaceController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new workspace requested";
    showAddWindow();
}

void WorkspaceController::onWorkspaceActivated(
    const workspace::domain::workspace& ws) {

    const std::string ws_id = boost::uuids::to_string(ws.id);
    BOOST_LOG_SEV(lg(), debug) << "Workspace activation requested: " << ws_id;

    constexpr std::string_view live_id = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa";
    if (ws_id == live_id) {
        WorkspaceContext ctx;
        mdiArea_->setProperty("ores_workspace_context",
            QVariant::fromValue(ctx));
        emit statusMessage(tr("Active workspace: Live"));
        return;
    }

    QPointer<WorkspaceController> self = this;
    const std::string ws_name = ws.name;

    struct ResolveResult {
        bool success;
        std::vector<std::string> resolution_order;
        std::string error;
    };

    auto task = [self, ws_id]() -> ResolveResult {
        if (!self) return {false, {}, "Controller destroyed"};
        workspace::messaging::resolve_workspace_request request;
        request.workspace_id = ws_id;
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));
        if (!result) return {false, {}, "Failed to communicate with server"};
        return {true, result->resolution_order, {}};
    };

    auto* watcher = new QFutureWatcher<ResolveResult>(self);
    connect(watcher, &QFutureWatcher<ResolveResult>::finished,
            self, [self, watcher, ws_id, ws_name]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Resolve failed for workspace "
                                       << ws_id << ": " << result.error;
            emit self->errorMessage(QString::fromStdString(result.error));
            return;
        }

        WorkspaceContext ctx;
        ctx.id = QString::fromStdString(ws_id);
        ctx.name = QString::fromStdString(ws_name);
        ctx.resolution_order.clear();
        for (const auto& uuid_str : result.resolution_order)
            ctx.resolution_order.push_back(QString::fromStdString(uuid_str));

        self->mdiArea_->setProperty("ores_workspace_context",
            QVariant::fromValue(ctx));
        emit self->statusMessage(
            tr("Active workspace: %1").arg(QString::fromStdString(ws_name)));
    });

    QFuture<ResolveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void WorkspaceController::onShowHistory(
    const workspace::domain::workspace& workspace) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << workspace.name;
    showHistoryWindow(workspace);
}

void WorkspaceController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new workspace";

    auto* detailDialog = new WorkspaceDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &WorkspaceDetailDialog::statusMessage,
            this, &WorkspaceController::statusMessage);
    connect(detailDialog, &WorkspaceDetailDialog::errorMessage,
            this, &WorkspaceController::errorMessage);
    connect(detailDialog, &WorkspaceDetailDialog::workspaceSaved,
            this, [self = QPointer<WorkspaceController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Workspace saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Workspace");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Layers, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void WorkspaceController::showDetailWindow(
    const workspace::domain::workspace& workspace) {

    const QString identifier = QString::fromStdString(workspace.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << workspace.name;

    auto* detailDialog = new WorkspaceDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setWorkspace(workspace);

    connect(detailDialog, &WorkspaceDetailDialog::statusMessage,
            this, &WorkspaceController::statusMessage);
    connect(detailDialog, &WorkspaceDetailDialog::errorMessage,
            this, &WorkspaceController::errorMessage);
    connect(detailDialog, &WorkspaceDetailDialog::workspaceSaved,
            this, [self = QPointer<WorkspaceController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Workspace saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &WorkspaceDetailDialog::workspaceDeleted,
            this, [self = QPointer<WorkspaceController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Workspace deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Workspace: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Layers, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<WorkspaceController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void WorkspaceController::showHistoryWindow(
    const workspace::domain::workspace& workspace) {
    const QString code = QString::fromStdString(workspace.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for workspace: "
                              << workspace.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << workspace.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << workspace.name;

    auto* historyDialog = new WorkspaceHistoryDialog(
        workspace.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &WorkspaceHistoryDialog::statusChanged,
            this, [self = QPointer<WorkspaceController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &WorkspaceHistoryDialog::errorOccurred,
            this, [self = QPointer<WorkspaceController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &WorkspaceHistoryDialog::revertVersionRequested,
            this, &WorkspaceController::onRevertVersion);
    connect(historyDialog, &WorkspaceHistoryDialog::openVersionRequested,
            this, &WorkspaceController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Workspace History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<WorkspaceController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void WorkspaceController::onOpenVersion(
    const workspace::domain::workspace& workspace, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for workspace: " << workspace.name;

    const QString code = QString::fromStdString(workspace.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new WorkspaceDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setWorkspace(workspace);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &WorkspaceDetailDialog::statusMessage,
            this, [self = QPointer<WorkspaceController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &WorkspaceDetailDialog::errorMessage,
            this, [self = QPointer<WorkspaceController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Workspace: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<WorkspaceController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void WorkspaceController::onRevertVersion(
    const workspace::domain::workspace& workspace) {
    BOOST_LOG_SEV(lg(), info) << "Reverting workspace to version: "
                              << workspace.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new WorkspaceDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setWorkspace(workspace);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &WorkspaceDetailDialog::statusMessage,
            this, &WorkspaceController::statusMessage);
    connect(detailDialog, &WorkspaceDetailDialog::errorMessage,
            this, &WorkspaceController::errorMessage);
    connect(detailDialog, &WorkspaceDetailDialog::workspaceSaved,
            this, [self = QPointer<WorkspaceController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Workspace reverted: " << code.toStdString();
        emit self->statusMessage(QString("Workspace '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Workspace: %1")
        .arg(QString::fromStdString(workspace.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* WorkspaceController::listWindow() const {
    return listWindow_;
}

}

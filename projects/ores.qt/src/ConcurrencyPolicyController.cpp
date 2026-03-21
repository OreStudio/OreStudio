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
#include "ores.qt/ConcurrencyPolicyController.hpp"
#include "ores.qt/ChangeReasonCache.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ConcurrencyPolicyMdiWindow.hpp"
#include "ores.qt/ConcurrencyPolicyDetailDialog.hpp"
#include "ores.qt/ConcurrencyPolicyHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

ConcurrencyPolicyController::ConcurrencyPolicyController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      changeReasonCache_(changeReasonCache),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ConcurrencyPolicyController created";
}

void ConcurrencyPolicyController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "concurrency_policies");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ConcurrencyPolicyMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ConcurrencyPolicyMdiWindow::statusChanged,
            this, &ConcurrencyPolicyController::statusMessage);
    connect(listWindow_, &ConcurrencyPolicyMdiWindow::errorOccurred,
            this, &ConcurrencyPolicyController::errorMessage);
    connect(listWindow_, &ConcurrencyPolicyMdiWindow::showPolicyDetails,
            this, &ConcurrencyPolicyController::onShowDetails);
    connect(listWindow_, &ConcurrencyPolicyMdiWindow::addNewRequested,
            this, &ConcurrencyPolicyController::onAddNewRequested);
    connect(listWindow_, &ConcurrencyPolicyMdiWindow::showPolicyHistory,
            this, &ConcurrencyPolicyController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Concurrency Policies");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Settings, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<ConcurrencyPolicyController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Concurrency Policy list window created";
}

void ConcurrencyPolicyController::closeAllWindows() {
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

void ConcurrencyPolicyController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void ConcurrencyPolicyController::onShowDetails(
    const reporting::domain::concurrency_policy& policy) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << policy.code;
    showDetailWindow(policy);
}

void ConcurrencyPolicyController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new concurrency policy requested";
    showAddWindow();
}

void ConcurrencyPolicyController::onShowHistory(
    const reporting::domain::concurrency_policy& policy) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << policy.code;
    showHistoryWindow(QString::fromStdString(policy.code));
}

void ConcurrencyPolicyController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new concurrency policy";

    auto* detailDialog = new ConcurrencyPolicyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &ConcurrencyPolicyDetailDialog::statusMessage,
            this, &ConcurrencyPolicyController::statusMessage);
    connect(detailDialog, &ConcurrencyPolicyDetailDialog::errorMessage,
            this, &ConcurrencyPolicyController::errorMessage);
    connect(detailDialog, &ConcurrencyPolicyDetailDialog::policySaved,
            this, [self = QPointer<ConcurrencyPolicyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Concurrency Policy saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Concurrency Policy");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Settings, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ConcurrencyPolicyController::showDetailWindow(
    const reporting::domain::concurrency_policy& policy) {

    const QString identifier = QString::fromStdString(policy.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << policy.code;

    auto* detailDialog = new ConcurrencyPolicyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setPolicy(policy);

    connect(detailDialog, &ConcurrencyPolicyDetailDialog::statusMessage,
            this, &ConcurrencyPolicyController::statusMessage);
    connect(detailDialog, &ConcurrencyPolicyDetailDialog::errorMessage,
            this, &ConcurrencyPolicyController::errorMessage);
    connect(detailDialog, &ConcurrencyPolicyDetailDialog::policySaved,
            this, [self = QPointer<ConcurrencyPolicyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Concurrency Policy saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &ConcurrencyPolicyDetailDialog::policyDeleted,
            this, [self = QPointer<ConcurrencyPolicyController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Concurrency Policy deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Concurrency Policy: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Settings, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ConcurrencyPolicyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ConcurrencyPolicyController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for concurrency policy: "
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

    auto* historyDialog = new ConcurrencyPolicyHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &ConcurrencyPolicyHistoryDialog::statusChanged,
            this, [self = QPointer<ConcurrencyPolicyController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &ConcurrencyPolicyHistoryDialog::errorOccurred,
            this, [self = QPointer<ConcurrencyPolicyController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &ConcurrencyPolicyHistoryDialog::revertVersionRequested,
            this, &ConcurrencyPolicyController::onRevertVersion);
    connect(historyDialog, &ConcurrencyPolicyHistoryDialog::openVersionRequested,
            this, &ConcurrencyPolicyController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Concurrency Policy History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<ConcurrencyPolicyController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ConcurrencyPolicyController::onOpenVersion(
    const reporting::domain::concurrency_policy& policy, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for concurrency policy: " << policy.code;

    const QString code = QString::fromStdString(policy.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new ConcurrencyPolicyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setPolicy(policy);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ConcurrencyPolicyDetailDialog::statusMessage,
            this, [self = QPointer<ConcurrencyPolicyController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &ConcurrencyPolicyDetailDialog::errorMessage,
            this, [self = QPointer<ConcurrencyPolicyController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Concurrency Policy: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ConcurrencyPolicyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ConcurrencyPolicyController::onRevertVersion(
    const reporting::domain::concurrency_policy& policy) {
    BOOST_LOG_SEV(lg(), info) << "Reverting concurrency policy to version: "
                              << policy.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new ConcurrencyPolicyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setPolicy(policy);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ConcurrencyPolicyDetailDialog::statusMessage,
            this, &ConcurrencyPolicyController::statusMessage);
    connect(detailDialog, &ConcurrencyPolicyDetailDialog::errorMessage,
            this, &ConcurrencyPolicyController::errorMessage);
    connect(detailDialog, &ConcurrencyPolicyDetailDialog::policySaved,
            this, [self = QPointer<ConcurrencyPolicyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Concurrency Policy reverted: " << code.toStdString();
        emit self->statusMessage(QString("Concurrency Policy '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Concurrency Policy: %1")
        .arg(QString::fromStdString(policy.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* ConcurrencyPolicyController::listWindow() const {
    return listWindow_;
}

}

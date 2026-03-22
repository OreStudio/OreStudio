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
#include "ores.qt/BatchController.hpp"
#include "ores.qt/ChangeReasonCache.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/BatchMdiWindow.hpp"
#include "ores.qt/BatchDetailDialog.hpp"
#include "ores.qt/BatchHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.compute.api/eventing/batch_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view batch_event_name =
        eventing::domain::event_traits<
            compute::eventing::batch_changed_event>::name;
}

BatchController::BatchController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          batch_event_name, parent),
      changeReasonCache_(changeReasonCache),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BatchController created";
}

void BatchController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "batches");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BatchMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &BatchMdiWindow::statusChanged,
            this, &BatchController::statusMessage);
    connect(listWindow_, &BatchMdiWindow::errorOccurred,
            this, &BatchController::errorMessage);
    connect(listWindow_, &BatchMdiWindow::showBatchDetails,
            this, &BatchController::onShowDetails);
    connect(listWindow_, &BatchMdiWindow::addNewRequested,
            this, &BatchController::onAddNewRequested);
    connect(listWindow_, &BatchMdiWindow::showBatchHistory,
            this, &BatchController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Compute Batches");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Table, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<BatchController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Batch list window created";
}

void BatchController::closeAllWindows() {
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

void BatchController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BatchController::onShowDetails(
    const compute::domain::batch& batch) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << batch.external_ref;
    showDetailWindow(batch);
}

void BatchController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new compute batch requested";
    showAddWindow();
}

void BatchController::onShowHistory(
    const compute::domain::batch& batch) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << batch.external_ref;
    showHistoryWindow(batch);
}

void BatchController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new compute batch";

    auto* detailDialog = new BatchDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &BatchDetailDialog::statusMessage,
            this, &BatchController::statusMessage);
    connect(detailDialog, &BatchDetailDialog::errorMessage,
            this, &BatchController::errorMessage);
    connect(detailDialog, &BatchDetailDialog::batchSaved,
            this, [self = QPointer<BatchController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Batch saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Batch");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Table, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BatchController::showDetailWindow(
    const compute::domain::batch& batch) {

    const QString identifier = QString::fromStdString(batch.external_ref);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << batch.external_ref;

    auto* detailDialog = new BatchDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setBatch(batch);

    connect(detailDialog, &BatchDetailDialog::statusMessage,
            this, &BatchController::statusMessage);
    connect(detailDialog, &BatchDetailDialog::errorMessage,
            this, &BatchController::errorMessage);
    connect(detailDialog, &BatchDetailDialog::batchSaved,
            this, [self = QPointer<BatchController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Batch saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &BatchDetailDialog::batchDeleted,
            this, [self = QPointer<BatchController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Batch deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Batch: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Table, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BatchController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BatchController::showHistoryWindow(
    const compute::domain::batch& batch) {
    const QString code = QString::fromStdString(batch.external_ref);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for compute batch: "
                              << batch.external_ref;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << batch.external_ref;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << batch.external_ref;

    auto* historyDialog = new BatchHistoryDialog(
        batch.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &BatchHistoryDialog::statusChanged,
            this, [self = QPointer<BatchController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &BatchHistoryDialog::errorOccurred,
            this, [self = QPointer<BatchController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &BatchHistoryDialog::revertVersionRequested,
            this, &BatchController::onRevertVersion);
    connect(historyDialog, &BatchHistoryDialog::openVersionRequested,
            this, &BatchController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Batch History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<BatchController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BatchController::onOpenVersion(
    const compute::domain::batch& batch, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for compute batch: " << batch.external_ref;

    const QString code = QString::fromStdString(batch.external_ref);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BatchDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBatch(batch);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &BatchDetailDialog::statusMessage,
            this, [self = QPointer<BatchController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &BatchDetailDialog::errorMessage,
            this, [self = QPointer<BatchController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Batch: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BatchController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BatchController::onRevertVersion(
    const compute::domain::batch& batch) {
    BOOST_LOG_SEV(lg(), info) << "Reverting compute batch to version: "
                              << batch.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BatchDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBatch(batch);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &BatchDetailDialog::statusMessage,
            this, &BatchController::statusMessage);
    connect(detailDialog, &BatchDetailDialog::errorMessage,
            this, &BatchController::errorMessage);
    connect(detailDialog, &BatchDetailDialog::batchSaved,
            this, [self = QPointer<BatchController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Batch reverted: " << code.toStdString();
        emit self->statusMessage(QString("Batch '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Batch: %1")
        .arg(QString::fromStdString(batch.external_ref)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BatchController::listWindow() const {
    return listWindow_;
}

}

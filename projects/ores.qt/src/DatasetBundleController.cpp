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
#include "ores.qt/DatasetBundleController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/DatasetBundleMdiWindow.hpp"
#include "ores.qt/DatasetBundleDetailDialog.hpp"
#include "ores.qt/DatasetBundleHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

DatasetBundleController::DatasetBundleController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "DatasetBundleController created";
}

void DatasetBundleController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "dataset_bundles");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new DatasetBundleMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &DatasetBundleMdiWindow::statusChanged,
            this, &DatasetBundleController::statusMessage);
    connect(listWindow_, &DatasetBundleMdiWindow::errorOccurred,
            this, &DatasetBundleController::errorMessage);
    connect(listWindow_, &DatasetBundleMdiWindow::showBundleDetails,
            this, &DatasetBundleController::onShowDetails);
    connect(listWindow_, &DatasetBundleMdiWindow::addNewRequested,
            this, &DatasetBundleController::onAddNewRequested);
    connect(listWindow_, &DatasetBundleMdiWindow::showBundleHistory,
            this, &DatasetBundleController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Dataset Bundles");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Folder, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<DatasetBundleController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Dataset Bundle list window created";
}

void DatasetBundleController::closeAllWindows() {
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

void DatasetBundleController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void DatasetBundleController::onShowDetails(
    const dq::domain::dataset_bundle& bundle) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << bundle.code;
    showDetailWindow(bundle);
}

void DatasetBundleController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new dataset bundle requested";
    showAddWindow();
}

void DatasetBundleController::onShowHistory(
    const dq::domain::dataset_bundle& bundle) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << bundle.code;
    showHistoryWindow(bundle);
}

void DatasetBundleController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new dataset bundle";

    auto* detailDialog = new DatasetBundleDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &DatasetBundleDetailDialog::statusMessage,
            this, &DatasetBundleController::statusMessage);
    connect(detailDialog, &DatasetBundleDetailDialog::errorMessage,
            this, &DatasetBundleController::errorMessage);
    connect(detailDialog, &DatasetBundleDetailDialog::bundleSaved,
            this, [self = QPointer<DatasetBundleController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Dataset Bundle saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Dataset Bundle");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Folder, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DatasetBundleController::showDetailWindow(
    const dq::domain::dataset_bundle& bundle) {

    const QString identifier = QString::fromStdString(bundle.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << bundle.code;

    auto* detailDialog = new DatasetBundleDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setBundle(bundle);

    connect(detailDialog, &DatasetBundleDetailDialog::statusMessage,
            this, &DatasetBundleController::statusMessage);
    connect(detailDialog, &DatasetBundleDetailDialog::errorMessage,
            this, &DatasetBundleController::errorMessage);
    connect(detailDialog, &DatasetBundleDetailDialog::bundleSaved,
            this, [self = QPointer<DatasetBundleController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Dataset Bundle saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &DatasetBundleDetailDialog::bundleDeleted,
            this, [self = QPointer<DatasetBundleController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Dataset Bundle deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Dataset Bundle: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Folder, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<DatasetBundleController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DatasetBundleController::showHistoryWindow(
    const dq::domain::dataset_bundle& bundle) {
    const QString code = QString::fromStdString(bundle.code);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for dataset bundle: "
                              << bundle.code;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << bundle.code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << bundle.code;

    auto* historyDialog = new DatasetBundleHistoryDialog(
        bundle.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &DatasetBundleHistoryDialog::statusChanged,
            this, [self = QPointer<DatasetBundleController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &DatasetBundleHistoryDialog::errorOccurred,
            this, [self = QPointer<DatasetBundleController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &DatasetBundleHistoryDialog::revertVersionRequested,
            this, &DatasetBundleController::onRevertVersion);
    connect(historyDialog, &DatasetBundleHistoryDialog::openVersionRequested,
            this, &DatasetBundleController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Dataset Bundle History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<DatasetBundleController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void DatasetBundleController::onOpenVersion(
    const dq::domain::dataset_bundle& bundle, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for dataset bundle: " << bundle.code;

    const QString code = QString::fromStdString(bundle.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new DatasetBundleDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBundle(bundle);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &DatasetBundleDetailDialog::statusMessage,
            this, [self = QPointer<DatasetBundleController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &DatasetBundleDetailDialog::errorMessage,
            this, [self = QPointer<DatasetBundleController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Dataset Bundle: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<DatasetBundleController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void DatasetBundleController::onRevertVersion(
    const dq::domain::dataset_bundle& bundle) {
    BOOST_LOG_SEV(lg(), info) << "Reverting dataset bundle to version: "
                              << bundle.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new DatasetBundleDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBundle(bundle);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &DatasetBundleDetailDialog::statusMessage,
            this, &DatasetBundleController::statusMessage);
    connect(detailDialog, &DatasetBundleDetailDialog::errorMessage,
            this, &DatasetBundleController::errorMessage);
    connect(detailDialog, &DatasetBundleDetailDialog::bundleSaved,
            this, [self = QPointer<DatasetBundleController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Dataset Bundle reverted: " << code.toStdString();
        emit self->statusMessage(QString("Dataset Bundle '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Dataset Bundle: %1")
        .arg(QString::fromStdString(bundle.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* DatasetBundleController::listWindow() const {
    return listWindow_;
}

}

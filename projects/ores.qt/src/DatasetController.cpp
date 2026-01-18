/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/DatasetController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/DatasetMdiWindow.hpp"
#include "ores.qt/DatasetDetailDialog.hpp"
#include "ores.qt/DatasetHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

DatasetController::DatasetController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "DatasetController created";
}

DatasetController::~DatasetController() {
    BOOST_LOG_SEV(lg(), debug) << "DatasetController destroyed";
}

void DatasetController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "datasets");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new DatasetMdiWindow(clientManager_, username_);

    connect(listWindow_, &DatasetMdiWindow::statusChanged,
            this, &DatasetController::statusMessage);
    connect(listWindow_, &DatasetMdiWindow::errorOccurred,
            this, &DatasetController::errorMessage);
    connect(listWindow_, &DatasetMdiWindow::showDatasetDetails,
            this, &DatasetController::onShowDetails);
    connect(listWindow_, &DatasetMdiWindow::addNewRequested,
            this, &DatasetController::onAddNewRequested);
    connect(listWindow_, &DatasetMdiWindow::showDatasetHistory,
            this, &DatasetController::onShowHistory);

    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Datasets");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed, this, [this, key]() {
        untrack_window(key);
        listWindow_ = nullptr;
        listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Dataset list window created";
}

void DatasetController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

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

void DatasetController::reloadListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "reloadListWindow called";

    if (listWindow_) {
        listWindow_->reload();
    }
}

void DatasetController::onShowDetails(const dq::domain::dataset& dataset) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << dataset.id;
    showDetailWindow(dataset);
}

void DatasetController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new dataset requested";
    showAddWindow();
}

void DatasetController::onShowHistory(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << id;
    showHistoryWindow(id);
}

void DatasetController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new dataset";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new DatasetDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);
    detailDialog->loadLookupData();

    connect(detailDialog, &DatasetDetailDialog::statusMessage,
            this, &DatasetController::statusMessage);
    connect(detailDialog, &DatasetDetailDialog::errorMessage,
            this, &DatasetController::errorMessage);
    connect(detailDialog, &DatasetDetailDialog::datasetSaved,
            this, [this](const boost::uuids::uuid& id) {
        BOOST_LOG_SEV(lg(), info) << "Dataset saved: " << id;
        handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Dataset");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DatasetController::showDetailWindow(const dq::domain::dataset& dataset) {
    const QString identifier = QString::fromStdString(boost::uuids::to_string(dataset.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << dataset.id;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new DatasetDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->loadLookupData();
    detailDialog->setDataset(dataset);

    connect(detailDialog, &DatasetDetailDialog::statusMessage,
            this, &DatasetController::statusMessage);
    connect(detailDialog, &DatasetDetailDialog::errorMessage,
            this, &DatasetController::errorMessage);
    connect(detailDialog, &DatasetDetailDialog::datasetSaved,
            this, [this](const boost::uuids::uuid& id) {
        BOOST_LOG_SEV(lg(), info) << "Dataset saved: " << id;
        handleEntitySaved();
    });
    connect(detailDialog, &DatasetDetailDialog::datasetDeleted,
            this, [this, key](const boost::uuids::uuid& id) {
        BOOST_LOG_SEV(lg(), info) << "Dataset deleted: " << id;
        handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Dataset: %1").arg(
        QString::fromStdString(dataset.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<DatasetController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DatasetController::showHistoryWindow(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for dataset: " << id;

    const QString idStr = QString::fromStdString(boost::uuids::to_string(id));
    const QString windowKey = build_window_key("history", idStr);

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << id;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << id;
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new DatasetHistoryDialog(id, clientManager_, mainWindow_);

    connect(historyDialog, &DatasetHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &DatasetHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &DatasetHistoryDialog::revertVersionRequested,
            this, &DatasetController::onRevertVersion);
    connect(historyDialog, &DatasetHistoryDialog::openVersionRequested,
            this, &DatasetController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Dataset History: %1").arg(idStr));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<DatasetController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void DatasetController::onOpenVersion(
    const dq::domain::dataset& dataset, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for dataset: " << dataset.id;

    const QString idStr = QString::fromStdString(boost::uuids::to_string(dataset.id));
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(idStr).arg(versionNumber));

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new DatasetDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->loadLookupData();
    detailDialog->setDataset(dataset);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &DatasetDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &DatasetDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Dataset: %1 (Version %2)")
        .arg(QString::fromStdString(dataset.name)).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<DatasetController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void DatasetController::onRevertVersion(
    const dq::domain::dataset& dataset) {
    BOOST_LOG_SEV(lg(), info) << "Reverting dataset to version: "
                              << dataset.version;

    auto* detailDialog = new DatasetDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->loadLookupData();
    detailDialog->setDataset(dataset);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &DatasetDetailDialog::statusMessage,
            this, &DatasetController::statusMessage);
    connect(detailDialog, &DatasetDetailDialog::errorMessage,
            this, &DatasetController::errorMessage);
    connect(detailDialog, &DatasetDetailDialog::datasetSaved,
            this, [this](const boost::uuids::uuid& id) {
        BOOST_LOG_SEV(lg(), info) << "Dataset reverted: " << id;
        emit statusMessage(QString("Dataset reverted successfully"));
        handleEntitySaved();
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Dataset: %1")
        .arg(QString::fromStdString(dataset.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

}

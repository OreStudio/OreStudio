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
#include "ores.qt/DataDomainController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/DataDomainMdiWindow.hpp"
#include "ores.qt/DataDomainDetailDialog.hpp"
#include "ores.qt/DataDomainHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

DataDomainController::DataDomainController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "DataDomainController created";
}

DataDomainController::~DataDomainController() {
    BOOST_LOG_SEV(lg(), debug) << "DataDomainController destroyed";
}

void DataDomainController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "data_domains");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new DataDomainMdiWindow(clientManager_, username_);

    connect(listWindow_, &DataDomainMdiWindow::statusChanged,
            this, &DataDomainController::statusMessage);
    connect(listWindow_, &DataDomainMdiWindow::errorOccurred,
            this, &DataDomainController::errorMessage);
    connect(listWindow_, &DataDomainMdiWindow::showDomainDetails,
            this, &DataDomainController::onShowDetails);
    connect(listWindow_, &DataDomainMdiWindow::addNewRequested,
            this, &DataDomainController::onAddNewRequested);
    connect(listWindow_, &DataDomainMdiWindow::showDomainHistory,
            this, &DataDomainController::onShowHistory);

    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Data Domains");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_20_regular.svg", iconColor));
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

    BOOST_LOG_SEV(lg(), debug) << "Data domain list window created";
}

void DataDomainController::closeAllWindows() {
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

void DataDomainController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void DataDomainController::onShowDetails(
    const dq::domain::data_domain& domain) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << domain.name;
    showDetailWindow(domain);
}

void DataDomainController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new data domain requested";
    showAddWindow();
}

void DataDomainController::onShowHistory(const QString& name) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << name.toStdString();
    showHistoryWindow(name);
}

void DataDomainController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new data domain";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new DataDomainDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &DataDomainDetailDialog::statusMessage,
            this, &DataDomainController::statusMessage);
    connect(detailDialog, &DataDomainDetailDialog::errorMessage,
            this, &DataDomainController::errorMessage);
    connect(detailDialog, &DataDomainDetailDialog::domainSaved,
            this, [this](const QString& name) {
        BOOST_LOG_SEV(lg(), info) << "Data domain saved: " << name.toStdString();
        handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Data Domain");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DataDomainController::showDetailWindow(
    const dq::domain::data_domain& domain) {

    const QString identifier = QString::fromStdString(domain.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << domain.name;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new DataDomainDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setDomain(domain);

    connect(detailDialog, &DataDomainDetailDialog::statusMessage,
            this, &DataDomainController::statusMessage);
    connect(detailDialog, &DataDomainDetailDialog::errorMessage,
            this, &DataDomainController::errorMessage);
    connect(detailDialog, &DataDomainDetailDialog::domainSaved,
            this, [this](const QString& name) {
        BOOST_LOG_SEV(lg(), info) << "Data domain saved: " << name.toStdString();
        handleEntitySaved();
    });
    connect(detailDialog, &DataDomainDetailDialog::domainDeleted,
            this, [this, key](const QString& name) {
        BOOST_LOG_SEV(lg(), info) << "Data domain deleted: " << name.toStdString();
        handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Data Domain: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_20_regular.svg", iconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<DataDomainController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DataDomainController::showHistoryWindow(const QString& name) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for data domain: "
                              << name.toStdString();

    const QString windowKey = build_window_key("history", name);

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << name.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << name.toStdString();
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new DataDomainHistoryDialog(name, clientManager_, mainWindow_);

    connect(historyDialog, &DataDomainHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &DataDomainHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &DataDomainHistoryDialog::revertVersionRequested,
            this, &DataDomainController::onRevertVersion);
    connect(historyDialog, &DataDomainHistoryDialog::openVersionRequested,
            this, &DataDomainController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Data Domain History: %1").arg(name));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<DataDomainController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void DataDomainController::onOpenVersion(
    const dq::domain::data_domain& domain, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for data domain: " << domain.name;

    const QString name = QString::fromStdString(domain.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(name).arg(versionNumber));

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new DataDomainDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDomain(domain);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &DataDomainDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &DataDomainDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Data Domain: %1 (Version %2)")
        .arg(name).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<DataDomainController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void DataDomainController::onRevertVersion(
    const dq::domain::data_domain& domain) {
    BOOST_LOG_SEV(lg(), info) << "Reverting data domain to version: "
                              << domain.version;

    auto* detailDialog = new DataDomainDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDomain(domain);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &DataDomainDetailDialog::statusMessage,
            this, &DataDomainController::statusMessage);
    connect(detailDialog, &DataDomainDetailDialog::errorMessage,
            this, &DataDomainController::errorMessage);
    connect(detailDialog, &DataDomainDetailDialog::domainSaved,
            this, [this](const QString& name) {
        BOOST_LOG_SEV(lg(), info) << "Data domain reverted: " << name.toStdString();
        emit statusMessage(QString("Data domain '%1' reverted successfully").arg(name));
        handleEntitySaved();
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Data Domain: %1")
        .arg(QString::fromStdString(domain.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

}

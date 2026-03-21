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
#include "ores.qt/ReportInstanceController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ReportInstanceMdiWindow.hpp"
#include "ores.qt/ReportInstanceDetailDialog.hpp"
#include "ores.qt/ReportInstanceHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportInstanceController::ReportInstanceController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ReportInstanceController created";
}

void ReportInstanceController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "report_instances");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ReportInstanceMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ReportInstanceMdiWindow::statusChanged,
            this, &ReportInstanceController::statusMessage);
    connect(listWindow_, &ReportInstanceMdiWindow::errorOccurred,
            this, &ReportInstanceController::errorMessage);
    connect(listWindow_, &ReportInstanceMdiWindow::showInstanceDetails,
            this, &ReportInstanceController::onShowDetails);
    connect(listWindow_, &ReportInstanceMdiWindow::addNewRequested,
            this, &ReportInstanceController::onAddNewRequested);
    connect(listWindow_, &ReportInstanceMdiWindow::showInstanceHistory,
            this, &ReportInstanceController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Report Instances");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Record, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<ReportInstanceController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Report Instance list window created";
}

void ReportInstanceController::closeAllWindows() {
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

void ReportInstanceController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void ReportInstanceController::onShowDetails(
    const reporting::domain::report_instance& instance) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << instance.name;
    showDetailWindow(instance);
}

void ReportInstanceController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new report instance requested";
    showAddWindow();
}

void ReportInstanceController::onShowHistory(
    const reporting::domain::report_instance& instance) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << instance.name;
    showHistoryWindow(instance);
}

void ReportInstanceController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new report instance";

    auto* detailDialog = new ReportInstanceDetailDialog(mainWindow_);
    // TODO: wire changeReasonCache_
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &ReportInstanceDetailDialog::statusMessage,
            this, &ReportInstanceController::statusMessage);
    connect(detailDialog, &ReportInstanceDetailDialog::errorMessage,
            this, &ReportInstanceController::errorMessage);
    connect(detailDialog, &ReportInstanceDetailDialog::instanceSaved,
            this, [self = QPointer<ReportInstanceController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Instance saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Report Instance");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Record, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ReportInstanceController::showDetailWindow(
    const reporting::domain::report_instance& instance) {

    const QString identifier = QString::fromStdString(instance.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << instance.name;

    auto* detailDialog = new ReportInstanceDetailDialog(mainWindow_);
    // TODO: wire changeReasonCache_
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setInstance(instance);

    connect(detailDialog, &ReportInstanceDetailDialog::statusMessage,
            this, &ReportInstanceController::statusMessage);
    connect(detailDialog, &ReportInstanceDetailDialog::errorMessage,
            this, &ReportInstanceController::errorMessage);
    connect(detailDialog, &ReportInstanceDetailDialog::instanceSaved,
            this, [self = QPointer<ReportInstanceController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Instance saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &ReportInstanceDetailDialog::instanceDeleted,
            this, [self = QPointer<ReportInstanceController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Instance deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Report Instance: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Record, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ReportInstanceController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ReportInstanceController::showHistoryWindow(
    const reporting::domain::report_instance& instance) {
    const QString code = QString::fromStdString(instance.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for report instance: "
                              << instance.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << instance.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << instance.name;

    auto* historyDialog = new ReportInstanceHistoryDialog(
        instance.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &ReportInstanceHistoryDialog::statusChanged,
            this, [self = QPointer<ReportInstanceController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &ReportInstanceHistoryDialog::errorOccurred,
            this, [self = QPointer<ReportInstanceController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &ReportInstanceHistoryDialog::revertVersionRequested,
            this, &ReportInstanceController::onRevertVersion);
    connect(historyDialog, &ReportInstanceHistoryDialog::openVersionRequested,
            this, &ReportInstanceController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Report Instance History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<ReportInstanceController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ReportInstanceController::onOpenVersion(
    const reporting::domain::report_instance& instance, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for report instance: " << instance.name;

    const QString code = QString::fromStdString(instance.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new ReportInstanceDetailDialog(mainWindow_);
    // TODO: wire changeReasonCache_
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setInstance(instance);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ReportInstanceDetailDialog::statusMessage,
            this, [self = QPointer<ReportInstanceController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &ReportInstanceDetailDialog::errorMessage,
            this, [self = QPointer<ReportInstanceController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Report Instance: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ReportInstanceController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ReportInstanceController::onRevertVersion(
    const reporting::domain::report_instance& instance) {
    BOOST_LOG_SEV(lg(), info) << "Reverting report instance to version: "
                              << instance.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new ReportInstanceDetailDialog(mainWindow_);
    // TODO: wire changeReasonCache_
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setInstance(instance);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ReportInstanceDetailDialog::statusMessage,
            this, &ReportInstanceController::statusMessage);
    connect(detailDialog, &ReportInstanceDetailDialog::errorMessage,
            this, &ReportInstanceController::errorMessage);
    connect(detailDialog, &ReportInstanceDetailDialog::instanceSaved,
            this, [self = QPointer<ReportInstanceController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Instance reverted: " << code.toStdString();
        emit self->statusMessage(QString("Report Instance '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Report Instance: %1")
        .arg(QString::fromStdString(instance.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* ReportInstanceController::listWindow() const {
    return listWindow_;
}

}

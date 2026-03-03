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
#include "ores.qt/ReportTypeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ReportTypeMdiWindow.hpp"
#include "ores.qt/ReportTypeDetailDialog.hpp"
#include "ores.qt/ReportTypeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportTypeController::ReportTypeController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ReportTypeController created";
}

void ReportTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "report_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ReportTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ReportTypeMdiWindow::statusChanged,
            this, &ReportTypeController::statusMessage);
    connect(listWindow_, &ReportTypeMdiWindow::errorOccurred,
            this, &ReportTypeController::errorMessage);
    connect(listWindow_, &ReportTypeMdiWindow::showTypeDetails,
            this, &ReportTypeController::onShowDetails);
    connect(listWindow_, &ReportTypeMdiWindow::addNewRequested,
            this, &ReportTypeController::onAddNewRequested);
    connect(listWindow_, &ReportTypeMdiWindow::showTypeHistory,
            this, &ReportTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Report Types");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<ReportTypeController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Report Type list window created";
}

void ReportTypeController::closeAllWindows() {
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

void ReportTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void ReportTypeController::onShowDetails(
    const reporting::domain::report_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void ReportTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new report type requested";
    showAddWindow();
}

void ReportTypeController::onShowHistory(
    const reporting::domain::report_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void ReportTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new report type";

    auto* detailDialog = new ReportTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &ReportTypeDetailDialog::statusMessage,
            this, &ReportTypeController::statusMessage);
    connect(detailDialog, &ReportTypeDetailDialog::errorMessage,
            this, &ReportTypeController::errorMessage);
    connect(detailDialog, &ReportTypeDetailDialog::typeSaved,
            this, [self = QPointer<ReportTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Report Type");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ReportTypeController::showDetailWindow(
    const reporting::domain::report_type& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new ReportTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(type);

    connect(detailDialog, &ReportTypeDetailDialog::statusMessage,
            this, &ReportTypeController::statusMessage);
    connect(detailDialog, &ReportTypeDetailDialog::errorMessage,
            this, &ReportTypeController::errorMessage);
    connect(detailDialog, &ReportTypeDetailDialog::typeSaved,
            this, [self = QPointer<ReportTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &ReportTypeDetailDialog::typeDeleted,
            this, [self = QPointer<ReportTypeController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Type deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Report Type: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ReportTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ReportTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for report type: "
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

    auto* historyDialog = new ReportTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &ReportTypeHistoryDialog::statusChanged,
            this, [self = QPointer<ReportTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &ReportTypeHistoryDialog::errorOccurred,
            this, [self = QPointer<ReportTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &ReportTypeHistoryDialog::revertVersionRequested,
            this, &ReportTypeController::onRevertVersion);
    connect(historyDialog, &ReportTypeHistoryDialog::openVersionRequested,
            this, &ReportTypeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Report Type History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<ReportTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ReportTypeController::onOpenVersion(
    const reporting::domain::report_type& type, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for report type: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new ReportTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ReportTypeDetailDialog::statusMessage,
            this, [self = QPointer<ReportTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &ReportTypeDetailDialog::errorMessage,
            this, [self = QPointer<ReportTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Report Type: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ReportTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ReportTypeController::onRevertVersion(
    const reporting::domain::report_type& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting report type to version: "
                              << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new ReportTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ReportTypeDetailDialog::statusMessage,
            this, &ReportTypeController::statusMessage);
    connect(detailDialog, &ReportTypeDetailDialog::errorMessage,
            this, &ReportTypeController::errorMessage);
    connect(detailDialog, &ReportTypeDetailDialog::typeSaved,
            this, [self = QPointer<ReportTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Type reverted: " << code.toStdString();
        emit self->statusMessage(QString("Report Type '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Report Type: %1")
        .arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* ReportTypeController::listWindow() const {
    return listWindow_;
}

}

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
#include "ores.qt/BusinessUnitController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/BusinessUnitMdiWindow.hpp"
#include "ores.qt/BusinessUnitDetailDialog.hpp"
#include "ores.qt/BusinessUnitHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

BusinessUnitController::BusinessUnitController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BusinessUnitController created";
}

void BusinessUnitController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "business_units");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BusinessUnitMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &BusinessUnitMdiWindow::statusChanged,
            this, &BusinessUnitController::statusMessage);
    connect(listWindow_, &BusinessUnitMdiWindow::errorOccurred,
            this, &BusinessUnitController::errorMessage);
    connect(listWindow_, &BusinessUnitMdiWindow::showUnitDetails,
            this, &BusinessUnitController::onShowDetails);
    connect(listWindow_, &BusinessUnitMdiWindow::addNewRequested,
            this, &BusinessUnitController::onAddNewRequested);
    connect(listWindow_, &BusinessUnitMdiWindow::showUnitHistory,
            this, &BusinessUnitController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Business Units");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::PeopleTeam, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<BusinessUnitController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Business Unit list window created";
}

void BusinessUnitController::closeAllWindows() {
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

void BusinessUnitController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BusinessUnitController::onShowDetails(
    const refdata::domain::business_unit& business_unit) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << business_unit.unit_code;
    showDetailWindow(business_unit);
}

void BusinessUnitController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new business unit requested";
    showAddWindow();
}

void BusinessUnitController::onShowHistory(
    const refdata::domain::business_unit& business_unit) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << business_unit.unit_code;
    showHistoryWindow(business_unit);
}

void BusinessUnitController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new business unit";

    auto* detailDialog = new BusinessUnitDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &BusinessUnitDetailDialog::statusMessage,
            this, &BusinessUnitController::statusMessage);
    connect(detailDialog, &BusinessUnitDetailDialog::errorMessage,
            this, &BusinessUnitController::errorMessage);
    connect(detailDialog, &BusinessUnitDetailDialog::business_unitSaved,
            this, [self = QPointer<BusinessUnitController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Business Unit saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Business Unit");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::PeopleTeam, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BusinessUnitController::showDetailWindow(
    const refdata::domain::business_unit& business_unit) {

    const QString identifier = QString::fromStdString(business_unit.unit_code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << business_unit.unit_code;

    auto* detailDialog = new BusinessUnitDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setUnit(business_unit);

    connect(detailDialog, &BusinessUnitDetailDialog::statusMessage,
            this, &BusinessUnitController::statusMessage);
    connect(detailDialog, &BusinessUnitDetailDialog::errorMessage,
            this, &BusinessUnitController::errorMessage);
    connect(detailDialog, &BusinessUnitDetailDialog::business_unitSaved,
            this, [self = QPointer<BusinessUnitController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Business Unit saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &BusinessUnitDetailDialog::business_unitDeleted,
            this, [self = QPointer<BusinessUnitController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Business Unit deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Business Unit: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::PeopleTeam, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BusinessUnitController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BusinessUnitController::showHistoryWindow(
    const refdata::domain::business_unit& business_unit) {
    const QString code = QString::fromStdString(business_unit.unit_code);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for business unit: "
                              << business_unit.unit_code;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << business_unit.unit_code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << business_unit.unit_code;

    auto* historyDialog = new BusinessUnitHistoryDialog(
        business_unit.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &BusinessUnitHistoryDialog::statusChanged,
            this, [self = QPointer<BusinessUnitController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &BusinessUnitHistoryDialog::errorOccurred,
            this, [self = QPointer<BusinessUnitController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &BusinessUnitHistoryDialog::revertVersionRequested,
            this, &BusinessUnitController::onRevertVersion);
    connect(historyDialog, &BusinessUnitHistoryDialog::openVersionRequested,
            this, &BusinessUnitController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Business Unit History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<BusinessUnitController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BusinessUnitController::onOpenVersion(
    const refdata::domain::business_unit& business_unit, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for business unit: " << business_unit.unit_code;

    const QString code = QString::fromStdString(business_unit.unit_code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BusinessUnitDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setUnit(business_unit);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &BusinessUnitDetailDialog::statusMessage,
            this, [self = QPointer<BusinessUnitController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &BusinessUnitDetailDialog::errorMessage,
            this, [self = QPointer<BusinessUnitController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Business Unit: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BusinessUnitController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BusinessUnitController::onRevertVersion(
    const refdata::domain::business_unit& business_unit) {
    BOOST_LOG_SEV(lg(), info) << "Reverting business unit to version: "
                              << business_unit.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BusinessUnitDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setUnit(business_unit);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &BusinessUnitDetailDialog::statusMessage,
            this, &BusinessUnitController::statusMessage);
    connect(detailDialog, &BusinessUnitDetailDialog::errorMessage,
            this, &BusinessUnitController::errorMessage);
    connect(detailDialog, &BusinessUnitDetailDialog::business_unitSaved,
            this, [self = QPointer<BusinessUnitController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Business Unit reverted: " << code.toStdString();
        emit self->statusMessage(QString("Business Unit '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Business Unit: %1")
        .arg(QString::fromStdString(business_unit.unit_code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BusinessUnitController::listWindow() const {
    return listWindow_;
}

}

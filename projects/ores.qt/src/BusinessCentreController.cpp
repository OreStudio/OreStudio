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
#include "ores.qt/BusinessCentreController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/BusinessCentreMdiWindow.hpp"
#include "ores.qt/BusinessCentreDetailDialog.hpp"
#include "ores.qt/BusinessCentreHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

BusinessCentreController::BusinessCentreController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BusinessCentreController created";
}

void BusinessCentreController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "business_centres");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BusinessCentreMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &BusinessCentreMdiWindow::statusChanged,
            this, &BusinessCentreController::statusMessage);
    connect(listWindow_, &BusinessCentreMdiWindow::errorOccurred,
            this, &BusinessCentreController::errorMessage);
    connect(listWindow_, &BusinessCentreMdiWindow::showBusinessCentreDetails,
            this, &BusinessCentreController::onShowDetails);
    connect(listWindow_, &BusinessCentreMdiWindow::addNewRequested,
            this, &BusinessCentreController::onAddNewRequested);
    connect(listWindow_, &BusinessCentreMdiWindow::showBusinessCentreHistory,
            this, &BusinessCentreController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Business Centres");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Building, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<BusinessCentreController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Business centre list window created";
}

void BusinessCentreController::closeAllWindows() {
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

void BusinessCentreController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BusinessCentreController::onShowDetails(
    const refdata::domain::business_centre& business_centre) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << business_centre.code;
    showDetailWindow(business_centre);
}

void BusinessCentreController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new business centre requested";
    showAddWindow();
}

void BusinessCentreController::onShowHistory(
    const refdata::domain::business_centre& business_centre) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << business_centre.code;
    showHistoryWindow(business_centre);
}

void BusinessCentreController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new business centre";

    auto* detailDialog = new BusinessCentreDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &BusinessCentreDetailDialog::statusMessage,
            this, &BusinessCentreController::statusMessage);
    connect(detailDialog, &BusinessCentreDetailDialog::errorMessage,
            this, &BusinessCentreController::errorMessage);
    connect(detailDialog, &BusinessCentreDetailDialog::businessCentreSaved,
            this, [self = QPointer<BusinessCentreController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Business centre saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Business Centre");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Building, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BusinessCentreController::showDetailWindow(
    const refdata::domain::business_centre& business_centre) {

    const QString identifier = QString::fromStdString(business_centre.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << business_centre.code;

    auto* detailDialog = new BusinessCentreDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setBusinessCentre(business_centre);

    connect(detailDialog, &BusinessCentreDetailDialog::statusMessage,
            this, &BusinessCentreController::statusMessage);
    connect(detailDialog, &BusinessCentreDetailDialog::errorMessage,
            this, &BusinessCentreController::errorMessage);
    connect(detailDialog, &BusinessCentreDetailDialog::businessCentreSaved,
            this, [self = QPointer<BusinessCentreController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Business centre saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &BusinessCentreDetailDialog::businessCentreDeleted,
            this, [self = QPointer<BusinessCentreController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Business centre deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Business Centre: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Building, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BusinessCentreController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BusinessCentreController::showHistoryWindow(
    const refdata::domain::business_centre& business_centre) {
    const QString code = QString::fromStdString(business_centre.code);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for business centre: "
                              << business_centre.code;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << business_centre.code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << business_centre.code;

    auto* historyDialog = new BusinessCentreHistoryDialog(
        code, clientManager_, mainWindow_);

    connect(historyDialog, &BusinessCentreHistoryDialog::statusChanged,
            this, [self = QPointer<BusinessCentreController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &BusinessCentreHistoryDialog::errorOccurred,
            this, [self = QPointer<BusinessCentreController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &BusinessCentreHistoryDialog::revertVersionRequested,
            this, &BusinessCentreController::onRevertVersion);
    connect(historyDialog, &BusinessCentreHistoryDialog::openVersionRequested,
            this, &BusinessCentreController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Business Centre History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<BusinessCentreController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BusinessCentreController::onOpenVersion(
    const refdata::domain::business_centre& business_centre, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for business centre: " << business_centre.code;

    const QString code = QString::fromStdString(business_centre.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BusinessCentreDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBusinessCentre(business_centre);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &BusinessCentreDetailDialog::statusMessage,
            this, [self = QPointer<BusinessCentreController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &BusinessCentreDetailDialog::errorMessage,
            this, [self = QPointer<BusinessCentreController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Business Centre: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BusinessCentreController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BusinessCentreController::onRevertVersion(
    const refdata::domain::business_centre& business_centre) {
    BOOST_LOG_SEV(lg(), info) << "Reverting business centre to version: "
                              << business_centre.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BusinessCentreDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBusinessCentre(business_centre);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &BusinessCentreDetailDialog::statusMessage,
            this, &BusinessCentreController::statusMessage);
    connect(detailDialog, &BusinessCentreDetailDialog::errorMessage,
            this, &BusinessCentreController::errorMessage);
    connect(detailDialog, &BusinessCentreDetailDialog::businessCentreSaved,
            this, [self = QPointer<BusinessCentreController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Business centre reverted: " << code.toStdString();
        emit self->statusMessage(QString("Business centre '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Business Centre: %1")
        .arg(QString::fromStdString(business_centre.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BusinessCentreController::listWindow() const {
    return listWindow_;
}

}

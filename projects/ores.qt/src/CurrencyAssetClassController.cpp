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
#include "ores.qt/CurrencyAssetClassController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/CurrencyAssetClassMdiWindow.hpp"
#include "ores.qt/CurrencyAssetClassDetailDialog.hpp"
#include "ores.qt/CurrencyAssetClassHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

CurrencyAssetClassController::CurrencyAssetClassController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CurrencyAssetClassController created";
}

void CurrencyAssetClassController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "currency_asset_classes");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CurrencyAssetClassMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &CurrencyAssetClassMdiWindow::statusChanged,
            this, &CurrencyAssetClassController::statusMessage);
    connect(listWindow_, &CurrencyAssetClassMdiWindow::errorOccurred,
            this, &CurrencyAssetClassController::errorMessage);
    connect(listWindow_, &CurrencyAssetClassMdiWindow::showClassDetails,
            this, &CurrencyAssetClassController::onShowDetails);
    connect(listWindow_, &CurrencyAssetClassMdiWindow::addNewRequested,
            this, &CurrencyAssetClassController::onAddNewRequested);
    connect(listWindow_, &CurrencyAssetClassMdiWindow::showClassHistory,
            this, &CurrencyAssetClassController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Currency Asset Classes");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Classification, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<CurrencyAssetClassController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Currency Asset Class list window created";
}

void CurrencyAssetClassController::closeAllWindows() {
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

void CurrencyAssetClassController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CurrencyAssetClassController::onShowDetails(
    const refdata::domain::currency_asset_class& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void CurrencyAssetClassController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new currency asset class requested";
    showAddWindow();
}

void CurrencyAssetClassController::onShowHistory(
    const refdata::domain::currency_asset_class& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void CurrencyAssetClassController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new currency asset class";

    auto* detailDialog = new CurrencyAssetClassDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &CurrencyAssetClassDetailDialog::statusMessage,
            this, &CurrencyAssetClassController::statusMessage);
    connect(detailDialog, &CurrencyAssetClassDetailDialog::errorMessage,
            this, &CurrencyAssetClassController::errorMessage);
    connect(detailDialog, &CurrencyAssetClassDetailDialog::typeSaved,
            this, [self = QPointer<CurrencyAssetClassController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Currency Asset Class saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Currency Asset Class");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Classification, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyAssetClassController::showDetailWindow(
    const refdata::domain::currency_asset_class& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new CurrencyAssetClassDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setClass(type);

    connect(detailDialog, &CurrencyAssetClassDetailDialog::statusMessage,
            this, &CurrencyAssetClassController::statusMessage);
    connect(detailDialog, &CurrencyAssetClassDetailDialog::errorMessage,
            this, &CurrencyAssetClassController::errorMessage);
    connect(detailDialog, &CurrencyAssetClassDetailDialog::typeSaved,
            this, [self = QPointer<CurrencyAssetClassController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Currency Asset Class saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &CurrencyAssetClassDetailDialog::typeDeleted,
            this, [self = QPointer<CurrencyAssetClassController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Currency Asset Class deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency Asset Class: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Classification, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CurrencyAssetClassController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyAssetClassController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for currency asset class: "
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

    auto* historyDialog = new CurrencyAssetClassHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &CurrencyAssetClassHistoryDialog::statusChanged,
            this, [self = QPointer<CurrencyAssetClassController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &CurrencyAssetClassHistoryDialog::errorOccurred,
            this, [self = QPointer<CurrencyAssetClassController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &CurrencyAssetClassHistoryDialog::revertVersionRequested,
            this, &CurrencyAssetClassController::onRevertVersion);
    connect(historyDialog, &CurrencyAssetClassHistoryDialog::openVersionRequested,
            this, &CurrencyAssetClassController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Currency Asset Class History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<CurrencyAssetClassController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CurrencyAssetClassController::onOpenVersion(
    const refdata::domain::currency_asset_class& type, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for currency asset class: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CurrencyAssetClassDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setClass(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &CurrencyAssetClassDetailDialog::statusMessage,
            this, [self = QPointer<CurrencyAssetClassController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &CurrencyAssetClassDetailDialog::errorMessage,
            this, [self = QPointer<CurrencyAssetClassController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency Asset Class: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CurrencyAssetClassController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CurrencyAssetClassController::onRevertVersion(
    const refdata::domain::currency_asset_class& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting currency asset class to version: "
                              << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CurrencyAssetClassDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setClass(type);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &CurrencyAssetClassDetailDialog::statusMessage,
            this, &CurrencyAssetClassController::statusMessage);
    connect(detailDialog, &CurrencyAssetClassDetailDialog::errorMessage,
            this, &CurrencyAssetClassController::errorMessage);
    connect(detailDialog, &CurrencyAssetClassDetailDialog::typeSaved,
            this, [self = QPointer<CurrencyAssetClassController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Currency Asset Class reverted: " << code.toStdString();
        emit self->statusMessage(QString("Currency Asset Class '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Currency Asset Class: %1")
        .arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CurrencyAssetClassController::listWindow() const {
    return listWindow_;
}

}

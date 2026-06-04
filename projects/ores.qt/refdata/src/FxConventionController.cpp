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
#include "ores.qt/FxConventionController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FxConventionDetailDialog.hpp"
#include "ores.qt/FxConventionHistoryDialog.hpp"
#include "ores.qt/FxConventionMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

FxConventionController::FxConventionController(QMainWindow* mainWindow,
                                               QMdiArea* mdiArea,
                                               ClientManager* clientManager,
                                               const QString& username,
                                               QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, std::string_view{}, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "FxConventionController created";
}

void FxConventionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "fx_conventions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new FxConventionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &FxConventionMdiWindow::statusChanged,
            this,
            &FxConventionController::statusMessage);
    connect(listWindow_,
            &FxConventionMdiWindow::errorOccurred,
            this,
            &FxConventionController::errorMessage);
    connect(listWindow_,
            &FxConventionMdiWindow::showConventionDetails,
            this,
            &FxConventionController::onShowDetails);
    connect(listWindow_,
            &FxConventionMdiWindow::addNewRequested,
            this,
            &FxConventionController::onAddNewRequested);
    connect(listWindow_,
            &FxConventionMdiWindow::showConventionHistory,
            this,
            &FxConventionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("FX Conventions");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<FxConventionController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "FX Convention list window created";
}

void FxConventionController::closeAllWindows() {
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

void FxConventionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void FxConventionController::onShowDetails(const refdata::domain::fx_convention& fxc) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << fxc.id;
    showDetailWindow(fxc);
}

void FxConventionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new FX convention requested";
    showAddWindow();
}

void FxConventionController::onShowHistory(const refdata::domain::fx_convention& fxc) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << fxc.id;
    showHistoryWindow(QString::fromStdString(fxc.id));
}

void FxConventionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new FX convention";

    auto* detailDialog = new FxConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &FxConventionDetailDialog::statusMessage,
            this,
            &FxConventionController::statusMessage);
    connect(detailDialog,
            &FxConventionDetailDialog::errorMessage,
            this,
            &FxConventionController::errorMessage);
    connect(detailDialog,
            &FxConventionDetailDialog::fxcSaved,
            this,
            [self = QPointer<FxConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "FX Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New FX Convention");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FxConventionController::showDetailWindow(const refdata::domain::fx_convention& fxc) {

    const QString identifier = QString::fromStdString(fxc.id);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << fxc.id;

    auto* detailDialog = new FxConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConvention(fxc);

    connect(detailDialog,
            &FxConventionDetailDialog::statusMessage,
            this,
            &FxConventionController::statusMessage);
    connect(detailDialog,
            &FxConventionDetailDialog::errorMessage,
            this,
            &FxConventionController::errorMessage);
    connect(detailDialog,
            &FxConventionDetailDialog::fxcSaved,
            this,
            [self = QPointer<FxConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "FX Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &FxConventionDetailDialog::fxcDeleted,
            this,
            [self = QPointer<FxConventionController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "FX Convention deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("FX Convention: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<FxConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FxConventionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for FX convention: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new FxConventionHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &FxConventionHistoryDialog::statusChanged,
            this,
            [self = QPointer<FxConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &FxConventionHistoryDialog::errorOccurred,
            this,
            [self = QPointer<FxConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &FxConventionHistoryDialog::revertVersionRequested,
            this,
            &FxConventionController::onRevertVersion);
    connect(historyDialog,
            &FxConventionHistoryDialog::openVersionRequested,
            this,
            &FxConventionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("FX Convention History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<FxConventionController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void FxConventionController::onOpenVersion(const refdata::domain::fx_convention& fxc,
                                           int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for FX convention: " << fxc.id;

    const QString code = QString::fromStdString(fxc.id);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new FxConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(fxc);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &FxConventionDetailDialog::statusMessage,
            this,
            [self = QPointer<FxConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &FxConventionDetailDialog::errorMessage,
            this,
            [self = QPointer<FxConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("FX Convention: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<FxConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void FxConventionController::onRevertVersion(const refdata::domain::fx_convention& fxc) {
    BOOST_LOG_SEV(lg(), info) << "Reverting FX convention to version: " << fxc.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new FxConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(fxc);
    detailDialog->setCreateMode(false);

    connect(detailDialog,
            &FxConventionDetailDialog::statusMessage,
            this,
            &FxConventionController::statusMessage);
    connect(detailDialog,
            &FxConventionDetailDialog::errorMessage,
            this,
            &FxConventionController::errorMessage);
    connect(detailDialog,
            &FxConventionDetailDialog::fxcSaved,
            this,
            [self = QPointer<FxConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "FX Convention reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("FX Convention '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert FX Convention: %1").arg(QString::fromStdString(fxc.id)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* FxConventionController::listWindow() const {
    return listWindow_;
}

}

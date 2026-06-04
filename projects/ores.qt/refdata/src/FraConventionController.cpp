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
#include "ores.qt/FraConventionController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FraConventionDetailDialog.hpp"
#include "ores.qt/FraConventionHistoryDialog.hpp"
#include "ores.qt/FraConventionMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

FraConventionController::FraConventionController(QMainWindow* mainWindow,
                                                 QMdiArea* mdiArea,
                                                 ClientManager* clientManager,
                                                 const QString& username,
                                                 QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, std::string_view{}, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "FraConventionController created";
}

void FraConventionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "fra_conventions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new FraConventionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &FraConventionMdiWindow::statusChanged,
            this,
            &FraConventionController::statusMessage);
    connect(listWindow_,
            &FraConventionMdiWindow::errorOccurred,
            this,
            &FraConventionController::errorMessage);
    connect(listWindow_,
            &FraConventionMdiWindow::showConventionDetails,
            this,
            &FraConventionController::onShowDetails);
    connect(listWindow_,
            &FraConventionMdiWindow::addNewRequested,
            this,
            &FraConventionController::onAddNewRequested);
    connect(listWindow_,
            &FraConventionMdiWindow::showConventionHistory,
            this,
            &FraConventionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("FRA Conventions");
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
            [self = QPointer<FraConventionController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "FRA Convention list window created";
}

void FraConventionController::closeAllWindows() {
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

void FraConventionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void FraConventionController::onShowDetails(const refdata::domain::fra_convention& fc) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << fc.id;
    showDetailWindow(fc);
}

void FraConventionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new FRA convention requested";
    showAddWindow();
}

void FraConventionController::onShowHistory(const refdata::domain::fra_convention& fc) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << fc.id;
    showHistoryWindow(QString::fromStdString(fc.id));
}

void FraConventionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new FRA convention";

    auto* detailDialog = new FraConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &FraConventionDetailDialog::statusMessage,
            this,
            &FraConventionController::statusMessage);
    connect(detailDialog,
            &FraConventionDetailDialog::errorMessage,
            this,
            &FraConventionController::errorMessage);
    connect(detailDialog,
            &FraConventionDetailDialog::fcSaved,
            this,
            [self = QPointer<FraConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "FRA Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New FRA Convention");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FraConventionController::showDetailWindow(const refdata::domain::fra_convention& fc) {

    const QString identifier = QString::fromStdString(fc.id);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << fc.id;

    auto* detailDialog = new FraConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConvention(fc);

    connect(detailDialog,
            &FraConventionDetailDialog::statusMessage,
            this,
            &FraConventionController::statusMessage);
    connect(detailDialog,
            &FraConventionDetailDialog::errorMessage,
            this,
            &FraConventionController::errorMessage);
    connect(detailDialog,
            &FraConventionDetailDialog::fcSaved,
            this,
            [self = QPointer<FraConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "FRA Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &FraConventionDetailDialog::fcDeleted,
            this,
            [self = QPointer<FraConventionController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "FRA Convention deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("FRA Convention: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<FraConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FraConventionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for FRA convention: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new FraConventionHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &FraConventionHistoryDialog::statusChanged,
            this,
            [self = QPointer<FraConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &FraConventionHistoryDialog::errorOccurred,
            this,
            [self = QPointer<FraConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &FraConventionHistoryDialog::revertVersionRequested,
            this,
            &FraConventionController::onRevertVersion);
    connect(historyDialog,
            &FraConventionHistoryDialog::openVersionRequested,
            this,
            &FraConventionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("FRA Convention History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<FraConventionController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void FraConventionController::onOpenVersion(const refdata::domain::fra_convention& fc,
                                            int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for FRA convention: " << fc.id;

    const QString code = QString::fromStdString(fc.id);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new FraConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(fc);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &FraConventionDetailDialog::statusMessage,
            this,
            [self = QPointer<FraConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &FraConventionDetailDialog::errorMessage,
            this,
            [self = QPointer<FraConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("FRA Convention: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<FraConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void FraConventionController::onRevertVersion(const refdata::domain::fra_convention& fc) {
    BOOST_LOG_SEV(lg(), info) << "Reverting FRA convention to version: " << fc.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new FraConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(fc);
    detailDialog->setCreateMode(false);

    connect(detailDialog,
            &FraConventionDetailDialog::statusMessage,
            this,
            &FraConventionController::statusMessage);
    connect(detailDialog,
            &FraConventionDetailDialog::errorMessage,
            this,
            &FraConventionController::errorMessage);
    connect(detailDialog,
            &FraConventionDetailDialog::fcSaved,
            this,
            [self = QPointer<FraConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "FRA Convention reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("FRA Convention '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert FRA Convention: %1").arg(QString::fromStdString(fc.id)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* FraConventionController::listWindow() const {
    return listWindow_;
}

}

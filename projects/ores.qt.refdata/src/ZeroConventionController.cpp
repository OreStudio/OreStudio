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
#include "ores.qt/ZeroConventionController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ZeroConventionMdiWindow.hpp"
#include "ores.qt/ZeroConventionDetailDialog.hpp"
#include "ores.qt/ZeroConventionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

ZeroConventionController::ZeroConventionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ZeroConventionController created";
}

void ZeroConventionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "zero_conventions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ZeroConventionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ZeroConventionMdiWindow::statusChanged,
            this, &ZeroConventionController::statusMessage);
    connect(listWindow_, &ZeroConventionMdiWindow::errorOccurred,
            this, &ZeroConventionController::errorMessage);
    connect(listWindow_, &ZeroConventionMdiWindow::showConventionDetails,
            this, &ZeroConventionController::onShowDetails);
    connect(listWindow_, &ZeroConventionMdiWindow::addNewRequested,
            this, &ZeroConventionController::onAddNewRequested);
    connect(listWindow_, &ZeroConventionMdiWindow::showConventionHistory,
            this, &ZeroConventionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Zero Conventions");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Curve, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<ZeroConventionController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Zero Convention list window created";
}

void ZeroConventionController::closeAllWindows() {
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

void ZeroConventionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void ZeroConventionController::onShowDetails(
    const refdata::domain::zero_convention& zc) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << zc.id;
    showDetailWindow(zc);
}

void ZeroConventionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new zero convention requested";
    showAddWindow();
}

void ZeroConventionController::onShowHistory(
    const refdata::domain::zero_convention& zc) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << zc.id;
    showHistoryWindow(QString::fromStdString(zc.id));
}

void ZeroConventionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new zero convention";

    auto* detailDialog = new ZeroConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &ZeroConventionDetailDialog::statusMessage,
            this, &ZeroConventionController::statusMessage);
    connect(detailDialog, &ZeroConventionDetailDialog::errorMessage,
            this, &ZeroConventionController::errorMessage);
    connect(detailDialog, &ZeroConventionDetailDialog::zcSaved,
            this, [self = QPointer<ZeroConventionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Zero Convention saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Zero Convention");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Curve, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ZeroConventionController::showDetailWindow(
    const refdata::domain::zero_convention& zc) {

    const QString identifier = QString::fromStdString(zc.id);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << zc.id;

    auto* detailDialog = new ZeroConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConvention(zc);

    connect(detailDialog, &ZeroConventionDetailDialog::statusMessage,
            this, &ZeroConventionController::statusMessage);
    connect(detailDialog, &ZeroConventionDetailDialog::errorMessage,
            this, &ZeroConventionController::errorMessage);
    connect(detailDialog, &ZeroConventionDetailDialog::zcSaved,
            this, [self = QPointer<ZeroConventionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Zero Convention saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &ZeroConventionDetailDialog::zcDeleted,
            this, [self = QPointer<ZeroConventionController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Zero Convention deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Zero Convention: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Curve, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ZeroConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ZeroConventionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for zero convention: "
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

    auto* historyDialog = new ZeroConventionHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &ZeroConventionHistoryDialog::statusChanged,
            this, [self = QPointer<ZeroConventionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &ZeroConventionHistoryDialog::errorOccurred,
            this, [self = QPointer<ZeroConventionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &ZeroConventionHistoryDialog::revertVersionRequested,
            this, &ZeroConventionController::onRevertVersion);
    connect(historyDialog, &ZeroConventionHistoryDialog::openVersionRequested,
            this, &ZeroConventionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Zero Convention History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<ZeroConventionController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ZeroConventionController::onOpenVersion(
    const refdata::domain::zero_convention& zc, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for zero convention: " << zc.id;

    const QString code = QString::fromStdString(zc.id);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new ZeroConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(zc);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ZeroConventionDetailDialog::statusMessage,
            this, [self = QPointer<ZeroConventionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &ZeroConventionDetailDialog::errorMessage,
            this, [self = QPointer<ZeroConventionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Zero Convention: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ZeroConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ZeroConventionController::onRevertVersion(
    const refdata::domain::zero_convention& zc) {
    BOOST_LOG_SEV(lg(), info) << "Reverting zero convention to version: "
                              << zc.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new ZeroConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(zc);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ZeroConventionDetailDialog::statusMessage,
            this, &ZeroConventionController::statusMessage);
    connect(detailDialog, &ZeroConventionDetailDialog::errorMessage,
            this, &ZeroConventionController::errorMessage);
    connect(detailDialog, &ZeroConventionDetailDialog::zcSaved,
            this, [self = QPointer<ZeroConventionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Zero Convention reverted: " << code.toStdString();
        emit self->statusMessage(QString("Zero Convention '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Zero Convention: %1")
        .arg(QString::fromStdString(zc.id)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* ZeroConventionController::listWindow() const {
    return listWindow_;
}

}

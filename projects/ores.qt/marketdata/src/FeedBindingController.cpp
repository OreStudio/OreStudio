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
#include "ores.qt/FeedBindingController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FeedBindingDetailDialog.hpp"
#include "ores.qt/FeedBindingHistoryDialog.hpp"
#include "ores.qt/FeedBindingMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

FeedBindingController::FeedBindingController(QMainWindow* mainWindow,
                                             QMdiArea* mdiArea,
                                             ClientManager* clientManager,
                                             const QString& username,
                                             QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, std::string_view{}, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "FeedBindingController created";
}

void FeedBindingController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "feed_bindings");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new FeedBindingMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &FeedBindingMdiWindow::statusChanged,
            this,
            &FeedBindingController::statusMessage);
    connect(listWindow_,
            &FeedBindingMdiWindow::errorOccurred,
            this,
            &FeedBindingController::errorMessage);
    connect(listWindow_,
            &FeedBindingMdiWindow::showBindingDetails,
            this,
            &FeedBindingController::onShowDetails);
    connect(listWindow_,
            &FeedBindingMdiWindow::addNewRequested,
            this,
            &FeedBindingController::onAddNewRequested);
    connect(listWindow_,
            &FeedBindingMdiWindow::showBindingHistory,
            this,
            &FeedBindingController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<FeedBindingController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Feed Binding list window created";
}

void FeedBindingController::closeAllWindows() {
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

void FeedBindingController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void FeedBindingController::onShowDetails(const&) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " <<.;
    showDetailWindow();
}

void FeedBindingController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new feed binding requested";
    showAddWindow();
}

void FeedBindingController::onShowHistory(const&) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " <<.;
    showHistoryWindow(QString::fromStdString(.));
}

void FeedBindingController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new feed binding";

    auto* detailDialog = new FeedBindingDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &FeedBindingDetailDialog::statusMessage,
            this,
            &FeedBindingController::statusMessage);
    connect(detailDialog,
            &FeedBindingDetailDialog::errorMessage,
            this,
            &FeedBindingController::errorMessage);
    connect(detailDialog,
            &FeedBindingDetailDialog::Saved,
            this,
            [self = QPointer<FeedBindingController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Feed Binding saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Feed Binding");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FeedBindingController::showDetailWindow(const&) {

    const QString identifier = QString::fromStdString(.);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " <<.;

    auto* detailDialog = new FeedBindingDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setBinding();

    connect(detailDialog,
            &FeedBindingDetailDialog::statusMessage,
            this,
            &FeedBindingController::statusMessage);
    connect(detailDialog,
            &FeedBindingDetailDialog::errorMessage,
            this,
            &FeedBindingController::errorMessage);
    connect(detailDialog,
            &FeedBindingDetailDialog::Saved,
            this,
            [self = QPointer<FeedBindingController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Feed Binding saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &FeedBindingDetailDialog::Deleted,
            this,
            [self = QPointer<FeedBindingController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Feed Binding deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Feed Binding: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<FeedBindingController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FeedBindingController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for feed binding: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new FeedBindingHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &FeedBindingHistoryDialog::statusChanged,
            this,
            [self = QPointer<FeedBindingController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &FeedBindingHistoryDialog::errorOccurred,
            this,
            [self = QPointer<FeedBindingController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &FeedBindingHistoryDialog::revertVersionRequested,
            this,
            &FeedBindingController::onRevertVersion);
    connect(historyDialog,
            &FeedBindingHistoryDialog::openVersionRequested,
            this,
            &FeedBindingController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Feed Binding History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<FeedBindingController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void FeedBindingController::onOpenVersion(const&, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for feed binding: " <<.;

    const QString code = QString::fromStdString(.);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new FeedBindingDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBinding();
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &FeedBindingDetailDialog::statusMessage,
            this,
            [self = QPointer<FeedBindingController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &FeedBindingDetailDialog::errorMessage,
            this,
            [self = QPointer<FeedBindingController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Feed Binding: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<FeedBindingController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void FeedBindingController::onRevertVersion(const&) {
    BOOST_LOG_SEV(lg(), info) << "Reverting feed binding to version: " <<.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new FeedBindingDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_ = ;
    reverted_.version = 0;
    detailDialog->setBinding(reverted_);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &FeedBindingDetailDialog::statusMessage,
            this,
            &FeedBindingController::statusMessage);
    connect(detailDialog,
            &FeedBindingDetailDialog::errorMessage,
            this,
            &FeedBindingController::errorMessage);
    connect(detailDialog,
            &FeedBindingDetailDialog::Saved,
            this,
            [self = QPointer<FeedBindingController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Feed Binding reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Feed Binding '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Feed Binding: %1").arg(QString::fromStdString(.)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* FeedBindingController::listWindow() const {
    return listWindow_;
}

}

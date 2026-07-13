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
#include "ores.qt/TenorController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.marketdata.api/eventing/tenor_changed_event.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TenorDetailDialog.hpp"
#include "ores.qt/TenorHistoryDialog.hpp"
#include "ores.qt/TenorMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view tenor_event_name =
    eventing::domain::event_traits<marketdata::eventing::tenor_changed_event>::name;
}

TenorController::TenorController(QMainWindow* mainWindow,
                                 QMdiArea* mdiArea,
                                 ClientManager* clientManager,
                                 const QString& username,
                                 QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, tenor_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "TenorController created";
}

void TenorController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "tenors");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new TenorMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &TenorMdiWindow::statusChanged, this, &TenorController::statusMessage);
    connect(listWindow_, &TenorMdiWindow::errorOccurred, this, &TenorController::errorMessage);
    connect(listWindow_, &TenorMdiWindow::showTenorDetails, this, &TenorController::onShowDetails);
    connect(
        listWindow_, &TenorMdiWindow::addNewRequested, this, &TenorController::onAddNewRequested);
    connect(listWindow_, &TenorMdiWindow::showTenorHistory, this, &TenorController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Tenors");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor));
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
            [self = QPointer<TenorController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Tenor list window created";
}

void TenorController::closeAllWindows() {
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

void TenorController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TenorController::onShowDetails(const marketdata::domain::tenor& tenor) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << tenor.code;
    showDetailWindow(tenor);
}

void TenorController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new tenor requested";
    showAddWindow();
}


void TenorController::onShowHistory(const marketdata::domain::tenor& tenor) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << tenor.code;
    showHistoryWindow(QString::fromStdString(tenor.code));
}

void TenorController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new tenor";

    auto* detailDialog = new TenorDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &TenorDetailDialog::statusMessage, this, &TenorController::statusMessage);
    connect(detailDialog, &TenorDetailDialog::errorMessage, this, &TenorController::errorMessage);
    connect(detailDialog,
            &TenorDetailDialog::tenorSaved,
            this,
            [self = QPointer<TenorController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Tenor");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenorController::showDetailWindow(const marketdata::domain::tenor& tenor) {

    const QString identifier = QString::fromStdString(tenor.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << tenor.code;

    auto* detailDialog = new TenorDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setTenor(tenor);

    connect(detailDialog, &TenorDetailDialog::statusMessage, this, &TenorController::statusMessage);
    connect(detailDialog, &TenorDetailDialog::errorMessage, this, &TenorController::errorMessage);
    connect(detailDialog,
            &TenorDetailDialog::tenorSaved,
            this,
            [self = QPointer<TenorController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &TenorDetailDialog::tenorDeleted,
            this,
            [self = QPointer<TenorController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenor: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<TenorController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenorController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for tenor: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new TenorHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &TenorHistoryDialog::statusChanged,
            this,
            [self = QPointer<TenorController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &TenorHistoryDialog::errorOccurred,
            this,
            [self = QPointer<TenorController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &TenorHistoryDialog::revertVersionRequested,
            this,
            &TenorController::onRevertVersion);
    connect(historyDialog,
            &TenorHistoryDialog::openVersionRequested,
            this,
            &TenorController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Tenor History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<TenorController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void TenorController::onOpenVersion(const marketdata::domain::tenor& tenor, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for tenor: " << tenor.code;

    const QString code = QString::fromStdString(tenor.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new TenorDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setTenor(tenor);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &TenorDetailDialog::statusMessage,
            this,
            [self = QPointer<TenorController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &TenorDetailDialog::errorMessage,
            this,
            [self = QPointer<TenorController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenor: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TenorController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void TenorController::onRevertVersion(const marketdata::domain::tenor& tenor) {
    BOOST_LOG_SEV(lg(), info) << "Reverting tenor to version: " << tenor.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new TenorDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_tenor = tenor;
    reverted_tenor.version = 0;
    detailDialog->setTenor(reverted_tenor);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog, &TenorDetailDialog::statusMessage, this, &TenorController::statusMessage);
    connect(detailDialog, &TenorDetailDialog::errorMessage, this, &TenorController::errorMessage);
    connect(detailDialog,
            &TenorDetailDialog::tenorSaved,
            this,
            [self = QPointer<TenorController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor reverted: " << code.toStdString();
                emit self->statusMessage(QString("Tenor '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Tenor: %1").arg(QString::fromStdString(tenor.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* TenorController::listWindow() const {
    return listWindow_;
}

void TenorController::notifyOpenDialogs(const QStringList& entityIds) {
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        auto* window = it.value();
        if (!window)
            continue;

        if (it.key().startsWith("details.")) {
            if (auto* dialog = qobject_cast<DetailDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        } else if (it.key().startsWith("history.")) {
            if (auto* dialog = qobject_cast<HistoryDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        }
    }
}

}

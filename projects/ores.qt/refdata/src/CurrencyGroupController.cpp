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
#include "ores.qt/CurrencyGroupController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CurrencyGroupDetailDialog.hpp"
#include "ores.qt/CurrencyGroupHistoryDialog.hpp"
#include "ores.qt/CurrencyGroupMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/currency_group_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view group_event_name =
    eventing::domain::event_traits<refdata::eventing::currency_group_changed_event>::name;
}

CurrencyGroupController::CurrencyGroupController(QMainWindow* mainWindow,
                                                 QMdiArea* mdiArea,
                                                 ClientManager* clientManager,
                                                 ChangeReasonCache* changeReasonCache,
                                                 const QString& username,
                                                 QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, group_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CurrencyGroupController created";
}

void CurrencyGroupController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "currency_groups");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CurrencyGroupMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &CurrencyGroupMdiWindow::statusChanged,
            this,
            &CurrencyGroupController::statusMessage);
    connect(listWindow_,
            &CurrencyGroupMdiWindow::errorOccurred,
            this,
            &CurrencyGroupController::errorMessage);
    connect(listWindow_,
            &CurrencyGroupMdiWindow::showGroupDetails,
            this,
            &CurrencyGroupController::onShowDetails);
    connect(listWindow_,
            &CurrencyGroupMdiWindow::addNewRequested,
            this,
            &CurrencyGroupController::onAddNewRequested);
    connect(listWindow_,
            &CurrencyGroupMdiWindow::showGroupHistory,
            this,
            &CurrencyGroupController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Currency Groups");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
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
            [self = QPointer<CurrencyGroupController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Currency Group list window created";
}

void CurrencyGroupController::closeAllWindows() {
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

void CurrencyGroupController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CurrencyGroupController::onShowDetails(const refdata::domain::currency_group& group) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << group.code;
    showDetailWindow(group);
}

void CurrencyGroupController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new currency group requested";
    showAddWindow();
}

void CurrencyGroupController::onShowHistory(const refdata::domain::currency_group& group) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << group.code;
    showHistoryWindow(QString::fromStdString(group.code));
}

void CurrencyGroupController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new currency group";

    auto* detailDialog = new CurrencyGroupDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CurrencyGroupDetailDialog::statusMessage,
            this,
            &CurrencyGroupController::statusMessage);
    connect(detailDialog,
            &CurrencyGroupDetailDialog::errorMessage,
            this,
            &CurrencyGroupController::errorMessage);
    connect(detailDialog,
            &CurrencyGroupDetailDialog::groupSaved,
            this,
            [self = QPointer<CurrencyGroupController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency Group saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Currency Group");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyGroupController::showDetailWindow(const refdata::domain::currency_group& group) {

    const QString identifier = QString::fromStdString(group.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << group.code;

    auto* detailDialog = new CurrencyGroupDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setGroup(group);

    connect(detailDialog,
            &CurrencyGroupDetailDialog::statusMessage,
            this,
            &CurrencyGroupController::statusMessage);
    connect(detailDialog,
            &CurrencyGroupDetailDialog::errorMessage,
            this,
            &CurrencyGroupController::errorMessage);
    connect(detailDialog,
            &CurrencyGroupDetailDialog::groupSaved,
            this,
            [self = QPointer<CurrencyGroupController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency Group saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CurrencyGroupDetailDialog::groupDeleted,
            this,
            [self = QPointer<CurrencyGroupController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency Group deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency Group: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CurrencyGroupController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyGroupController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for currency group: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new CurrencyGroupHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &CurrencyGroupHistoryDialog::statusChanged,
            this,
            [self = QPointer<CurrencyGroupController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &CurrencyGroupHistoryDialog::errorOccurred,
            this,
            [self = QPointer<CurrencyGroupController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &CurrencyGroupHistoryDialog::revertVersionRequested,
            this,
            &CurrencyGroupController::onRevertVersion);
    connect(historyDialog,
            &CurrencyGroupHistoryDialog::openVersionRequested,
            this,
            &CurrencyGroupController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Currency Group History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CurrencyGroupController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CurrencyGroupController::onOpenVersion(const refdata::domain::currency_group& group,
                                            int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for currency group: " << group.code;

    const QString code = QString::fromStdString(group.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CurrencyGroupDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setGroup(group);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CurrencyGroupDetailDialog::statusMessage,
            this,
            [self = QPointer<CurrencyGroupController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CurrencyGroupDetailDialog::errorMessage,
            this,
            [self = QPointer<CurrencyGroupController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Currency Group: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CurrencyGroupController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CurrencyGroupController::onRevertVersion(const refdata::domain::currency_group& group) {
    BOOST_LOG_SEV(lg(), info) << "Reverting currency group to version: " << group.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CurrencyGroupDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_group = group;
    reverted_group.version = 0;
    detailDialog->setGroup(reverted_group);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CurrencyGroupDetailDialog::statusMessage,
            this,
            &CurrencyGroupController::statusMessage);
    connect(detailDialog,
            &CurrencyGroupDetailDialog::errorMessage,
            this,
            &CurrencyGroupController::errorMessage);
    connect(detailDialog,
            &CurrencyGroupDetailDialog::groupSaved,
            this,
            [self = QPointer<CurrencyGroupController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency Group reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Currency Group '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Currency Group: %1").arg(QString::fromStdString(group.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CurrencyGroupController::listWindow() const {
    return listWindow_;
}

}

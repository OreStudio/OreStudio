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
#include "ores.qt/CrmEnabledDerivedPairController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CrmEnabledDerivedPairDetailDialog.hpp"
#include "ores.qt/CrmEnabledDerivedPairHistoryDialog.hpp"
#include "ores.qt/CrmEnabledDerivedPairMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/crm_enabled_derived_pair_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view pair_event_name =
    eventing::domain::event_traits<refdata::eventing::crm_enabled_derived_pair_changed_event>::name;
}

CrmEnabledDerivedPairController::CrmEnabledDerivedPairController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    BadgeCache* badgeCache,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, pair_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , badgeCache_(badgeCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CrmEnabledDerivedPairController created";
}

void CrmEnabledDerivedPairController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "crm_enabled_derived_pairs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CrmEnabledDerivedPairMdiWindow(clientManager_, username_, badgeCache_);

    // Connect signals
    connect(listWindow_,
            &CrmEnabledDerivedPairMdiWindow::statusChanged,
            this,
            &CrmEnabledDerivedPairController::statusMessage);
    connect(listWindow_,
            &CrmEnabledDerivedPairMdiWindow::errorOccurred,
            this,
            &CrmEnabledDerivedPairController::errorMessage);
    connect(listWindow_,
            &CrmEnabledDerivedPairMdiWindow::showPairDetails,
            this,
            &CrmEnabledDerivedPairController::onShowDetails);
    connect(listWindow_,
            &CrmEnabledDerivedPairMdiWindow::addNewRequested,
            this,
            &CrmEnabledDerivedPairController::onAddNewRequested);
    connect(listWindow_,
            &CrmEnabledDerivedPairMdiWindow::showPairHistory,
            this,
            &CrmEnabledDerivedPairController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("CRM Enabled Derived Pairs");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor));
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
            [self = QPointer<CrmEnabledDerivedPairController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "CRM Enabled Derived Pair list window created";
}

void CrmEnabledDerivedPairController::closeAllWindows() {
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

void CrmEnabledDerivedPairController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CrmEnabledDerivedPairController::onShowDetails(
    const refdata::domain::crm_enabled_derived_pair& pair) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << boost::uuids::to_string(pair.id);
    showDetailWindow(pair);
}

void CrmEnabledDerivedPairController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new CRM enabled derived pair requested";
    showAddWindow();
}


void CrmEnabledDerivedPairController::onShowHistory(
    const refdata::domain::crm_enabled_derived_pair& pair) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(pair.id);
    showHistoryWindow(pair);
}

void CrmEnabledDerivedPairController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new CRM enabled derived pair";

    auto* detailDialog = new CrmEnabledDerivedPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::statusMessage,
            this,
            &CrmEnabledDerivedPairController::statusMessage);
    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::errorMessage,
            this,
            &CrmEnabledDerivedPairController::errorMessage);
    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CrmEnabledDerivedPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "CRM Enabled Derived Pair saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New CRM Enabled Derived Pair");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CrmEnabledDerivedPairController::showDetailWindow(
    const refdata::domain::crm_enabled_derived_pair& pair) {

    const QString identifier = QString::fromStdString(boost::uuids::to_string(pair.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(pair.id);

    auto* detailDialog = new CrmEnabledDerivedPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setPair(pair);

    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::statusMessage,
            this,
            &CrmEnabledDerivedPairController::statusMessage);
    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::errorMessage,
            this,
            &CrmEnabledDerivedPairController::errorMessage);
    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CrmEnabledDerivedPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "CRM Enabled Derived Pair saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::pairDeleted,
            this,
            [self = QPointer<CrmEnabledDerivedPairController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "CRM Enabled Derived Pair deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("CRM Enabled Derived Pair: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CrmEnabledDerivedPairController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CrmEnabledDerivedPairController::showHistoryWindow(
    const refdata::domain::crm_enabled_derived_pair& pair) {
    const QString code = QString::fromStdString(boost::uuids::to_string(pair.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for CRM enabled derived pair: "
                              << boost::uuids::to_string(pair.id);

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << boost::uuids::to_string(pair.id);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << boost::uuids::to_string(pair.id);

    auto* historyDialog =
        new CrmEnabledDerivedPairHistoryDialog(pair.id, code, clientManager_, mainWindow_);

    connect(historyDialog,
            &CrmEnabledDerivedPairHistoryDialog::statusChanged,
            this,
            [self = QPointer<CrmEnabledDerivedPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &CrmEnabledDerivedPairHistoryDialog::errorOccurred,
            this,
            [self = QPointer<CrmEnabledDerivedPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &CrmEnabledDerivedPairHistoryDialog::revertVersionRequested,
            this,
            &CrmEnabledDerivedPairController::onRevertVersion);
    connect(historyDialog,
            &CrmEnabledDerivedPairHistoryDialog::openVersionRequested,
            this,
            &CrmEnabledDerivedPairController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("CRM Enabled Derived Pair History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CrmEnabledDerivedPairController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CrmEnabledDerivedPairController::onOpenVersion(
    const refdata::domain::crm_enabled_derived_pair& pair, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for CRM enabled derived pair: "
                              << boost::uuids::to_string(pair.id);

    const QString code = QString::fromStdString(boost::uuids::to_string(pair.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CrmEnabledDerivedPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setPair(pair);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::statusMessage,
            this,
            [self = QPointer<CrmEnabledDerivedPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::errorMessage,
            this,
            [self = QPointer<CrmEnabledDerivedPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("CRM Enabled Derived Pair: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CrmEnabledDerivedPairController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CrmEnabledDerivedPairController::onRevertVersion(
    const refdata::domain::crm_enabled_derived_pair& pair) {
    BOOST_LOG_SEV(lg(), info) << "Reverting CRM enabled derived pair to version: " << pair.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CrmEnabledDerivedPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_pair = pair;
    reverted_pair.version = 0;
    detailDialog->setPair(reverted_pair);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::statusMessage,
            this,
            &CrmEnabledDerivedPairController::statusMessage);
    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::errorMessage,
            this,
            &CrmEnabledDerivedPairController::errorMessage);
    connect(detailDialog,
            &CrmEnabledDerivedPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CrmEnabledDerivedPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "CRM Enabled Derived Pair reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("CRM Enabled Derived Pair '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert CRM Enabled Derived Pair: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(pair.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CrmEnabledDerivedPairController::listWindow() const {
    return listWindow_;
}

void CrmEnabledDerivedPairController::notifyOpenDialogs(const QStringList& entityIds) {
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

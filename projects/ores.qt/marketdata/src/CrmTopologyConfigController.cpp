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
#include "ores.qt/CrmTopologyConfigController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.marketdata.api/eventing/crm_topology_config_changed_event.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CrmTopologyConfigDetailDialog.hpp"
#include "ores.qt/CrmTopologyConfigHistoryDialog.hpp"
#include "ores.qt/CrmTopologyConfigMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view config_event_name =
    eventing::domain::event_traits<marketdata::eventing::crm_topology_config_changed_event>::name;
}

CrmTopologyConfigController::CrmTopologyConfigController(QMainWindow* mainWindow,
                                                         QMdiArea* mdiArea,
                                                         ClientManager* clientManager,
                                                         ChangeReasonCache* changeReasonCache,
                                                         const QString& username,
                                                         BadgeCache* badgeCache,
                                                         QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, config_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , badgeCache_(badgeCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CrmTopologyConfigController created";
}

void CrmTopologyConfigController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "crm_topology_configs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CrmTopologyConfigMdiWindow(clientManager_, username_, badgeCache_);

    // Connect signals
    connect(listWindow_,
            &CrmTopologyConfigMdiWindow::statusChanged,
            this,
            &CrmTopologyConfigController::statusMessage);
    connect(listWindow_,
            &CrmTopologyConfigMdiWindow::errorOccurred,
            this,
            &CrmTopologyConfigController::errorMessage);
    connect(listWindow_,
            &CrmTopologyConfigMdiWindow::showConfigDetails,
            this,
            &CrmTopologyConfigController::onShowDetails);
    connect(listWindow_,
            &CrmTopologyConfigMdiWindow::addNewRequested,
            this,
            &CrmTopologyConfigController::onAddNewRequested);
    connect(listWindow_,
            &CrmTopologyConfigMdiWindow::showConfigHistory,
            this,
            &CrmTopologyConfigController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("CRM Topology Configs");
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
            [self = QPointer<CrmTopologyConfigController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "CRM Topology Config list window created";
}

void CrmTopologyConfigController::closeAllWindows() {
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

void CrmTopologyConfigController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CrmTopologyConfigController::onShowDetails(
    const marketdata::domain::crm_topology_config& config) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << config.name;
    showDetailWindow(config);
}

void CrmTopologyConfigController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new CRM topology config requested";
    showAddWindow();
}


void CrmTopologyConfigController::onShowHistory(
    const marketdata::domain::crm_topology_config& config) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << config.name;
    showHistoryWindow(config);
}

void CrmTopologyConfigController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new CRM topology config";

    auto* detailDialog = new CrmTopologyConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::statusMessage,
            this,
            &CrmTopologyConfigController::statusMessage);
    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::errorMessage,
            this,
            &CrmTopologyConfigController::errorMessage);
    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::configSaved,
            this,
            [self = QPointer<CrmTopologyConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CRM Topology Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New CRM Topology Config");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CrmTopologyConfigController::showDetailWindow(
    const marketdata::domain::crm_topology_config& config) {

    const QString identifier = QString::fromStdString(config.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << config.name;

    auto* detailDialog = new CrmTopologyConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConfig(config);

    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::statusMessage,
            this,
            &CrmTopologyConfigController::statusMessage);
    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::errorMessage,
            this,
            &CrmTopologyConfigController::errorMessage);
    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::configSaved,
            this,
            [self = QPointer<CrmTopologyConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CRM Topology Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::configDeleted,
            this,
            [self = QPointer<CrmTopologyConfigController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CRM Topology Config deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("CRM Topology Config: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CrmTopologyConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CrmTopologyConfigController::showHistoryWindow(
    const marketdata::domain::crm_topology_config& config) {
    const QString code = QString::fromStdString(config.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for CRM topology config: " << config.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << config.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << config.name;

    auto* historyDialog =
        new CrmTopologyConfigHistoryDialog(config.id, code, clientManager_, mainWindow_);

    connect(historyDialog,
            &CrmTopologyConfigHistoryDialog::statusChanged,
            this,
            [self = QPointer<CrmTopologyConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &CrmTopologyConfigHistoryDialog::errorOccurred,
            this,
            [self = QPointer<CrmTopologyConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &CrmTopologyConfigHistoryDialog::revertVersionRequested,
            this,
            &CrmTopologyConfigController::onRevertVersion);
    connect(historyDialog,
            &CrmTopologyConfigHistoryDialog::openVersionRequested,
            this,
            &CrmTopologyConfigController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("CRM Topology Config History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CrmTopologyConfigController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CrmTopologyConfigController::onOpenVersion(
    const marketdata::domain::crm_topology_config& config, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for CRM topology config: " << config.name;

    const QString code = QString::fromStdString(config.name);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CrmTopologyConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConfig(config);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::statusMessage,
            this,
            [self = QPointer<CrmTopologyConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::errorMessage,
            this,
            [self = QPointer<CrmTopologyConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("CRM Topology Config: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CrmTopologyConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CrmTopologyConfigController::onRevertVersion(
    const marketdata::domain::crm_topology_config& config) {
    BOOST_LOG_SEV(lg(), info) << "Reverting CRM topology config to version: " << config.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CrmTopologyConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_config = config;
    reverted_config.version = 0;
    detailDialog->setConfig(reverted_config);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::statusMessage,
            this,
            &CrmTopologyConfigController::statusMessage);
    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::errorMessage,
            this,
            &CrmTopologyConfigController::errorMessage);
    connect(detailDialog,
            &CrmTopologyConfigDetailDialog::configSaved,
            this,
            [self = QPointer<CrmTopologyConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CRM Topology Config reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("CRM Topology Config '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert CRM Topology Config: %1").arg(QString::fromStdString(config.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CrmTopologyConfigController::listWindow() const {
    return listWindow_;
}

void CrmTopologyConfigController::notifyOpenDialogs(const QStringList& entityIds) {
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

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
#include "ores.qt/CrmDriverPairController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CrmDriverPairDetailDialog.hpp"
#include "ores.qt/CrmDriverPairMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/crm_driver_pair_changed_event.hpp"
#include "ores.refdata.api/messaging/crm_driver_pair_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view pair_event_name =
    eventing::domain::event_traits<refdata::eventing::crm_driver_pair_changed_event>::name;
}

CrmDriverPairController::CrmDriverPairController(QMainWindow* mainWindow,
                                                 QMdiArea* mdiArea,
                                                 ClientManager* clientManager,
                                                 ImageCache* imageCache,
                                                 ChangeReasonCache* changeReasonCache,
                                                 const QString& username,
                                                 BadgeCache* badgeCache,
                                                 QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, pair_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , badgeCache_(badgeCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {
    setImageCache(imageCache);

    BOOST_LOG_SEV(lg(), debug) << "CrmDriverPairController created";
}

void CrmDriverPairController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "crm_driver_pairs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CrmDriverPairMdiWindow(clientManager_, username_, badgeCache_, imageCache_);

    // Connect signals
    connect(listWindow_,
            &CrmDriverPairMdiWindow::statusChanged,
            this,
            &CrmDriverPairController::statusMessage);
    connect(listWindow_,
            &CrmDriverPairMdiWindow::errorOccurred,
            this,
            &CrmDriverPairController::errorMessage);
    connect(listWindow_,
            &CrmDriverPairMdiWindow::showPairDetails,
            this,
            &CrmDriverPairController::onShowDetails);
    connect(listWindow_,
            &CrmDriverPairMdiWindow::addNewRequested,
            this,
            &CrmDriverPairController::onAddNewRequested);
    connect(listWindow_,
            &CrmDriverPairMdiWindow::showPairHistory,
            this,
            &CrmDriverPairController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("CRM Driver Pairs");
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
            [self = QPointer<CrmDriverPairController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "CRM Driver Pair list window created";
}

void CrmDriverPairController::closeAllWindows() {
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

void CrmDriverPairController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CrmDriverPairController::onShowDetails(const refdata::domain::crm_driver_pair& pair) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << boost::uuids::to_string(pair.id);
    showDetailWindow(pair);
}

void CrmDriverPairController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new CRM driver pair requested";
    showAddWindow();
}


void CrmDriverPairController::onShowHistory(const refdata::domain::crm_driver_pair& pair) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(pair.id);
    showHistoryWindow(pair);
}

void CrmDriverPairController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new CRM driver pair";

    auto* detailDialog = new CrmDriverPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CrmDriverPairDetailDialog::statusMessage,
            this,
            &CrmDriverPairController::statusMessage);
    connect(detailDialog,
            &CrmDriverPairDetailDialog::errorMessage,
            this,
            &CrmDriverPairController::errorMessage);
    connect(detailDialog,
            &CrmDriverPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CrmDriverPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CRM Driver Pair saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New CRM Driver Pair");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CrmDriverPairController::showDetailWindow(const refdata::domain::crm_driver_pair& pair) {

    const QString identifier = QString::fromStdString(boost::uuids::to_string(pair.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(pair.id);

    auto* detailDialog = new CrmDriverPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setPair(pair);

    connect(detailDialog,
            &CrmDriverPairDetailDialog::statusMessage,
            this,
            &CrmDriverPairController::statusMessage);
    connect(detailDialog,
            &CrmDriverPairDetailDialog::errorMessage,
            this,
            &CrmDriverPairController::errorMessage);
    connect(detailDialog,
            &CrmDriverPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CrmDriverPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CRM Driver Pair saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CrmDriverPairDetailDialog::pairDeleted,
            this,
            [self = QPointer<CrmDriverPairController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CRM Driver Pair deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("CRM Driver Pair: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CrmDriverPairController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CrmDriverPairController::showHistoryWindow(const refdata::domain::crm_driver_pair& pair) {
    const QString code = QString::fromStdString(boost::uuids::to_string(pair.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for CRM driver pair: "
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

    const QString entityId = QString::fromStdString(boost::uuids::to_string(pair.id));
    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::crm_driver_pair{})),
                          entityId.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<CrmDriverPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<CrmDriverPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<CrmDriverPairController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<CrmDriverPairController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onOpenHistoryVersion(entityId, version);
            });

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("CRM Driver Pair History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CrmDriverPairController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CrmDriverPairController::onOpenVersion(const refdata::domain::crm_driver_pair& pair,
                                            int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for CRM driver pair: " << boost::uuids::to_string(pair.id);

    const QString code = QString::fromStdString(boost::uuids::to_string(pair.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CrmDriverPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setPair(pair);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CrmDriverPairDetailDialog::statusMessage,
            this,
            [self = QPointer<CrmDriverPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CrmDriverPairDetailDialog::errorMessage,
            this,
            [self = QPointer<CrmDriverPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("CRM Driver Pair: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CrmDriverPairController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CrmDriverPairController::fetchCrmDriverPairHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::crm_driver_pair>, QString>)>
        callback) {
    refdata::messaging::get_crm_driver_pair_history_request request;
    request.id = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::crm_driver_pair>, QString>;

    QPointer<CrmDriverPairController> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run([clientManager, request = std::move(request)]() -> FetchResult {
        if (!clientManager || !clientManager->isConnected())
            return std::unexpected(QString("Not connected to server"));
        auto result = clientManager->process_authenticated_request(std::move(request));
        if (!result)
            return std::unexpected(QString::fromStdString(result.error()));
        if (!result->success)
            return std::unexpected(QString::fromStdString(result->message));
        return std::move(result->history);
    });

    auto* watcher = new QFutureWatcher<FetchResult>(this);
    connect(watcher,
            &QFutureWatcher<FetchResult>::finished,
            this,
            [self, watcher, callback = std::move(callback)]() mutable {
                auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                callback(std::move(result));
            });
    watcher->setFuture(future);
}

void CrmDriverPairController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CrmDriverPairController> self = this;
    fetchCrmDriverPairHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::crm_driver_pair>, QString> result) {
            if (!self)
                return;
            if (!result) {
                emit self->errorMessage(QString("Failed to load history for '%1': %2")
                                            .arg(entityId)
                                            .arg(result.error()));
                return;
            }
            const auto& history = *result;
            const auto it = std::find_if(history.begin(), history.end(), [&](const auto& v) {
                return v.version == versionNumber;
            });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onOpenVersion(*it, versionNumber);
        });
}

void CrmDriverPairController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CrmDriverPairController> self = this;
    fetchCrmDriverPairHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::crm_driver_pair>, QString> result) {
            if (!self)
                return;
            if (!result) {
                emit self->errorMessage(QString("Failed to load history for '%1': %2")
                                            .arg(entityId)
                                            .arg(result.error()));
                return;
            }
            const auto& history = *result;
            const auto it = std::find_if(history.begin(), history.end(), [&](const auto& v) {
                return v.version == versionNumber;
            });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onRevertVersion(*it);
        });
}

void CrmDriverPairController::onRevertVersion(const refdata::domain::crm_driver_pair& pair) {
    BOOST_LOG_SEV(lg(), info) << "Reverting CRM driver pair to version: " << pair.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CrmDriverPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_pair = pair;
    reverted_pair.version = 0;
    detailDialog->setPair(reverted_pair);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CrmDriverPairDetailDialog::statusMessage,
            this,
            &CrmDriverPairController::statusMessage);
    connect(detailDialog,
            &CrmDriverPairDetailDialog::errorMessage,
            this,
            &CrmDriverPairController::errorMessage);
    connect(detailDialog,
            &CrmDriverPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CrmDriverPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CRM Driver Pair reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("CRM Driver Pair '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert CRM Driver Pair: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(pair.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CrmDriverPairController::listWindow() const {
    return listWindow_;
}

void CrmDriverPairController::notifyOpenDialogs(const QStringList& entityIds) {
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

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
#include "ores.qt/AssetClassCodeController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/AssetClassCodeDetailDialog.hpp"
#include "ores.qt/AssetClassCodeMdiWindow.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/asset_class_code_changed_event.hpp"
#include "ores.refdata.api/messaging/asset_class_code_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view class__event_name =
    eventing::domain::event_traits<refdata::eventing::asset_class_code_changed_event>::name;
}

AssetClassCodeController::AssetClassCodeController(QMainWindow* mainWindow,
                                                   QMdiArea* mdiArea,
                                                   ClientManager* clientManager,
                                                   ChangeReasonCache* changeReasonCache,
                                                   const QString& username,
                                                   QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, class__event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "AssetClassCodeController created";
}

void AssetClassCodeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "asset_class_codes");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new AssetClassCodeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &AssetClassCodeMdiWindow::statusChanged,
            this,
            &AssetClassCodeController::statusMessage);
    connect(listWindow_,
            &AssetClassCodeMdiWindow::errorOccurred,
            this,
            &AssetClassCodeController::errorMessage);
    connect(listWindow_,
            &AssetClassCodeMdiWindow::showCodeDetails,
            this,
            &AssetClassCodeController::onShowDetails);
    connect(listWindow_,
            &AssetClassCodeMdiWindow::addNewRequested,
            this,
            &AssetClassCodeController::onAddNewRequested);
    connect(listWindow_,
            &AssetClassCodeMdiWindow::showCodeHistory,
            this,
            &AssetClassCodeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Asset Class Codes");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));
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
            [self = QPointer<AssetClassCodeController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Asset Class Code list window created";
}

void AssetClassCodeController::closeAllWindows() {
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

void AssetClassCodeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void AssetClassCodeController::onShowDetails(const refdata::domain::asset_class_code& class_) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << class_.code;
    showDetailWindow(class_);
}

void AssetClassCodeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new asset class code requested";
    showAddWindow();
}


void AssetClassCodeController::onShowHistory(const refdata::domain::asset_class_code& class_) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << class_.code;
    showHistoryWindow(QString::fromStdString(class_.code));
}

void AssetClassCodeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new asset class code";

    auto* detailDialog = new AssetClassCodeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &AssetClassCodeDetailDialog::statusMessage,
            this,
            &AssetClassCodeController::statusMessage);
    connect(detailDialog,
            &AssetClassCodeDetailDialog::errorMessage,
            this,
            &AssetClassCodeController::errorMessage);
    connect(detailDialog,
            &AssetClassCodeDetailDialog::class_Saved,
            this,
            [self = QPointer<AssetClassCodeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Asset Class Code saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Asset Class Code");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void AssetClassCodeController::showDetailWindow(const refdata::domain::asset_class_code& class_) {

    const QString identifier = QString::fromStdString(class_.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << class_.code;

    auto* detailDialog = new AssetClassCodeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setCode(class_);

    connect(detailDialog,
            &AssetClassCodeDetailDialog::statusMessage,
            this,
            &AssetClassCodeController::statusMessage);
    connect(detailDialog,
            &AssetClassCodeDetailDialog::errorMessage,
            this,
            &AssetClassCodeController::errorMessage);
    connect(detailDialog,
            &AssetClassCodeDetailDialog::class_Saved,
            this,
            [self = QPointer<AssetClassCodeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Asset Class Code saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &AssetClassCodeDetailDialog::class_Deleted,
            this,
            [self = QPointer<AssetClassCodeController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Asset Class Code deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Asset Class Code: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<AssetClassCodeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void AssetClassCodeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for asset class code: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::asset_class_code{})),
                          code.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<AssetClassCodeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<AssetClassCodeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<AssetClassCodeController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<AssetClassCodeController>(this)](
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
    historyWindow->setWindowTitle(QString("Asset Class Code History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<AssetClassCodeController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void AssetClassCodeController::onOpenVersion(const refdata::domain::asset_class_code& class_,
                                             int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for asset class code: " << class_.code;

    const QString code = QString::fromStdString(class_.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new AssetClassCodeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCode(class_);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &AssetClassCodeDetailDialog::statusMessage,
            this,
            [self = QPointer<AssetClassCodeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &AssetClassCodeDetailDialog::errorMessage,
            this,
            [self = QPointer<AssetClassCodeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Asset Class Code: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<AssetClassCodeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void AssetClassCodeController::fetchAssetClassCodeHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::asset_class_code>, QString>)>
        callback) {
    refdata::messaging::get_asset_class_code_history_request request;
    request.code = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::asset_class_code>, QString>;

    QPointer<AssetClassCodeController> self = this;
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

void AssetClassCodeController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<AssetClassCodeController> self = this;
    fetchAssetClassCodeHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::asset_class_code>, QString> result) {
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

void AssetClassCodeController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<AssetClassCodeController> self = this;
    fetchAssetClassCodeHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::asset_class_code>, QString> result) {
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

void AssetClassCodeController::onRevertVersion(const refdata::domain::asset_class_code& class_) {
    BOOST_LOG_SEV(lg(), info) << "Reverting asset class code to version: " << class_.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new AssetClassCodeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_class_ = class_;
    reverted_class_.version = 0;
    detailDialog->setCode(reverted_class_);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &AssetClassCodeDetailDialog::statusMessage,
            this,
            &AssetClassCodeController::statusMessage);
    connect(detailDialog,
            &AssetClassCodeDetailDialog::errorMessage,
            this,
            &AssetClassCodeController::errorMessage);
    connect(detailDialog,
            &AssetClassCodeDetailDialog::class_Saved,
            this,
            [self = QPointer<AssetClassCodeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Asset Class Code reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Asset Class Code '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Asset Class Code: %1").arg(QString::fromStdString(class_.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* AssetClassCodeController::listWindow() const {
    return listWindow_;
}

void AssetClassCodeController::notifyOpenDialogs(const QStringList& entityIds) {
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

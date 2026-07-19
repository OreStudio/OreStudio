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
#include "ores.qt/IrCurveGenerationConfigController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/IrCurveGenerationConfigDetailDialog.hpp"
#include "ores.qt/IrCurveGenerationConfigMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.synthetic.api/eventing/ir_curve_generation_config_changed_event.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
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
constexpr std::string_view ir_curve_generation_config_event_name = eventing::domain::event_traits<
    synthetic::eventing::ir_curve_generation_config_changed_event>::name;
}

IrCurveGenerationConfigController::IrCurveGenerationConfigController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow,
                       mdiArea,
                       clientManager,
                       username,
                       ir_curve_generation_config_event_name,
                       parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "IrCurveGenerationConfigController created";
}

void IrCurveGenerationConfigController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "ir_curve_generation_configs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new IrCurveGenerationConfigMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &IrCurveGenerationConfigMdiWindow::statusChanged,
            this,
            &IrCurveGenerationConfigController::statusMessage);
    connect(listWindow_,
            &IrCurveGenerationConfigMdiWindow::errorOccurred,
            this,
            &IrCurveGenerationConfigController::errorMessage);
    connect(listWindow_,
            &IrCurveGenerationConfigMdiWindow::showConfigDetails,
            this,
            &IrCurveGenerationConfigController::onShowDetails);
    connect(listWindow_,
            &IrCurveGenerationConfigMdiWindow::addNewRequested,
            this,
            &IrCurveGenerationConfigController::onAddNewRequested);
    connect(listWindow_,
            &IrCurveGenerationConfigMdiWindow::showConfigHistory,
            this,
            &IrCurveGenerationConfigController::onShowHistory);
    connect(listWindow_,
            &IrCurveGenerationConfigMdiWindow::showConfigSnapshot,
            this,
            &IrCurveGenerationConfigController::showConfigSnapshot);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("IR Curve Generation Configs");
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
            [self = QPointer<IrCurveGenerationConfigController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "IR Curve Generation Config list window created";
}

void IrCurveGenerationConfigController::closeAllWindows() {
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

void IrCurveGenerationConfigController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void IrCurveGenerationConfigController::onShowDetails(
    const synthetic::domain::ir_curve_generation_config& ir_curve_generation_config) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: "
                               << boost::uuids::to_string(ir_curve_generation_config.id);
    showDetailWindow(ir_curve_generation_config);
}

void IrCurveGenerationConfigController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new IR curve generation config requested";
    showAddWindow();
}


void IrCurveGenerationConfigController::onShowHistory(
    const synthetic::domain::ir_curve_generation_config& ir_curve_generation_config) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(ir_curve_generation_config.id);
    showHistoryWindow(ir_curve_generation_config);
}

void IrCurveGenerationConfigController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new IR curve generation config";

    auto* detailDialog = new IrCurveGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::statusMessage,
            this,
            &IrCurveGenerationConfigController::statusMessage);
    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::errorMessage,
            this,
            &IrCurveGenerationConfigController::errorMessage);
    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::ir_curve_generation_configSaved,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "IR Curve Generation Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New IR Curve Generation Config");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void IrCurveGenerationConfigController::showDetailWindow(
    const synthetic::domain::ir_curve_generation_config& ir_curve_generation_config) {

    const QString identifier =
        QString::fromStdString(boost::uuids::to_string(ir_curve_generation_config.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(ir_curve_generation_config.id);

    auto* detailDialog = new IrCurveGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConfig(ir_curve_generation_config);

    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::statusMessage,
            this,
            &IrCurveGenerationConfigController::statusMessage);
    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::errorMessage,
            this,
            &IrCurveGenerationConfigController::errorMessage);
    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::ir_curve_generation_configSaved,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "IR Curve Generation Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::ir_curve_generation_configDeleted,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "IR Curve Generation Config deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("IR Curve Generation Config: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<IrCurveGenerationConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void IrCurveGenerationConfigController::showHistoryWindow(
    const synthetic::domain::ir_curve_generation_config& ir_curve_generation_config) {
    const QString code =
        QString::fromStdString(boost::uuids::to_string(ir_curve_generation_config.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for IR curve generation config: "
                              << boost::uuids::to_string(ir_curve_generation_config.id);

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << boost::uuids::to_string(ir_curve_generation_config.id);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << boost::uuids::to_string(ir_curve_generation_config.id);

    const QString entityId =
        QString::fromStdString(boost::uuids::to_string(ir_curve_generation_config.id));
    auto* historyDialog = new HistoryDialog(
        std::string(entity_type_of(synthetic::domain::ir_curve_generation_config{})),
        entityId.toStdString(),
        clientManager_,
        mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](
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
    historyWindow->setWindowTitle(QString("IR Curve Generation Config History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<IrCurveGenerationConfigController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void IrCurveGenerationConfigController::onOpenVersion(
    const synthetic::domain::ir_curve_generation_config& ir_curve_generation_config,
    int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for IR curve generation config: "
                              << boost::uuids::to_string(ir_curve_generation_config.id);

    const QString code =
        QString::fromStdString(boost::uuids::to_string(ir_curve_generation_config.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new IrCurveGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConfig(ir_curve_generation_config);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::statusMessage,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::errorMessage,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("IR Curve Generation Config: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<IrCurveGenerationConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void IrCurveGenerationConfigController::fetchIrCurveGenerationConfigHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<synthetic::domain::ir_curve_generation_config>,
                                     QString>)> callback) {
    synthetic::messaging::get_ir_curve_generation_config_history_request request;
    request.id = entityId.toStdString();

    using FetchResult =
        std::expected<std::vector<synthetic::domain::ir_curve_generation_config>, QString>;

    QPointer<IrCurveGenerationConfigController> self = this;
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

void IrCurveGenerationConfigController::onOpenHistoryVersion(const QString& entityId,
                                                             int versionNumber) {
    QPointer<IrCurveGenerationConfigController> self = this;
    fetchIrCurveGenerationConfigHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<synthetic::domain::ir_curve_generation_config>, QString>
                result) {
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

void IrCurveGenerationConfigController::onRevertHistoryVersion(const QString& entityId,
                                                               int versionNumber) {
    QPointer<IrCurveGenerationConfigController> self = this;
    fetchIrCurveGenerationConfigHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<synthetic::domain::ir_curve_generation_config>, QString>
                result) {
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

void IrCurveGenerationConfigController::onRevertVersion(
    const synthetic::domain::ir_curve_generation_config& ir_curve_generation_config) {
    BOOST_LOG_SEV(lg(), info) << "Reverting IR curve generation config to version: "
                              << ir_curve_generation_config.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new IrCurveGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_ir_curve_generation_config = ir_curve_generation_config;
    reverted_ir_curve_generation_config.version = 0;
    detailDialog->setConfig(reverted_ir_curve_generation_config);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::statusMessage,
            this,
            &IrCurveGenerationConfigController::statusMessage);
    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::errorMessage,
            this,
            &IrCurveGenerationConfigController::errorMessage);
    connect(detailDialog,
            &IrCurveGenerationConfigDetailDialog::ir_curve_generation_configSaved,
            this,
            [self = QPointer<IrCurveGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "IR Curve Generation Config reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("IR Curve Generation Config '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert IR Curve Generation Config: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(ir_curve_generation_config.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* IrCurveGenerationConfigController::listWindow() const {
    return listWindow_;
}

void IrCurveGenerationConfigController::notifyOpenDialogs(const QStringList& entityIds) {
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

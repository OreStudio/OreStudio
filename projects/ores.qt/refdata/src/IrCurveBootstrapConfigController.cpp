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
#include "ores.qt/IrCurveBootstrapConfigController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/CurveBuilderWorkbench.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/IrCurveBootstrapConfigMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/ir_curve_bootstrap_config_changed_event.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_config_protocol.hpp"
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
constexpr std::string_view config_event_name = eventing::domain::event_traits<
    refdata::eventing::ir_curve_bootstrap_config_changed_event>::name;
}

IrCurveBootstrapConfigController::IrCurveBootstrapConfigController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, config_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr)
    , changeReasonCache_(changeReasonCache) {
    setImageCache(imageCache);
    BOOST_LOG_SEV(lg(), debug) << "IrCurveBootstrapConfigController created";
}

void IrCurveBootstrapConfigController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "ir_curve_bootstrap_configs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new IrCurveBootstrapConfigMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &IrCurveBootstrapConfigMdiWindow::statusChanged,
            this,
            &IrCurveBootstrapConfigController::statusMessage);
    connect(listWindow_,
            &IrCurveBootstrapConfigMdiWindow::errorOccurred,
            this,
            &IrCurveBootstrapConfigController::errorMessage);
    connect(listWindow_,
            &IrCurveBootstrapConfigMdiWindow::showConfigDetails,
            this,
            &IrCurveBootstrapConfigController::onShowDetails);
    connect(listWindow_,
            &IrCurveBootstrapConfigMdiWindow::addNewRequested,
            this,
            &IrCurveBootstrapConfigController::onAddNewRequested);
    connect(listWindow_,
            &IrCurveBootstrapConfigMdiWindow::showConfigHistory,
            this,
            &IrCurveBootstrapConfigController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("IR Curve Bootstrap Configs");
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
            [self = QPointer<IrCurveBootstrapConfigController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "IR Curve Bootstrap Config list window created";
}

void IrCurveBootstrapConfigController::closeAllWindows() {
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

void IrCurveBootstrapConfigController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void IrCurveBootstrapConfigController::onShowDetails(
    const refdata::domain::ir_curve_bootstrap_config& config) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << boost::uuids::to_string(config.id);
    showDetailWindow(config);
}

void IrCurveBootstrapConfigController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new IR curve bootstrap config requested";
    showAddWindow();
}

void IrCurveBootstrapConfigController::openNewCurveWindow() {
    BOOST_LOG_SEV(lg(), info) << "Build Curve requested directly from Market Data menu";
    showAddWindow();
}


void IrCurveBootstrapConfigController::onShowHistory(
    const refdata::domain::ir_curve_bootstrap_config& config) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(config.id);
    showHistoryWindow(config);
}

void IrCurveBootstrapConfigController::wireWorkbenchCommon(CurveBuilderWorkbench* workbench,
                                                            DetachableMdiSubWindow* window) {
    workbench->setClientManager(clientManager_);
    workbench->setImageCache(imageCache());
    workbench->setChangeReasonCache(changeReasonCache_);
    workbench->setUsername(username_.toStdString());

    connect(workbench,
            &CurveBuilderWorkbench::statusMessage,
            this,
            &IrCurveBootstrapConfigController::statusMessage);
    connect(workbench,
            &CurveBuilderWorkbench::errorMessage,
            this,
            &IrCurveBootstrapConfigController::errorMessage);
    connect(workbench, &CurveBuilderWorkbench::closeRequested, window, &QWidget::close);
}

void IrCurveBootstrapConfigController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new IR curve bootstrap config";

    auto* workbench = new CurveBuilderWorkbench(mainWindow_);

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(workbench);
    detailWindow->setWindowTitle("New IR Curve Bootstrap Config");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    wireWorkbenchCommon(workbench, detailWindow);
    workbench->setCreateMode(true);

    connect(workbench,
            &CurveBuilderWorkbench::configSaved,
            this,
            [self = QPointer<IrCurveBootstrapConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "IR Curve Bootstrap Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    register_detachable_window(detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void IrCurveBootstrapConfigController::showDetailWindow(
    const refdata::domain::ir_curve_bootstrap_config& config) {

    const QString identifier = QString::fromStdString(boost::uuids::to_string(config.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(config.id);

    auto* workbench = new CurveBuilderWorkbench(mainWindow_);

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(workbench);
    detailWindow->setWindowTitle(QString("IR Curve Bootstrap Config: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    wireWorkbenchCommon(workbench, detailWindow);
    workbench->setConfig(config);

    connect(workbench,
            &CurveBuilderWorkbench::configSaved,
            this,
            [self = QPointer<IrCurveBootstrapConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "IR Curve Bootstrap Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<IrCurveBootstrapConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    show_managed_window(detailWindow, listMdiSubWindow_);
}

void IrCurveBootstrapConfigController::showHistoryWindow(
    const refdata::domain::ir_curve_bootstrap_config& config) {
    const QString code = QString::fromStdString(boost::uuids::to_string(config.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for IR curve bootstrap config: "
                              << boost::uuids::to_string(config.id);

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << boost::uuids::to_string(config.id);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << boost::uuids::to_string(config.id);

    const QString entityId = QString::fromStdString(boost::uuids::to_string(config.id));
    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::ir_curve_bootstrap_config{})),
                          entityId.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<IrCurveBootstrapConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<IrCurveBootstrapConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<IrCurveBootstrapConfigController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<IrCurveBootstrapConfigController>(this)](
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
    historyWindow->setWindowTitle(QString("IR Curve Bootstrap Config History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<IrCurveBootstrapConfigController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void IrCurveBootstrapConfigController::onOpenVersion(
    const refdata::domain::ir_curve_bootstrap_config& config, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for IR curve bootstrap config: "
                              << boost::uuids::to_string(config.id);

    const QString code = QString::fromStdString(boost::uuids::to_string(config.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    // Historical versions are opened in the same workbench, not read-only -- Workbench has no
    // read-only mode (unlike the discarded DetailDialog); Save is still gated on the workbench's
    // own edited config, so viewing a historical version is safe, just not enforced-immutable.
    auto* workbench = new CurveBuilderWorkbench(mainWindow_);

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(workbench);
    detailWindow->setWindowTitle(
        QString("IR Curve Bootstrap Config: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    wireWorkbenchCommon(workbench, detailWindow);
    workbench->setConfig(config);

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<IrCurveBootstrapConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void IrCurveBootstrapConfigController::fetchIrCurveBootstrapConfigHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::ir_curve_bootstrap_config>,
                                     QString>)> callback) {
    refdata::messaging::get_ir_curve_bootstrap_config_history_request request;
    request.id = entityId.toStdString();

    using FetchResult =
        std::expected<std::vector<refdata::domain::ir_curve_bootstrap_config>, QString>;

    QPointer<IrCurveBootstrapConfigController> self = this;
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

void IrCurveBootstrapConfigController::onOpenHistoryVersion(const QString& entityId,
                                                            int versionNumber) {
    QPointer<IrCurveBootstrapConfigController> self = this;
    fetchIrCurveBootstrapConfigHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::ir_curve_bootstrap_config>, QString>
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

void IrCurveBootstrapConfigController::onRevertHistoryVersion(const QString& entityId,
                                                              int versionNumber) {
    QPointer<IrCurveBootstrapConfigController> self = this;
    fetchIrCurveBootstrapConfigHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::ir_curve_bootstrap_config>, QString>
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

void IrCurveBootstrapConfigController::onRevertVersion(
    const refdata::domain::ir_curve_bootstrap_config& config) {
    BOOST_LOG_SEV(lg(), info) << "Reverting IR curve bootstrap config to version: "
                              << config.version;

    // Open workbench with the old version data loaded for editing; the workbench doesn't need a
    // markDirty() equivalent since the user must explicitly click Save regardless.
    auto* workbench = new CurveBuilderWorkbench(mainWindow_);
    auto reverted_config = config;
    reverted_config.version = 0;

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(workbench);
    detailWindow->setWindowTitle(
        QString("Revert IR Curve Bootstrap Config: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(config.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    wireWorkbenchCommon(workbench, detailWindow);
    workbench->setConfig(reverted_config);

    connect(workbench,
            &CurveBuilderWorkbench::configSaved,
            this,
            [self = QPointer<IrCurveBootstrapConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "IR Curve Bootstrap Config reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("IR Curve Bootstrap Config '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    register_detachable_window(detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* IrCurveBootstrapConfigController::listWindow() const {
    return listWindow_;
}

void IrCurveBootstrapConfigController::notifyOpenDialogs(const QStringList& entityIds) {
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

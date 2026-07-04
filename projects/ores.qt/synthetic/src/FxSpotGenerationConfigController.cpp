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
#include "ores.qt/FxSpotGenerationConfigController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FxSpotGenerationConfigDetailDialog.hpp"
#include "ores.qt/FxSpotGenerationConfigHistoryDialog.hpp"
#include "ores.qt/FxSpotGenerationConfigMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.synthetic.api/eventing/fx_spot_generation_config_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view fx_spot_generation_config_event_name = eventing::domain::event_traits<
    synthetic::eventing::fx_spot_generation_config_changed_event>::name;
}

FxSpotGenerationConfigController::FxSpotGenerationConfigController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow,
                       mdiArea,
                       clientManager,
                       username,
                       fx_spot_generation_config_event_name,
                       parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {
    setImageCache(imageCache);

    BOOST_LOG_SEV(lg(), debug) << "FxSpotGenerationConfigController created";
}

void FxSpotGenerationConfigController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "fx_spot_generation_configs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new FxSpotGenerationConfigMdiWindow(clientManager_, username_, imageCache_);

    // Connect signals
    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::statusChanged,
            this,
            &FxSpotGenerationConfigController::statusMessage);
    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::errorOccurred,
            this,
            &FxSpotGenerationConfigController::errorMessage);
    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::showConfigDetails,
            this,
            &FxSpotGenerationConfigController::onShowDetails);
    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::addNewRequested,
            this,
            &FxSpotGenerationConfigController::onAddNewRequested);
    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::showConfigHistory,
            this,
            &FxSpotGenerationConfigController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("FX Spot Generation Configs");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));
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
            [self = QPointer<FxSpotGenerationConfigController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "FX Spot Generation Config list window created";
}

void FxSpotGenerationConfigController::closeAllWindows() {
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

void FxSpotGenerationConfigController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void FxSpotGenerationConfigController::onShowDetails(
    const synthetic::domain::fx_spot_generation_config& fx_spot_generation_config) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: "
                               << boost::uuids::to_string(fx_spot_generation_config.id);
    showDetailWindow(fx_spot_generation_config);
}

void FxSpotGenerationConfigController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new FX spot generation config requested";
    showAddWindow();
}

void FxSpotGenerationConfigController::onShowHistory(
    const synthetic::domain::fx_spot_generation_config& fx_spot_generation_config) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(fx_spot_generation_config.id);
    showHistoryWindow(fx_spot_generation_config);
}

void FxSpotGenerationConfigController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new FX spot generation config";

    auto* detailDialog = new FxSpotGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::statusMessage,
            this,
            &FxSpotGenerationConfigController::statusMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::errorMessage,
            this,
            &FxSpotGenerationConfigController::errorMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::fx_spot_generation_configSaved,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "FX Spot Generation Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New FX Spot Generation Config");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FxSpotGenerationConfigController::showDetailWindow(
    const synthetic::domain::fx_spot_generation_config& fx_spot_generation_config) {

    const QString identifier =
        QString::fromStdString(boost::uuids::to_string(fx_spot_generation_config.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(fx_spot_generation_config.id);

    auto* detailDialog = new FxSpotGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConfig(fx_spot_generation_config);

    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::statusMessage,
            this,
            &FxSpotGenerationConfigController::statusMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::errorMessage,
            this,
            &FxSpotGenerationConfigController::errorMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::fx_spot_generation_configSaved,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "FX Spot Generation Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::fx_spot_generation_configDeleted,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "FX Spot Generation Config deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("FX Spot Generation Config: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<FxSpotGenerationConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FxSpotGenerationConfigController::showHistoryWindow(
    const synthetic::domain::fx_spot_generation_config& fx_spot_generation_config) {
    const QString code =
        QString::fromStdString(boost::uuids::to_string(fx_spot_generation_config.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for FX spot generation config: "
                              << boost::uuids::to_string(fx_spot_generation_config.id);

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << boost::uuids::to_string(fx_spot_generation_config.id);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << boost::uuids::to_string(fx_spot_generation_config.id);

    auto* historyDialog = new FxSpotGenerationConfigHistoryDialog(
        fx_spot_generation_config.id, code, clientManager_, mainWindow_);

    connect(historyDialog,
            &FxSpotGenerationConfigHistoryDialog::statusChanged,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &FxSpotGenerationConfigHistoryDialog::errorOccurred,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &FxSpotGenerationConfigHistoryDialog::revertVersionRequested,
            this,
            &FxSpotGenerationConfigController::onRevertVersion);
    connect(historyDialog,
            &FxSpotGenerationConfigHistoryDialog::openVersionRequested,
            this,
            &FxSpotGenerationConfigController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("FX Spot Generation Config History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<FxSpotGenerationConfigController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void FxSpotGenerationConfigController::onOpenVersion(
    const synthetic::domain::fx_spot_generation_config& fx_spot_generation_config,
    int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for FX spot generation config: "
                              << boost::uuids::to_string(fx_spot_generation_config.id);

    const QString code =
        QString::fromStdString(boost::uuids::to_string(fx_spot_generation_config.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new FxSpotGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConfig(fx_spot_generation_config);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::statusMessage,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::errorMessage,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("FX Spot Generation Config: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<FxSpotGenerationConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void FxSpotGenerationConfigController::onRevertVersion(
    const synthetic::domain::fx_spot_generation_config& fx_spot_generation_config) {
    BOOST_LOG_SEV(lg(), info) << "Reverting FX spot generation config to version: "
                              << fx_spot_generation_config.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new FxSpotGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_fx_spot_generation_config = fx_spot_generation_config;
    reverted_fx_spot_generation_config.version = 0;
    detailDialog->setConfig(reverted_fx_spot_generation_config);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::statusMessage,
            this,
            &FxSpotGenerationConfigController::statusMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::errorMessage,
            this,
            &FxSpotGenerationConfigController::errorMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::fx_spot_generation_configSaved,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "FX Spot Generation Config reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("FX Spot Generation Config '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert FX Spot Generation Config: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(fx_spot_generation_config.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* FxSpotGenerationConfigController::listWindow() const {
    return listWindow_;
}

}

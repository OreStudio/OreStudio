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
#include "ores.qt/MarketDataGenerationConfigController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MarketDataGenerationConfigDetailDialog.hpp"
#include "ores.qt/MarketDataGenerationConfigHistoryDialog.hpp"
#include "ores.qt/MarketDataGenerationConfigMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.synthetic.api/eventing/market_data_generation_config_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view market_data_generation_config_event_name =
    eventing::domain::event_traits<
        synthetic::eventing::market_data_generation_config_changed_event>::name;
}

MarketDataGenerationConfigController::MarketDataGenerationConfigController(
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
                       market_data_generation_config_event_name,
                       parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "MarketDataGenerationConfigController created";
}

void MarketDataGenerationConfigController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "market_data_generation_configs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new MarketDataGenerationConfigMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &MarketDataGenerationConfigMdiWindow::statusChanged,
            this,
            &MarketDataGenerationConfigController::statusMessage);
    connect(listWindow_,
            &MarketDataGenerationConfigMdiWindow::errorOccurred,
            this,
            &MarketDataGenerationConfigController::errorMessage);
    connect(listWindow_,
            &MarketDataGenerationConfigMdiWindow::showConfigDetails,
            this,
            &MarketDataGenerationConfigController::onShowDetails);
    connect(listWindow_,
            &MarketDataGenerationConfigMdiWindow::addNewRequested,
            this,
            &MarketDataGenerationConfigController::onAddNewRequested);
    connect(listWindow_,
            &MarketDataGenerationConfigMdiWindow::showConfigHistory,
            this,
            &MarketDataGenerationConfigController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Market Data Generation Configs");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Database, IconUtils::DefaultIconColor));
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
            [self = QPointer<MarketDataGenerationConfigController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Market Data Generation Config list window created";
}

void MarketDataGenerationConfigController::closeAllWindows() {
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

void MarketDataGenerationConfigController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void MarketDataGenerationConfigController::onShowDetails(
    const synthetic::domain::market_data_generation_config& market_data_generation_config) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: "
                               << boost::uuids::to_string(market_data_generation_config.id);
    showDetailWindow(market_data_generation_config);
}

void MarketDataGenerationConfigController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new market data generation config requested";
    showAddWindow();
}

void MarketDataGenerationConfigController::onShowHistory(
    const synthetic::domain::market_data_generation_config& market_data_generation_config) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(market_data_generation_config.id);
    showHistoryWindow(market_data_generation_config);
}

void MarketDataGenerationConfigController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new market data generation config";

    auto* detailDialog = new MarketDataGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::statusMessage,
            this,
            &MarketDataGenerationConfigController::statusMessage);
    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::errorMessage,
            this,
            &MarketDataGenerationConfigController::errorMessage);
    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::market_data_generation_configSaved,
            this,
            [self = QPointer<MarketDataGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Market Data Generation Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Market Data Generation Config");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Database, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void MarketDataGenerationConfigController::showDetailWindow(
    const synthetic::domain::market_data_generation_config& market_data_generation_config) {

    const QString identifier =
        QString::fromStdString(boost::uuids::to_string(market_data_generation_config.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(market_data_generation_config.id);

    auto* detailDialog = new MarketDataGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConfig(market_data_generation_config);

    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::statusMessage,
            this,
            &MarketDataGenerationConfigController::statusMessage);
    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::errorMessage,
            this,
            &MarketDataGenerationConfigController::errorMessage);
    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::market_data_generation_configSaved,
            this,
            [self = QPointer<MarketDataGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Market Data Generation Config saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(
        detailDialog,
        &MarketDataGenerationConfigDetailDialog::market_data_generation_configDeleted,
        this,
        [self = QPointer<MarketDataGenerationConfigController>(this), key](const QString& code) {
            if (!self)
                return;
            BOOST_LOG_SEV(lg(), info)
                << "Market Data Generation Config deleted: " << code.toStdString();
            self->handleEntityDeleted();
        });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Market Data Generation Config: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Database, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<MarketDataGenerationConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void MarketDataGenerationConfigController::showHistoryWindow(
    const synthetic::domain::market_data_generation_config& market_data_generation_config) {
    const QString code =
        QString::fromStdString(boost::uuids::to_string(market_data_generation_config.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for market data generation config: "
                              << boost::uuids::to_string(market_data_generation_config.id);

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << boost::uuids::to_string(market_data_generation_config.id);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << boost::uuids::to_string(market_data_generation_config.id);

    auto* historyDialog = new MarketDataGenerationConfigHistoryDialog(
        market_data_generation_config.id, code, clientManager_, mainWindow_);

    connect(historyDialog,
            &MarketDataGenerationConfigHistoryDialog::statusChanged,
            this,
            [self = QPointer<MarketDataGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &MarketDataGenerationConfigHistoryDialog::errorOccurred,
            this,
            [self = QPointer<MarketDataGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &MarketDataGenerationConfigHistoryDialog::revertVersionRequested,
            this,
            &MarketDataGenerationConfigController::onRevertVersion);
    connect(historyDialog,
            &MarketDataGenerationConfigHistoryDialog::openVersionRequested,
            this,
            &MarketDataGenerationConfigController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Market Data Generation Config History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<MarketDataGenerationConfigController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void MarketDataGenerationConfigController::onOpenVersion(
    const synthetic::domain::market_data_generation_config& market_data_generation_config,
    int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for market data generation config: "
                              << boost::uuids::to_string(market_data_generation_config.id);

    const QString code =
        QString::fromStdString(boost::uuids::to_string(market_data_generation_config.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new MarketDataGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConfig(market_data_generation_config);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::statusMessage,
            this,
            [self = QPointer<MarketDataGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::errorMessage,
            this,
            [self = QPointer<MarketDataGenerationConfigController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Market Data Generation Config: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<MarketDataGenerationConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void MarketDataGenerationConfigController::onRevertVersion(
    const synthetic::domain::market_data_generation_config& market_data_generation_config) {
    BOOST_LOG_SEV(lg(), info) << "Reverting market data generation config to version: "
                              << market_data_generation_config.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new MarketDataGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_market_data_generation_config = market_data_generation_config;
    reverted_market_data_generation_config.version = 0;
    detailDialog->setConfig(reverted_market_data_generation_config);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::statusMessage,
            this,
            &MarketDataGenerationConfigController::statusMessage);
    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::errorMessage,
            this,
            &MarketDataGenerationConfigController::errorMessage);
    connect(detailDialog,
            &MarketDataGenerationConfigDetailDialog::market_data_generation_configSaved,
            this,
            [self = QPointer<MarketDataGenerationConfigController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Market Data Generation Config reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Market Data Generation Config '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Market Data Generation Config: %1")
                                     .arg(QString::fromStdString(boost::uuids::to_string(
                                         market_data_generation_config.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* MarketDataGenerationConfigController::listWindow() const {
    return listWindow_;
}

}

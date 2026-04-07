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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/MktdataPlugin.hpp"

#include <QMenu>
#include <QAction>
#include <QMdiArea>
#include <QMdiSubWindow>
#include <QMainWindow>

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/DataLibrarianWindow.hpp"
#include "ores.qt/CurrencyMarketTierController.hpp"
#include "ores.qt/MarketDataController.hpp"
#include "ores.qt/PricingEngineTypeController.hpp"
#include "ores.qt/PricingModelConfigController.hpp"
#include "ores.qt/PricingModelProductController.hpp"
#include "ores.qt/PricingModelProductParameterController.hpp"

namespace ores::qt {

MktdataPlugin::MktdataPlugin(QObject* parent) : QObject(parent) {}

MktdataPlugin::~MktdataPlugin() = default;

// ---------------------------------------------------------------------------
// Helper: wire standard controller signals to MktdataPlugin forwarding slots.
// ---------------------------------------------------------------------------
void MktdataPlugin::connect_controller_signals(QObject* ctrl) {
    connect(ctrl, SIGNAL(statusMessage(const QString&)),
            this, SLOT(on_status_message(const QString&)));
    connect(ctrl, SIGNAL(errorMessage(const QString&)),
            this, SLOT(on_status_message(const QString&)));
    connect(ctrl, SIGNAL(detachableWindowCreated(DetachableMdiSubWindow*)),
            this, SLOT(on_window_created(DetachableMdiSubWindow*)));
    connect(ctrl, SIGNAL(detachableWindowDestroyed(DetachableMdiSubWindow*)),
            this, SLOT(on_window_destroyed(DetachableMdiSubWindow*)));
}

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers
// ---------------------------------------------------------------------------
void MktdataPlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

    currencyMarketTierController_ = std::make_unique<CurrencyMarketTierController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(currencyMarketTierController_.get());

    marketDataController_ = std::make_unique<MarketDataController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username);
    connect(marketDataController_.get(), &MarketDataController::statusMessage,
            this, &MktdataPlugin::status_message);
    connect(marketDataController_.get(), &MarketDataController::errorMessage,
            this, &MktdataPlugin::status_message);
    connect(marketDataController_.get(), &MarketDataController::detachableWindowCreated,
            this, &MktdataPlugin::window_created);
    connect(marketDataController_.get(), &MarketDataController::detachableWindowDestroyed,
            this, &MktdataPlugin::window_destroyed);

    pricingEngineTypeController_ = std::make_unique<PricingEngineTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connect_controller_signals(pricingEngineTypeController_.get());

    pricingModelConfigController_ = std::make_unique<PricingModelConfigController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connect_controller_signals(pricingModelConfigController_.get());

    pricingModelProductController_ = std::make_unique<PricingModelProductController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connect_controller_signals(pricingModelProductController_.get());

    pricingModelProductParameterController_ = std::make_unique<PricingModelProductParameterController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connect_controller_signals(pricingModelProductParameterController_.get());
}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — return Analytics and Market Data menus.
// ---------------------------------------------------------------------------
QList<QMenu*> MktdataPlugin::create_menus() {
    using IC = IconUtils;
    auto ico = [](Icon i) { return IC::createRecoloredIcon(i, IC::DefaultIconColor); };

    // ---- Analytics --------------------------------------------------
    auto* menuAnalytics = new QMenu(tr("&Analytics"));

    auto* actPricingEngineTypes = menuAnalytics->addAction(
        ico(Icon::Tag), tr("&Pricing Engine Types"));
    connect(actPricingEngineTypes, &QAction::triggered, this, [this]() {
        if (pricingEngineTypeController_) pricingEngineTypeController_->showListWindow();
    });
    menuAnalytics->addSeparator();
    auto* actPricingModelConfigs = menuAnalytics->addAction(
        ico(Icon::Chart), tr("Pricing &Model Configs"));
    connect(actPricingModelConfigs, &QAction::triggered, this, [this]() {
        if (pricingModelConfigController_) pricingModelConfigController_->showListWindow();
    });
    auto* actPricingModelProducts = menuAnalytics->addAction(
        ico(Icon::Table), tr("Pricing Model &Products"));
    connect(actPricingModelProducts, &QAction::triggered, this, [this]() {
        if (pricingModelProductController_) pricingModelProductController_->showListWindow();
    });
    auto* actPricingModelProductParameters = menuAnalytics->addAction(
        ico(Icon::Settings), tr("Pricing Model Product &Parameters"));
    connect(actPricingModelProductParameters, &QAction::triggered, this, [this]() {
        if (pricingModelProductParameterController_)
            pricingModelProductParameterController_->showListWindow();
    });

    // ---- Market Data ------------------------------------------------
    auto* menuMarketData = new QMenu(tr("&Market Data"));

    auto* actMarketSeries = menuMarketData->addAction(ico(Icon::ChartMultiple), tr("Market &Series"));
    connect(actMarketSeries, &QAction::triggered, this, [this]() {
        if (marketDataController_) marketDataController_->showListWindow();
    });
    auto* actMarketFixings = menuMarketData->addAction(ico(Icon::Chart), tr("Market &Fixings"));
    connect(actMarketFixings, &QAction::triggered, this, [this]() {
        if (marketDataController_) marketDataController_->showFixingsWindow();
    });
    menuMarketData->addSeparator();
    auto* actCurrencyMarketTiers = menuMarketData->addAction(
        ico(Icon::Chart), tr("Currency Market &Tiers"));
    connect(actCurrencyMarketTiers, &QAction::triggered, this, [this]() {
        if (currencyMarketTierController_) currencyMarketTierController_->showListWindow();
    });
    menuMarketData->addSeparator();

    // Assets submenu
    auto* menuAssets = menuMarketData->addMenu(tr("&Assets"));
    auto* actDataLibrarian = menuAssets->addAction(ico(Icon::Library), tr("Data &Librarian"));
    connect(actDataLibrarian, &QAction::triggered, this, [this]() {
        if (data_librarian_window_) {
            ctx_.mdi_area->setActiveSubWindow(
                qobject_cast<QMdiSubWindow*>(data_librarian_window_->parent()));
            return;
        }

        auto* librarianWindow = new DataLibrarianWindow(
            ctx_.client_manager, ctx_.username, ctx_.badge_cache, ctx_.main_window);

        auto* subWindow = new DetachableMdiSubWindow(ctx_.main_window);
        subWindow->setWidget(librarianWindow);
        subWindow->setWindowTitle(tr("Data Librarian"));
        subWindow->setWindowIcon(IconUtils::createRecoloredIcon(
            Icon::Library, IconUtils::DefaultIconColor));
        subWindow->setAttribute(Qt::WA_DeleteOnClose);

        connect(librarianWindow, &DataLibrarianWindow::statusChanged,
                this, [this](const QString& msg) { emit status_message(msg); });
        connect(librarianWindow, &DataLibrarianWindow::errorOccurred,
                this, [this](const QString& msg) { emit status_message(msg); });

        data_librarian_window_ = subWindow;
        connect(subWindow, &QObject::destroyed, this, [this]() {
            data_librarian_window_ = nullptr;
        });

        ctx_.mdi_area->addSubWindow(subWindow);
        subWindow->resize(librarianWindow->sizeHint());
        subWindow->show();
    });

    return {menuAnalytics, menuMarketData};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — close open singleton windows, destroy all controllers
// ---------------------------------------------------------------------------
void MktdataPlugin::on_logout() {
    if (data_librarian_window_) {
        data_librarian_window_->close();
        data_librarian_window_ = nullptr;
    }

    pricingModelProductParameterController_.reset();
    pricingModelProductController_.reset();
    pricingModelConfigController_.reset();
    pricingEngineTypeController_.reset();
    marketDataController_.reset();
    currencyMarketTierController_.reset();

    ctx_ = {};
}

} // namespace ores::qt

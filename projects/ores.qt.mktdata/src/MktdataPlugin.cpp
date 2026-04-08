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

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/CurrencyMarketTierController.hpp"
#include "ores.qt/MarketDataController.hpp"

namespace ores::qt {

MktdataPlugin::MktdataPlugin(QObject* parent) : PluginBase(parent) {}

MktdataPlugin::~MktdataPlugin() = default;

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers
// ---------------------------------------------------------------------------
void MktdataPlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

    currencyMarketTierController_ = std::make_unique<CurrencyMarketTierController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(currencyMarketTierController_.get());

    marketDataController_ = std::make_unique<MarketDataController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username);
    connect(marketDataController_.get(), &MarketDataController::statusMessage,
            this, &PluginBase::statusMessage);
    connect(marketDataController_.get(), &MarketDataController::errorMessage,
            this, &PluginBase::statusMessage);
    connect(marketDataController_.get(), &MarketDataController::detachableWindowCreated,
            this, &PluginBase::windowCreated);
    connect(marketDataController_.get(), &MarketDataController::detachableWindowDestroyed,
            this, &PluginBase::windowDestroyed);

}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — return the Market Data menu.
// ---------------------------------------------------------------------------
QList<QMenu*> MktdataPlugin::create_menus() {
    using IC = IconUtils;
    auto ico = [](Icon i) { return IC::createRecoloredIcon(i, IC::DefaultIconColor); };

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
    return {menuMarketData};
}

QList<QAction*> MktdataPlugin::toolbar_actions() {
    return {};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — close open singleton windows, destroy all controllers
// ---------------------------------------------------------------------------
void MktdataPlugin::on_logout() {
    marketDataController_.reset();
    currencyMarketTierController_.reset();

    ctx_ = {};
}

} // namespace ores::qt

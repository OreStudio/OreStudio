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
#include "ores.logging/make_logger.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MarketDataController.hpp"
#include <QAction>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.mktdata_plugin");
    return instance;
}
}

MktdataPlugin::MktdataPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

MktdataPlugin::~MktdataPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers
// ---------------------------------------------------------------------------
void MktdataPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    marketDataController_ = std::make_unique<MarketDataController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username);
    connect(marketDataController_.get(),
            &MarketDataController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(marketDataController_.get(),
            &MarketDataController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(marketDataController_.get(),
            &MarketDataController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(marketDataController_.get(),
            &MarketDataController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);
}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — return the Market Data menu.
// ---------------------------------------------------------------------------
void MktdataPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Populating shared Market Data menu.";
    marketDataMenu_ = smc.market_data_menu;
    if (!marketDataMenu_)
        return;

    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    auto* actMarketSeries =
        marketDataMenu_->addAction(ico(Icon::ChartMultiple), tr("Market &Series"));
    connect(actMarketSeries, &QAction::triggered, this, [this]() {
        if (marketDataController_)
            marketDataController_->showListWindow();
    });
    auto* actMarketFixings = marketDataMenu_->addAction(ico(Icon::Chart), tr("Market &Fixings"));
    connect(actMarketFixings, &QAction::triggered, this, [this]() {
        if (marketDataController_)
            marketDataController_->showFixingsWindow();
    });
}

QList<QMenu*> MktdataPlugin::create_menus() {
    // MktdataPlugin owns insertion of the shared Market Data menu into the bar;
    // SyntheticPlugin contributes additional entries via setup_menus().
    return marketDataMenu_ ? QList<QMenu*>{marketDataMenu_} : QList<QMenu*>{};
}

QList<QAction*> MktdataPlugin::toolbar_actions() {
    return {};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — close open singleton windows, destroy all controllers
// ---------------------------------------------------------------------------
void MktdataPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    marketDataController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

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
#include "ores.qt/SyntheticPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MarketDataGenerationConfigController.hpp"
#include <QAction>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.synthetic_plugin");
    return instance;
}
}

SyntheticPlugin::SyntheticPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

SyntheticPlugin::~SyntheticPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers
// ---------------------------------------------------------------------------
void SyntheticPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    configController_ = std::make_unique<MarketDataGenerationConfigController>(
        ctx_.main_window,
        ctx_.mdi_area,
        ctx_.client_manager,
        ctx_.change_reason_cache,
        ctx_.username);
    connect(configController_.get(),
            &MarketDataGenerationConfigController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(configController_.get(),
            &MarketDataGenerationConfigController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(configController_.get(),
            &MarketDataGenerationConfigController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(configController_.get(),
            &MarketDataGenerationConfigController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);
}

// ---------------------------------------------------------------------------
// IPlugin::setup_menus — contribute entries to the shared Market Data menu.
// ---------------------------------------------------------------------------
void SyntheticPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Populating shared Market Data menu.";
    marketDataMenu_ = smc.market_data_menu;
    if (!marketDataMenu_)
        return;

    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    auto* actConfigs =
        marketDataMenu_->addAction(ico(Icon::Chart), tr("Generation &Configs"));
    connect(actConfigs, &QAction::triggered, this, [this]() {
        if (configController_)
            configController_->showListWindow();
    });
}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — MktdataPlugin owns the shared Market Data menu.
// ---------------------------------------------------------------------------
QList<QMenu*> SyntheticPlugin::create_menus() {
    return {};
}

QList<QAction*> SyntheticPlugin::toolbar_actions() {
    return {};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — destroy all controllers
// ---------------------------------------------------------------------------
void SyntheticPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    configController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

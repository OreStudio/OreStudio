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
#include "ores.qt/MarketdataPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/CrmCrossRatesMatrixController.hpp"
#include "ores.qt/FeedBindingController.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QAction>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.marketdata_plugin");
    return instance;
}
}

MarketdataPlugin::MarketdataPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

MarketdataPlugin::~MarketdataPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void MarketdataPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    feedBindingController_ = std::make_unique<FeedBindingController>(ctx_.main_window,
                                                                     ctx_.mdi_area,
                                                                     ctx_.client_manager,
                                                                     ctx_.change_reason_cache,
                                                                     ctx_.username);

    connect(feedBindingController_.get(),
            &FeedBindingController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(feedBindingController_.get(),
            &FeedBindingController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(feedBindingController_.get(),
            &FeedBindingController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(feedBindingController_.get(),
            &FeedBindingController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);

    crmCrossRatesMatrixController_ = std::make_unique<CrmCrossRatesMatrixController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache);

    connect(crmCrossRatesMatrixController_.get(),
            &CrmCrossRatesMatrixController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(crmCrossRatesMatrixController_.get(),
            &CrmCrossRatesMatrixController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(crmCrossRatesMatrixController_.get(),
            &CrmCrossRatesMatrixController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(crmCrossRatesMatrixController_.get(),
            &CrmCrossRatesMatrixController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);
}

void MarketdataPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Populating shared Market Data menu.";
    marketDataMenu_ = smc.market_data_menu;
    if (!marketDataMenu_)
        return;

    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    marketDataMenu_->addSeparator();
    auto* actFeedBindings = marketDataMenu_->addAction(ico(Icon::ServerLink), tr("Feed &Bindings"));
    connect(actFeedBindings, &QAction::triggered, this, [this]() {
        if (feedBindingController_)
            feedBindingController_->showListWindow();
    });

    auto* actCrmMatrix =
        marketDataMenu_->addAction(ico(Icon::Currency), tr("Cross-&Rates Matrix"));
    connect(actCrmMatrix, &QAction::triggered, this, [this]() {
        if (crmCrossRatesMatrixController_)
            crmCrossRatesMatrixController_->showMatrix();
    });
}

QList<QMenu*> MarketdataPlugin::create_menus() {
    return {};
}

QList<QAction*> MarketdataPlugin::toolbar_actions() {
    return {};
}

void MarketdataPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    feedBindingController_.reset();
    crmCrossRatesMatrixController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

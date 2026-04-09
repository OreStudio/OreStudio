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
#include "ores.qt/AnalyticsPlugin.hpp"

#include <QMenu>
#include <QAction>

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PricingEngineTypeController.hpp"
#include "ores.qt/PricingModelConfigController.hpp"
#include "ores.qt/PricingModelProductController.hpp"
#include "ores.qt/PricingModelProductParameterController.hpp"

namespace ores::qt {

namespace {
auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}
}

AnalyticsPlugin::AnalyticsPlugin(QObject* parent) : PluginBase(parent) {}

AnalyticsPlugin::~AnalyticsPlugin() = default;

void AnalyticsPlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

    pricingEngineTypeController_ = std::make_unique<PricingEngineTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(pricingEngineTypeController_.get());

    pricingModelConfigController_ = std::make_unique<PricingModelConfigController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(pricingModelConfigController_.get());

    pricingModelProductController_ = std::make_unique<PricingModelProductController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(pricingModelProductController_.get());

    pricingModelProductParameterController_ =
        std::make_unique<PricingModelProductParameterController>(
            ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(pricingModelProductParameterController_.get());
}

QList<QMenu*> AnalyticsPlugin::create_menus() {
    auto* menuAnalytics = new QMenu(tr("&Analytics"));

    // ---- Pricing Models submenu -----------------------------------------
    auto* menuPricingModels = menuAnalytics->addMenu(tr("&Pricing Models"));

    auto* actPricingEngineTypes = menuPricingModels->addAction(
        ico(Icon::Tag), tr("&Pricing Engine Types"));
    connect(actPricingEngineTypes, &QAction::triggered, this, [this]() {
        if (pricingEngineTypeController_)
            pricingEngineTypeController_->showListWindow();
    });

    // ---- Configuration submenu ------------------------------------------
    auto* menuConfig = menuAnalytics->addMenu(tr("&Configuration"));

    auto* actModelConfigs = menuConfig->addAction(
        ico(Icon::Chart), tr("Model &Configurations"));
    connect(actModelConfigs, &QAction::triggered, this, [this]() {
        if (pricingModelConfigController_)
            pricingModelConfigController_->showListWindow();
    });

    auto* actModelProducts = menuConfig->addAction(
        ico(Icon::Table), tr("Model &Products"));
    connect(actModelProducts, &QAction::triggered, this, [this]() {
        if (pricingModelProductController_)
            pricingModelProductController_->showListWindow();
    });

    auto* actModelProductParameters = menuConfig->addAction(
        ico(Icon::Settings), tr("Model Product &Parameters"));
    connect(actModelProductParameters, &QAction::triggered, this, [this]() {
        if (pricingModelProductParameterController_)
            pricingModelProductParameterController_->showListWindow();
    });

    return {menuAnalytics};
}

void AnalyticsPlugin::on_logout() {
    pricingModelProductParameterController_.reset();
    pricingModelProductController_.reset();
    pricingModelConfigController_.reset();
    pricingEngineTypeController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

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
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FxSpotGenerationConfigController.hpp"
#include "ores.qt/GmmComponentController.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/IrCurveGenerationConfigController.hpp"
#include "ores.qt/IrCurveTemplateEntryController.hpp"
#include "ores.qt/MarketDataGenerationConfigController.hpp"
#include "ores.qt/MarketSimulatorWindow.hpp"
#include "ores.qt/YieldCurveProcessTypeController.hpp"
#include <QAction>
#include <QMdiArea>
#include <QMdiSubWindow>
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

    configController_ =
        std::make_unique<MarketDataGenerationConfigController>(ctx_.main_window,
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

    fxSpotConfigController_ =
        std::make_unique<FxSpotGenerationConfigController>(ctx_.main_window,
                                                           ctx_.mdi_area,
                                                           ctx_.client_manager,
                                                           ctx_.image_cache,
                                                           ctx_.change_reason_cache,
                                                           ctx_.username);
    connect(fxSpotConfigController_.get(),
            &FxSpotGenerationConfigController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(fxSpotConfigController_.get(),
            &FxSpotGenerationConfigController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(fxSpotConfigController_.get(),
            &FxSpotGenerationConfigController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(fxSpotConfigController_.get(),
            &FxSpotGenerationConfigController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);

    gmmComponentController_ = std::make_unique<GmmComponentController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.username);
    connect(gmmComponentController_.get(),
            &GmmComponentController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(gmmComponentController_.get(),
            &GmmComponentController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(gmmComponentController_.get(),
            &GmmComponentController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(gmmComponentController_.get(),
            &GmmComponentController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);

    irCurveGenerationConfigController_ =
        std::make_unique<IrCurveGenerationConfigController>(ctx_.main_window,
                                                            ctx_.mdi_area,
                                                            ctx_.client_manager,
                                                            ctx_.change_reason_cache,
                                                            ctx_.username);
    connect(irCurveGenerationConfigController_.get(),
            &IrCurveGenerationConfigController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(irCurveGenerationConfigController_.get(),
            &IrCurveGenerationConfigController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(irCurveGenerationConfigController_.get(),
            &IrCurveGenerationConfigController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(irCurveGenerationConfigController_.get(),
            &IrCurveGenerationConfigController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);

    yieldCurveProcessTypeController_ =
        std::make_unique<YieldCurveProcessTypeController>(ctx_.main_window,
                                                          ctx_.mdi_area,
                                                          ctx_.client_manager,
                                                          ctx_.change_reason_cache,
                                                          ctx_.username);
    connect(yieldCurveProcessTypeController_.get(),
            &YieldCurveProcessTypeController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(yieldCurveProcessTypeController_.get(),
            &YieldCurveProcessTypeController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(yieldCurveProcessTypeController_.get(),
            &YieldCurveProcessTypeController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(yieldCurveProcessTypeController_.get(),
            &YieldCurveProcessTypeController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);

    irCurveTemplateEntryController_ =
        std::make_unique<IrCurveTemplateEntryController>(ctx_.main_window,
                                                         ctx_.mdi_area,
                                                         ctx_.client_manager,
                                                         ctx_.change_reason_cache,
                                                         ctx_.username);
    connect(irCurveTemplateEntryController_.get(),
            &IrCurveTemplateEntryController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(irCurveTemplateEntryController_.get(),
            &IrCurveTemplateEntryController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(irCurveTemplateEntryController_.get(),
            &IrCurveTemplateEntryController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(irCurveTemplateEntryController_.get(),
            &IrCurveTemplateEntryController::detachableWindowDestroyed,
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

    // Group the synthetic data generation entries under their own submenu so
    // they read as distinct from the real market data entries above.
    marketDataMenu_->addSeparator();
    auto* syntheticMenu = marketDataMenu_->addMenu(ico(Icon::Chart), tr("&Synthetic"));

    // Primary entry: the one-stop Market Simulator authoring window.
    auto* actSimulator = syntheticMenu->addAction(ico(Icon::Chart), tr("&Market Simulator"));
    connect(actSimulator, &QAction::triggered, this, [this]() {
        if (!ctx_.mdi_area)
            return;
        if (marketSimulatorWindow_) {
            ctx_.mdi_area->setActiveSubWindow(marketSimulatorWindow_);
            return;
        }
        auto* window = new MarketSimulatorWindow(ctx_.client_manager,
                                                 ctx_.username,
                                                 ctx_.image_cache,
                                                 ctx_.change_reason_cache,
                                                 ctx_.main_window);
        auto* subWindow = new DetachableMdiSubWindow(ctx_.main_window);
        subWindow->setWidget(window);
        subWindow->setWindowTitle(tr("Market Simulator"));
        subWindow->setWindowIcon(
            IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
        subWindow->setAttribute(Qt::WA_DeleteOnClose);
        connect(window, &MarketSimulatorWindow::statusChanged, this, [this](const QString& msg) {
            emit statusMessage(msg);
        });
        connect(window, &MarketSimulatorWindow::errorOccurred, this, [this](const QString& msg) {
            emit statusMessage(msg);
        });
        marketSimulatorWindow_ = subWindow;
        connect(
            subWindow, &QObject::destroyed, this, [this]() { marketSimulatorWindow_ = nullptr; });
        ctx_.mdi_area->addSubWindow(subWindow);
        subWindow->resize(window->sizeHint());
        subWindow->show();
    });

    syntheticMenu->addSeparator();
    auto* advancedMenu = syntheticMenu->addMenu(tr("&Advanced (raw tables)"));

    auto* actConfigs = advancedMenu->addAction(ico(Icon::Chart), tr("Generation &Configs"));
    connect(actConfigs, &QAction::triggered, this, [this]() {
        if (configController_)
            configController_->showListWindow();
    });

    auto* actFxSpot = advancedMenu->addAction(ico(Icon::Chart), tr("&FX Spot Configs"));
    connect(actFxSpot, &QAction::triggered, this, [this]() {
        if (fxSpotConfigController_)
            fxSpotConfigController_->showListWindow();
    });

    auto* actGmm = advancedMenu->addAction(ico(Icon::Chart), tr("&GMM Components"));
    connect(actGmm, &QAction::triggered, this, [this]() {
        if (gmmComponentController_)
            gmmComponentController_->showListWindow();
    });

    auto* actIrCurveConfigs =
        advancedMenu->addAction(ico(Icon::Chart), tr("IR Curve &Generation Configs"));
    connect(actIrCurveConfigs, &QAction::triggered, this, [this]() {
        if (irCurveGenerationConfigController_)
            irCurveGenerationConfigController_->showListWindow();
    });

    auto* actIrCurveTemplateEntries =
        advancedMenu->addAction(ico(Icon::Table), tr("IR Curve &Template Entries"));
    connect(actIrCurveTemplateEntries, &QAction::triggered, this, [this]() {
        if (irCurveTemplateEntryController_)
            irCurveTemplateEntryController_->showListWindow();
    });

    auto* actYieldCurveProcessTypes =
        advancedMenu->addAction(ico(Icon::Tag), tr("&Yield Curve Process Types"));
    connect(actYieldCurveProcessTypes, &QAction::triggered, this, [this]() {
        if (yieldCurveProcessTypeController_)
            yieldCurveProcessTypeController_->showListWindow();
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
    if (marketSimulatorWindow_) {
        marketSimulatorWindow_->close();
        marketSimulatorWindow_ = nullptr;
    }
    irCurveTemplateEntryController_.reset();
    yieldCurveProcessTypeController_.reset();
    irCurveGenerationConfigController_.reset();
    gmmComponentController_.reset();
    fxSpotConfigController_.reset();
    configController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

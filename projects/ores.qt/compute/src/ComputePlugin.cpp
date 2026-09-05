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
#include "ores.qt/ComputePlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/AppController.hpp"
#include "ores.qt/AppVersionController.hpp"
#include "ores.qt/ComputeConsoleController.hpp"
#include "ores.qt/ComputeDashboardController.hpp"
#include "ores.qt/ConcurrencyPolicyController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/QueueMonitorController.hpp"
#include "ores.qt/ReportDefinitionController.hpp"
#include "ores.qt/ReportInstanceController.hpp"
#include "ores.qt/ReportTypeController.hpp"
#include <QAction>
#include <QMainWindow>
#include <QMdiArea>
#include <QMenu>
#include <QStatusBar>

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.compute_plugin");
    return instance;
}

auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}

}

ComputePlugin::ComputePlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

ComputePlugin::~ComputePlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void ComputePlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    if (messageQueueMenu_)
        messageQueueMenu_->setEnabled(true);

    BOOST_LOG_SEV(lg(), info) << "on_login ctx http_base_url='"
                              << (ctx_.http_base_url.empty() ? "(empty)" : ctx_.http_base_url)
                              << "'";

    appController_ = std::make_unique<AppController>(ctx_.main_window,
                                                     ctx_.mdi_area,
                                                     ctx_.client_manager,
                                                     ctx_.change_reason_cache,
                                                     ctx_.username,
                                                     this);
    connectControllerSignals(appController_.get());

    appVersionController_ = std::make_unique<AppVersionController>(ctx_.main_window,
                                                                   ctx_.mdi_area,
                                                                   ctx_.client_manager,
                                                                   ctx_.change_reason_cache,
                                                                   ctx_.username,
                                                                   this);
    connectControllerSignals(appVersionController_.get());

    computeDashboardController_ = std::make_unique<ComputeDashboardController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, this);
    connect(computeDashboardController_.get(),
            &ComputeDashboardController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(computeDashboardController_.get(),
            &ComputeDashboardController::errorMessage,
            this,
            [this](const QString& msg) { emit statusMessage(msg); });
    connect(computeDashboardController_.get(),
            &ComputeDashboardController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(computeDashboardController_.get(),
            &ComputeDashboardController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);

    computeConsoleController_ = std::make_unique<ComputeConsoleController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.badge_cache,
                                                                           this);
    if (!ctx_.http_base_url.empty())
        computeConsoleController_->setHttpBaseUrl(ctx_.http_base_url);
    connect(computeConsoleController_.get(),
            &ComputeConsoleController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(computeConsoleController_.get(),
            &ComputeConsoleController::errorMessage,
            this,
            [this](const QString& msg) { emit statusMessage(msg); });
    connect(computeConsoleController_.get(),
            &ComputeConsoleController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(computeConsoleController_.get(),
            &ComputeConsoleController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);

    queueMonitorController_ = std::make_unique<QueueMonitorController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(queueMonitorController_.get());

    reportTypeController_ = std::make_unique<ReportTypeController>(ctx_.main_window,
                                                                   ctx_.mdi_area,
                                                                   ctx_.client_manager,
                                                                   ctx_.change_reason_cache,
                                                                   ctx_.username,
                                                                   this);
    connectControllerSignals(reportTypeController_.get());

    concurrencyPolicyController_ =
        std::make_unique<ConcurrencyPolicyController>(ctx_.main_window,
                                                      ctx_.mdi_area,
                                                      ctx_.client_manager,
                                                      ctx_.change_reason_cache,
                                                      ctx_.username,
                                                      this);
    connectControllerSignals(concurrencyPolicyController_.get());

    reportDefinitionController_ =
        std::make_unique<ReportDefinitionController>(ctx_.main_window,
                                                     ctx_.mdi_area,
                                                     ctx_.client_manager,
                                                     ctx_.change_reason_cache,
                                                     ctx_.badge_cache,
                                                     ctx_.username,
                                                     this);
    connectControllerSignals(reportDefinitionController_.get());

    reportInstanceController_ = std::make_unique<ReportInstanceController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.username,
                                                                           this);
    connectControllerSignals(reportInstanceController_.get());
}

void ComputePlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
                               << " reporting=" << (smc.reporting_menu ? "ok" : "null")
                               << " report_config="
                               << (smc.report_configuration_menu ? "ok" : "null")
                               << " compute_config="
                               << (smc.compute_configuration_menu ? "ok" : "null")
                               << " message_queue=" << (smc.message_queue_menu ? "ok" : "null");

    // ---- Reporting menu: compute infrastructure (how) ------------------
    if (smc.reporting_menu) {
        reporting_menu_ = smc.reporting_menu;

        auto* actConsole =
            reporting_menu_->addAction(ico(Icon::ServerLink), tr("&Compute Console"));
        connect(actConsole, &QAction::triggered, this, [this]() {
            if (computeConsoleController_)
                computeConsoleController_->showConsole();
        });

        auto* actDashboard = reporting_menu_->addAction(ico(Icon::Chart), tr("Compute &Dashboard"));
        connect(actDashboard, &QAction::triggered, this, [this]() {
            if (computeDashboardController_)
                computeDashboardController_->showDashboard();
        });

        // Compute Configuration submenu — attached here so it sits under
        // the compute items, before the section separator.
        if (smc.compute_configuration_menu) {
            compute_configuration_menu_ = smc.compute_configuration_menu;
            reporting_menu_->addMenu(compute_configuration_menu_);
        }

        reporting_menu_->addSeparator();

        act_report_definitions_ =
            reporting_menu_->addAction(ico(Icon::ChartMultiple), tr("Report &Definitions"));
        connect(act_report_definitions_, &QAction::triggered, this, [this]() {
            if (reportDefinitionController_)
                reportDefinitionController_->showListWindow();
        });

        act_report_instances_ =
            reporting_menu_->addAction(ico(Icon::Record), tr("Report &Instances"));
        connect(act_report_instances_, &QAction::triggered, this, [this]() {
            if (reportInstanceController_)
                reportInstanceController_->showListWindow();
        });

        // Report Configuration submenu — attached here so it sits under
        // the report items, after the section separator.
        if (smc.report_configuration_menu) {
            report_configuration_menu_ = smc.report_configuration_menu;
            reporting_menu_->addMenu(report_configuration_menu_);
        }
    }

    // ---- Compute Configuration submenu: apps + app versions -------------
    if (smc.compute_configuration_menu) {
        auto* actAppVersions =
            smc.compute_configuration_menu->addAction(ico(Icon::TasksApp), tr("App &Versions"));
        connect(actAppVersions, &QAction::triggered, this, [this]() {
            if (appVersionController_)
                appVersionController_->showListWindow();
        });

        auto* actApps = smc.compute_configuration_menu->addAction(ico(Icon::TasksApp), tr("&Apps"));
        connect(actApps, &QAction::triggered, this, [this]() {
            if (appController_)
                appController_->showListWindow();
        });
    }

    // ---- Report Configuration submenu: report types + concurrency policies
    if (smc.report_configuration_menu) {
        auto* actConcurrencyPolicies = smc.report_configuration_menu->addAction(
            ico(Icon::Settings), tr("&Concurrency Policies"));
        connect(actConcurrencyPolicies, &QAction::triggered, this, [this]() {
            if (concurrencyPolicyController_)
                concurrencyPolicyController_->showListWindow();
        });

        auto* actReportTypes =
            smc.report_configuration_menu->addAction(ico(Icon::Chart), tr("Report &Types"));
        connect(actReportTypes, &QAction::triggered, this, [this]() {
            if (reportTypeController_)
                reportTypeController_->showListWindow();
        });
    }

    // ---- Operations > Message Queue ---------------------------------------
    if (smc.message_queue_menu && smc.telemetry_menu) {
        auto* msgQueue = smc.message_queue_menu;
        messageQueueMenu_ = msgQueue;
        messageQueueMenu_->setEnabled(false); // enabled on login

        auto* actQueueMonitor = msgQueue->addAction(ico(Icon::Server), tr("&Queue Monitor"));
        connect(actQueueMonitor, &QAction::triggered, this, [this]() {
            if (queueMonitorController_)
                queueMonitorController_->showListWindow();
        });
    }
}

QList<QMenu*> ComputePlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "All items contributed via setup_menus — no standalone menus.";
    return {};
}

QList<QAction*> ComputePlugin::toolbar_actions() {
    if (!act_report_definitions_ || !act_report_instances_)
        BOOST_LOG_SEV(lg(), warn) << "One or more toolbar actions are uninitialised.";
    return {act_report_definitions_, act_report_instances_};
}

void ComputePlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    reportInstanceController_.reset();
    reportDefinitionController_.reset();
    concurrencyPolicyController_.reset();
    reportTypeController_.reset();
    queueMonitorController_.reset();
    computeConsoleController_.reset();
    computeDashboardController_.reset();
    appVersionController_.reset();
    appController_.reset();
    ctx_ = {};

    if (messageQueueMenu_)
        messageQueueMenu_->setEnabled(false);
}

} // namespace ores::qt

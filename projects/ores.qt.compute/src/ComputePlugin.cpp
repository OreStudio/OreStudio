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

#include <QMenu>
#include <QAction>
#include <QMdiArea>
#include <QMainWindow>
#include <QStatusBar>
#include "ores.logging/make_logger.hpp"

#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/AppController.hpp"
#include "ores.qt/AppVersionController.hpp"
#include "ores.qt/ComputeDashboardController.hpp"
#include "ores.qt/ComputeConsoleController.hpp"
#include "ores.qt/ServiceDashboardController.hpp"
#include "ores.qt/QueueMonitorController.hpp"
#include "ores.qt/ReportTypeController.hpp"
#include "ores.qt/ConcurrencyPolicyController.hpp"
#include "ores.qt/ReportDefinitionController.hpp"
#include "ores.qt/ReportInstanceController.hpp"

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

ComputePlugin::ComputePlugin(QObject* parent) : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

ComputePlugin::~ComputePlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void ComputePlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    BOOST_LOG_SEV(lg(), info)
        << "on_login ctx http_base_url='"
        << (ctx_.http_base_url.empty() ? "(empty)" : ctx_.http_base_url)
        << "'";

    appController_ = std::make_unique<AppController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(appController_.get());

    appVersionController_ = std::make_unique<AppVersionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    if (!ctx_.http_base_url.empty())
        appVersionController_->setHttpBaseUrl(ctx_.http_base_url);
    connectControllerSignals(appVersionController_.get());

    computeDashboardController_ = std::make_unique<ComputeDashboardController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, this);
    connect(computeDashboardController_.get(), &ComputeDashboardController::statusMessage,
            this, &PluginBase::statusMessage);
    connect(computeDashboardController_.get(), &ComputeDashboardController::errorMessage,
            this, [this](const QString& msg) { emit statusMessage(msg); });
    connect(computeDashboardController_.get(), &ComputeDashboardController::detachableWindowCreated,
            this, &PluginBase::windowCreated);
    connect(computeDashboardController_.get(), &ComputeDashboardController::detachableWindowDestroyed,
            this, &PluginBase::windowDestroyed);

    computeConsoleController_ = std::make_unique<ComputeConsoleController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.badge_cache, this);
    if (!ctx_.http_base_url.empty())
        computeConsoleController_->setHttpBaseUrl(ctx_.http_base_url);
    connect(computeConsoleController_.get(), &ComputeConsoleController::statusMessage,
            this, &PluginBase::statusMessage);
    connect(computeConsoleController_.get(), &ComputeConsoleController::errorMessage,
            this, [this](const QString& msg) { emit statusMessage(msg); });
    connect(computeConsoleController_.get(), &ComputeConsoleController::detachableWindowCreated,
            this, &PluginBase::windowCreated);
    connect(computeConsoleController_.get(), &ComputeConsoleController::detachableWindowDestroyed,
            this, &PluginBase::windowDestroyed);

    serviceDashboardController_ = std::make_unique<ServiceDashboardController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, this);
    connect(serviceDashboardController_.get(), &ServiceDashboardController::statusMessage,
            this, &PluginBase::statusMessage);
    connect(serviceDashboardController_.get(), &ServiceDashboardController::errorMessage,
            this, [this](const QString& msg) { emit statusMessage(msg); });
    connect(serviceDashboardController_.get(), &ServiceDashboardController::detachableWindowCreated,
            this, &PluginBase::windowCreated);
    connect(serviceDashboardController_.get(), &ServiceDashboardController::detachableWindowDestroyed,
            this, &PluginBase::windowDestroyed);

    queueMonitorController_ = std::make_unique<QueueMonitorController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(queueMonitorController_.get());

    reportTypeController_ = std::make_unique<ReportTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(reportTypeController_.get());

    concurrencyPolicyController_ = std::make_unique<ConcurrencyPolicyController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(concurrencyPolicyController_.get());

    reportDefinitionController_ = std::make_unique<ReportDefinitionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connectControllerSignals(reportDefinitionController_.get());

    reportInstanceController_ = std::make_unique<ReportInstanceController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(reportInstanceController_.get());
}

void ComputePlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
        << " system=" << (smc.system_menu ? "ok" : "null")
        << " telemetry=" << (smc.telemetry_menu ? "ok" : "null");
    if (!smc.system_menu || !smc.telemetry_menu)
        return;

    auto* telemetryAction = smc.telemetry_menu->menuAction();

    // ---- System > Message Queue (before Telemetry) -----------------------
    auto* msgQueue = new QMenu(tr("&Message Queue"), smc.system_menu);
    smc.system_menu->insertMenu(telemetryAction, msgQueue);

    auto* actQueueMonitor = msgQueue->addAction(ico(Icon::Server), tr("&Queue Monitor"));
    connect(actQueueMonitor, &QAction::triggered, this, [this]() {
        if (queueMonitorController_) queueMonitorController_->showListWindow();
    });

    // ---- System > Telemetry > Service Dashboard (first item) ------------
    auto* firstTelemetryAction = smc.telemetry_menu->actions().isEmpty()
        ? nullptr : smc.telemetry_menu->actions().first();

    auto* actServiceDashboard = new QAction(ico(Icon::Chart), tr("&Service Dashboard..."), this);
    connect(actServiceDashboard, &QAction::triggered, this, [this]() {
        if (serviceDashboardController_) serviceDashboardController_->showDashboard();
    });

    if (firstTelemetryAction) {
        smc.telemetry_menu->insertAction(firstTelemetryAction, actServiceDashboard);
        auto* sep = new QAction(this);
        sep->setSeparator(true);
        smc.telemetry_menu->insertAction(firstTelemetryAction, sep);
    } else {
        smc.telemetry_menu->addAction(actServiceDashboard);
    }
}

QList<QMenu*> ComputePlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Building plugin menus.";
    QList<QMenu*> menus;

    // ---- Compute --------------------------------------------------------
    auto* menuCompute = new QMenu(tr("&Compute"));

    auto* actDashboard = menuCompute->addAction(ico(Icon::Chart), tr("&Dashboard"));
    connect(actDashboard, &QAction::triggered, this, [this]() {
        if (computeDashboardController_) computeDashboardController_->showDashboard();
    });

    auto* actConsole = menuCompute->addAction(ico(Icon::ServerLink), tr("&Console"));
    connect(actConsole, &QAction::triggered, this, [this]() {
        if (computeConsoleController_) computeConsoleController_->showConsole();
    });

    menuCompute->addSeparator();

    auto* actApps = menuCompute->addAction(ico(Icon::TasksApp), tr("&Apps"));
    connect(actApps, &QAction::triggered, this, [this]() {
        if (appController_) appController_->showListWindow();
    });

    auto* actAppVersions = menuCompute->addAction(ico(Icon::TasksApp), tr("App &Versions"));
    connect(actAppVersions, &QAction::triggered, this, [this]() {
        if (appVersionController_) appVersionController_->showListWindow();
    });

    // ---- Reporting (inserted before Compute in the menu bar) ------------
    auto* menuReporting = new QMenu(tr("&Reporting"));

    act_report_definitions_ = menuReporting->addAction(
        ico(Icon::ChartMultiple), tr("Report &Definitions"));
    connect(act_report_definitions_, &QAction::triggered, this, [this]() {
        if (reportDefinitionController_) reportDefinitionController_->showListWindow();
    });

    act_report_instances_ = menuReporting->addAction(ico(Icon::Record), tr("Report &Instances"));
    connect(act_report_instances_, &QAction::triggered, this, [this]() {
        if (reportInstanceController_) reportInstanceController_->showListWindow();
    });

    menuReporting->addSeparator();

    // ---- Reporting Codes submenu ----------------------------------------
    auto* menuReportingCodes = menuReporting->addMenu(tr("Reporting &Codes"));

    auto* actReportTypes = menuReportingCodes->addAction(ico(Icon::Chart), tr("Report &Types"));
    connect(actReportTypes, &QAction::triggered, this, [this]() {
        if (reportTypeController_) reportTypeController_->showListWindow();
    });

    auto* actConcurrencyPolicies = menuReportingCodes->addAction(
        ico(Icon::Settings), tr("&Concurrency Policies"));
    connect(actConcurrencyPolicies, &QAction::triggered, this, [this]() {
        if (concurrencyPolicyController_) concurrencyPolicyController_->showListWindow();
    });

    // Return Reporting before Compute so Reporting appears first in the menu bar.
    menus.append(menuReporting);
    menus.append(menuCompute);

    BOOST_LOG_SEV(lg(), debug) << "Plugin menus ready.";
    return menus;
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
    serviceDashboardController_.reset();
    computeConsoleController_.reset();
    computeDashboardController_.reset();
    appVersionController_.reset();
    appController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

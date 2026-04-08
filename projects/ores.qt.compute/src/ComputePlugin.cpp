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

#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ComputeDashboardController.hpp"
#include "ores.qt/ComputeConsoleController.hpp"
#include "ores.qt/ServiceDashboardController.hpp"
#include "ores.qt/JobDefinitionController.hpp"
#include "ores.qt/QueueMonitorController.hpp"
#include "ores.qt/ReportTypeController.hpp"
#include "ores.qt/ConcurrencyPolicyController.hpp"
#include "ores.qt/ReportDefinitionController.hpp"
#include "ores.qt/ReportInstanceController.hpp"
#include "ores.qt/OreImportController.hpp"

namespace ores::qt {

namespace {
auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}
}

ComputePlugin::ComputePlugin(QObject* parent) : PluginBase(parent) {}

ComputePlugin::~ComputePlugin() = default;

void ComputePlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

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

    jobDefinitionController_ = std::make_unique<JobDefinitionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username,
        ctx_.change_reason_cache, this);
    connectControllerSignals(jobDefinitionController_.get());

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

    oreImportController_ = std::make_unique<OreImportController>(
        ctx_.client_manager, this);
    connect(oreImportController_.get(), &OreImportController::statusMessage,
            this, &PluginBase::statusMessage);
}

QList<QMenu*> ComputePlugin::create_menus() {
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

    auto* actQueues = menuCompute->addAction(ico(Icon::Server), tr("&Queues"));
    connect(actQueues, &QAction::triggered, this, [this]() {
        if (queueMonitorController_) queueMonitorController_->showListWindow();
    });

    menuCompute->addSeparator();

    auto* actServiceDashboard = menuCompute->addAction(ico(Icon::Server), tr("&Service Dashboard"));
    connect(actServiceDashboard, &QAction::triggered, this, [this]() {
        if (serviceDashboardController_) serviceDashboardController_->showDashboard();
    });

    menuCompute->addSeparator();

    auto* actOreImport = menuCompute->addAction(ico(Icon::ImportOre), tr("&Import ORE Data"));
    connect(actOreImport, &QAction::triggered, this, [this]() {
        if (oreImportController_) oreImportController_->trigger(ctx_.main_window);
    });

    // ---- Reporting (inserted before Compute in the menu bar) ------------
    auto* menuReporting = new QMenu(tr("&Reporting"));

    auto* actReportTypes = menuReporting->addAction(ico(Icon::Chart), tr("Report &Types"));
    connect(actReportTypes, &QAction::triggered, this, [this]() {
        if (reportTypeController_) reportTypeController_->showListWindow();
    });

    auto* actConcurrencyPolicies = menuReporting->addAction(
        ico(Icon::Settings), tr("&Concurrency Policies"));
    connect(actConcurrencyPolicies, &QAction::triggered, this, [this]() {
        if (concurrencyPolicyController_) concurrencyPolicyController_->showListWindow();
    });

    menuReporting->addSeparator();

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

    auto* actJobDefs = menuReporting->addAction(ico(Icon::TasksApp), tr("&Job Definitions"));
    connect(actJobDefs, &QAction::triggered, this, [this]() {
        if (jobDefinitionController_) jobDefinitionController_->showListWindow();
    });

    menus.append(menuReporting);
    menus.append(menuCompute);

    return menus;
}

QList<QAction*> ComputePlugin::toolbar_actions() {
    return {act_report_definitions_, act_report_instances_};
}

void ComputePlugin::on_logout() {
    oreImportController_.reset();
    reportInstanceController_.reset();
    reportDefinitionController_.reset();
    concurrencyPolicyController_.reset();
    reportTypeController_.reset();
    queueMonitorController_.reset();
    jobDefinitionController_.reset();
    serviceDashboardController_.reset();
    computeConsoleController_.reset();
    computeDashboardController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

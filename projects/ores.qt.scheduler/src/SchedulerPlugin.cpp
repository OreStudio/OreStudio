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
#include "ores.qt/SchedulerPlugin.hpp"

#include <QMenu>
#include <QAction>

#include "ores.qt/IconUtils.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/JobDefinitionController.hpp"
#include "ores.qt/JobInstanceController.hpp"
#include "ores.qt/SchedulerMonitorController.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.scheduler_plugin");
    return instance;
}

auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}

}

SchedulerPlugin::SchedulerPlugin(QObject* parent) : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

SchedulerPlugin::~SchedulerPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void SchedulerPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    jobDefinitionController_ = std::make_unique<JobDefinitionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username,
        ctx_.change_reason_cache, this);
    connectControllerSignals(jobDefinitionController_.get());

    jobInstanceController_ = std::make_unique<JobInstanceController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, this);
    connectControllerSignals(jobInstanceController_.get());

    schedulerMonitorController_ = std::make_unique<SchedulerMonitorController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, this);
    connect(schedulerMonitorController_.get(), &SchedulerMonitorController::statusMessage,
            this, &SchedulerPlugin::statusMessage);
    connect(schedulerMonitorController_.get(), &SchedulerMonitorController::errorMessage,
            this, &SchedulerPlugin::statusMessage);
}

QList<QMenu*> SchedulerPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Building plugin menus.";
    auto* menuScheduler = new QMenu(tr("&Scheduler"));

    auto* actJobDefs = menuScheduler->addAction(
        ico(Icon::TasksApp), tr("&Job Definitions"));
    connect(actJobDefs, &QAction::triggered, this, [this]() {
        if (jobDefinitionController_) jobDefinitionController_->showListWindow();
    });

    auto* actJobInstances = menuScheduler->addAction(
        ico(Icon::Clock), tr("&Job Instances"));
    connect(actJobInstances, &QAction::triggered, this, [this]() {
        if (jobInstanceController_) jobInstanceController_->showListWindow();
    });

    menuScheduler->addSeparator();

    auto* actMonitor = menuScheduler->addAction(
        ico(Icon::Clock), tr("&Monitor"));
    connect(actMonitor, &QAction::triggered, this, [this]() {
        if (schedulerMonitorController_) schedulerMonitorController_->showWindow();
    });

    BOOST_LOG_SEV(lg(), debug) << "Plugin menus ready.";
    return {menuScheduler};
}

void SchedulerPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    schedulerMonitorController_.reset();
    jobInstanceController_.reset();
    jobDefinitionController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

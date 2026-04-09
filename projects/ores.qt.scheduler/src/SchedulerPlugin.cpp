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
#include "ores.qt/JobDefinitionController.hpp"

namespace ores::qt {

namespace {
auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}
}

SchedulerPlugin::SchedulerPlugin(QObject* parent) : PluginBase(parent) {}

SchedulerPlugin::~SchedulerPlugin() = default;

void SchedulerPlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

    jobDefinitionController_ = std::make_unique<JobDefinitionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username,
        ctx_.change_reason_cache, this);
    connectControllerSignals(jobDefinitionController_.get());
}

QList<QMenu*> SchedulerPlugin::create_menus() {
    auto* menuScheduler = new QMenu(tr("&Scheduler"));

    auto* actJobDefs = menuScheduler->addAction(
        ico(Icon::TasksApp), tr("&Job Definitions"));
    connect(actJobDefs, &QAction::triggered, this, [this]() {
        if (jobDefinitionController_) jobDefinitionController_->showListWindow();
    });

    auto* actJobInstances = menuScheduler->addAction(tr("&Job Instances"));
    actJobInstances->setEnabled(false);
    actJobInstances->setToolTip(tr("Not yet implemented"));

    menuScheduler->addSeparator();

    auto* actMonitor = menuScheduler->addAction(tr("&Monitor"));
    actMonitor->setEnabled(false);
    actMonitor->setToolTip(tr("Not yet implemented"));

    return {menuScheduler};
}

void SchedulerPlugin::on_logout() {
    jobDefinitionController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

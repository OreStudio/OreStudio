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
#include "ores.qt/WorkflowPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WorkflowController.hpp"
#include <QAction>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.workflow_plugin");
    return instance;
}

auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}

}

WorkflowPlugin::WorkflowPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

WorkflowPlugin::~WorkflowPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void WorkflowPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    controller_ = std::make_unique<WorkflowController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, this);

    connect(controller_.get(),
            &WorkflowController::statusMessage,
            this,
            &WorkflowPlugin::statusMessage);
    connect(controller_.get(),
            &WorkflowController::detachableWindowCreated,
            this,
            &WorkflowPlugin::windowCreated);
    connect(controller_.get(),
            &WorkflowController::detachableWindowDestroyed,
            this,
            &WorkflowPlugin::windowDestroyed);
}

void WorkflowPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
                               << " workflows=" << (smc.workflows_menu ? "ok" : "null");
    if (!smc.workflows_menu)
        return;

    auto* workflows = smc.workflows_menu;

    auto* actExecutions = workflows->addAction(ico(Icon::TasksApp), tr("Workflow &Executions"));
    connect(actExecutions, &QAction::triggered, this, [this]() {
        if (controller_)
            controller_->showListWindow();
    });

    auto* actDefinitions =
        workflows->addAction(ico(Icon::DocumentTable), tr("Workflow &Definitions"));
    connect(actDefinitions, &QAction::triggered, this, [this]() {
        if (controller_)
            controller_->showDefinitionsWindow();
    });
}

QList<QMenu*> WorkflowPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "All items contributed via setup_menus — no standalone menus.";
    return {};
}

void WorkflowPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    if (controller_)
        controller_->closeAllWindows();
    controller_.reset();
    ctx_ = {};
}

} // namespace ores::qt

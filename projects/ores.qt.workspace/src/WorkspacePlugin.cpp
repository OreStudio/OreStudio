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
#include "ores.qt/WorkspacePlugin.hpp"

#include <QMenu>
#include <QAction>

#include "ores.qt/IconUtils.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/WorkspaceController.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.workspace_plugin");
    return instance;
}
}

WorkspacePlugin::WorkspacePlugin(QObject* parent) : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

WorkspacePlugin::~WorkspacePlugin() = default;

void WorkspacePlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    workspaceController_ = std::make_unique<WorkspaceController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);

    connect(workspaceController_.get(), &WorkspaceController::statusMessage,
            this, &WorkspacePlugin::statusMessage);
    connect(workspaceController_.get(), &WorkspaceController::errorMessage,
            this, &WorkspacePlugin::statusMessage);
}

QList<QMenu*> WorkspacePlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Building plugin menus.";
    auto* menu = new QMenu(tr("&Workspaces"));

    act_workspaces_ = menu->addAction(
        IconUtils::createRecoloredIcon(Icon::Database, IconUtils::DefaultIconColor),
        tr("&Manage Workspaces"));
    connect(act_workspaces_, &QAction::triggered, this, [this]() {
        if (workspaceController_)
            workspaceController_->showListWindow();
    });

    BOOST_LOG_SEV(lg(), debug) << "Plugin menus ready.";
    return {menu};
}

void WorkspacePlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    workspaceController_.reset();
    ctx_ = {};
}

}

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
#include "ores.logging/make_logger.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WorkspaceController.hpp"
#include <QAction>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.workspace_plugin");
    return instance;
}
}

WorkspacePlugin::WorkspacePlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

WorkspacePlugin::~WorkspacePlugin() = default;

void WorkspacePlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    workspaceController_ = std::make_unique<WorkspaceController>(ctx_.main_window,
                                                                 ctx_.mdi_area,
                                                                 ctx_.client_manager,
                                                                 ctx_.username,
                                                                 ctx_.badge_cache,
                                                                 this);

    connect(workspaceController_.get(),
            &WorkspaceController::statusMessage,
            this,
            &WorkspacePlugin::statusMessage);
    connect(workspaceController_.get(),
            &WorkspaceController::errorMessage,
            this,
            &WorkspacePlugin::statusMessage);
}

void WorkspacePlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
                               << " data_transfer=" << (smc.data_transfer_menu ? "ok" : "null");
    if (!smc.data_transfer_menu)
        return;

    smc.data_transfer_menu->addSeparator();

    act_workspaces_ = smc.data_transfer_menu->addAction(
        IconUtils::createRecoloredIcon(Icon::Database, IconUtils::DefaultIconColor),
        tr("&Manage Workspaces"));
    connect(act_workspaces_, &QAction::triggered, this, [this]() {
        if (workspaceController_)
            workspaceController_->showListWindow();
    });
}

QList<QMenu*> WorkspacePlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "All items contributed via setup_menus — no standalone menus.";
    return {};
}

void WorkspacePlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    workspaceController_.reset();
    ctx_ = {};
}

}

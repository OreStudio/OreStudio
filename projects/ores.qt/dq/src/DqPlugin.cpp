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
#include "ores.qt/DqPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/BadgeDefinitionController.hpp"
#include "ores.qt/BadgeSeverityController.hpp"
#include "ores.qt/CodeDomainController.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QAction>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.dq_plugin");
    return instance;
}

auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}

}

DqPlugin::DqPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

DqPlugin::~DqPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void DqPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    badgeDefinitionController_ = std::make_unique<BadgeDefinitionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(badgeDefinitionController_.get());

    badgeSeverityController_ = std::make_unique<BadgeSeverityController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(badgeSeverityController_.get());

    codeDomainController_ = std::make_unique<CodeDomainController>(ctx_.main_window,
                                                                   ctx_.mdi_area,
                                                                   ctx_.client_manager,
                                                                   ctx_.username,
                                                                   ctx_.badge_cache,
                                                                   this);
    connectControllerSignals(codeDomainController_.get());
}

void DqPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Capturing shared Data Quality menu handle."
                               << " data_quality=" << (smc.data_quality_menu ? "ok" : "null")
                               << " classifications=" << (smc.coding_schemes_menu ? "ok" : "null");
    data_quality_menu_ = smc.data_quality_menu;
    auto* dq = data_quality_menu_;
    if (!dq)
        return;

    // &Badges submenu: every badge_definition/badge_severity/badge_mapping
    // entry point lives here, not loose at the Data Quality top level.
    auto* badges = dq->addMenu(tr("&Badges"));

    auto* actBadgeDefs = badges->addAction(tr("Badge &Definitions"));
    connect(actBadgeDefs, &QAction::triggered, this, [this]() {
        if (badgeDefinitionController_)
            badgeDefinitionController_->showListWindow();
    });

    auto* actBadgeSevs = badges->addAction(tr("Badge &Severities"));
    connect(actBadgeSevs, &QAction::triggered, this, [this]() {
        if (badgeSeverityController_)
            badgeSeverityController_->showListWindow();
    });

    // Badge Mappings: browsable as a "Badge Mappings" tab on each Code
    // Domain's detail dialog (see BadgeMappingsTab), not a standalone
    // window here.

    // Code Domains lives in the shared Classifications submenu alongside
    // Coding Schemes and Coding Scheme Authority Types (contributed by
    // DataManagementPlugin) — all classification/coding lookups together.
    if (auto* classifications = smc.coding_schemes_menu) {
        auto* actCodeDomains = classifications->addAction(ico(Icon::Tag), tr("Code &Domains"));
        connect(actCodeDomains, &QAction::triggered, this, [this]() {
            if (codeDomainController_)
                codeDomainController_->showListWindow();
        });
    }
}

QList<QMenu*> DqPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Building plugin menus."
                               << " data_quality_menu=" << (data_quality_menu_ ? "ok" : "null");
    if (!data_quality_menu_) {
        BOOST_LOG_SEV(lg(), warn) << "Data Quality menu handle is missing — no menu will appear.";
        return {};
    }
    return {data_quality_menu_};
}

void DqPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    codeDomainController_.reset();
    badgeSeverityController_.reset();
    badgeDefinitionController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

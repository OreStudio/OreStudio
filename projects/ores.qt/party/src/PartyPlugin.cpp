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
#include "ores.qt/PartyPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/BusinessUnitController.hpp"
#include "ores.qt/BusinessUnitTypeController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PartyController.hpp"
#include "ores.qt/PartyIdSchemeController.hpp"
#include "ores.qt/PartyStatusController.hpp"
#include <QAction>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.party_plugin");
    return instance;
}
}

PartyPlugin::PartyPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

PartyPlugin::~PartyPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers
// ---------------------------------------------------------------------------
void PartyPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    partyStatusController_ = std::make_unique<PartyStatusController>(ctx_.main_window,
                                                                     ctx_.mdi_area,
                                                                     ctx_.client_manager,
                                                                     ctx_.change_reason_cache,
                                                                     ctx_.username,
                                                                     this);
    connectControllerSignals(partyStatusController_.get());

    partyIdSchemeController_ = std::make_unique<PartyIdSchemeController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         this);
    connectControllerSignals(partyIdSchemeController_.get());

    partyController_ = std::make_unique<PartyController>(ctx_.main_window,
                                                         ctx_.mdi_area,
                                                         ctx_.client_manager,
                                                         ctx_.image_cache,
                                                         ctx_.change_reason_cache,
                                                         ctx_.badge_cache,
                                                         ctx_.username,
                                                         this);
    connectControllerSignals(partyController_.get());

    businessUnitController_ = std::make_unique<BusinessUnitController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.image_cache,
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.badge_cache,
                                                                       ctx_.username,
                                                                       this);
    connectControllerSignals(businessUnitController_.get());

    businessUnitTypeController_ =
        std::make_unique<BusinessUnitTypeController>(ctx_.main_window,
                                                     ctx_.mdi_area,
                                                     ctx_.client_manager,
                                                     ctx_.change_reason_cache,
                                                     ctx_.username,
                                                     this);
    connectControllerSignals(businessUnitTypeController_.get());
}

// ---------------------------------------------------------------------------
// IPlugin::setup_menus — contribute Organisation section to Reference Data.
// ---------------------------------------------------------------------------
void PartyPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
                               << " reference_data=" << (smc.reference_data_menu ? "ok" : "null");
    auto* ref = smc.reference_data_menu;
    if (!ref)
        return;

    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    ref->addSeparator();

    act_parties_ = ref->addAction(ico(Icon::Organization), tr("&Parties"));
    connect(act_parties_, &QAction::triggered, this, [this]() {
        if (partyController_)
            partyController_->showListWindow();
    });
    ref->addSeparator();

    act_business_units_ = ref->addAction(ico(Icon::PeopleTeam), tr("Business &Units"));
    connect(act_business_units_, &QAction::triggered, this, [this]() {
        if (businessUnitController_)
            businessUnitController_->showListWindow();
    });

    ref->addSeparator();

    // Organisation Codes submenu is host-owned and shared with
    // RefdataPlugin (see shared_menus_context::organisation_codes_menu):
    // party_type migrated there already (see Commission: party_type
    // story); the entries below migrate as each is (re-)commissioned.
    // RefdataPlugin inserts the submenu into Reference Data; do not
    // insert it a second time here.
    auto* menuOrgCodes = smc.organisation_codes_menu;
    if (!menuOrgCodes)
        return;
    auto* actPartyStatuses = menuOrgCodes->addAction(ico(Icon::Flag), tr("Party &Statuses"));
    connect(actPartyStatuses, &QAction::triggered, this, [this]() {
        if (partyStatusController_)
            partyStatusController_->showListWindow();
    });
    auto* actPartyIdSchemes = menuOrgCodes->addAction(ico(Icon::Key), tr("Party &ID Schemes"));
    connect(actPartyIdSchemes, &QAction::triggered, this, [this]() {
        if (partyIdSchemeController_)
            partyIdSchemeController_->showListWindow();
    });
    auto* actBizUnitTypes =
        menuOrgCodes->addAction(ico(Icon::PeopleTeam), tr("Business Unit &Types"));
    connect(actBizUnitTypes, &QAction::triggered, this, [this]() {
        if (businessUnitTypeController_)
            businessUnitTypeController_->showListWindow();
    });
}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — no standalone menu; all items in Reference Data.
// ---------------------------------------------------------------------------
QList<QMenu*> PartyPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "All items contributed via setup_menus — no standalone menus.";
    return {};
}

QList<QAction*> PartyPlugin::toolbar_actions() {
    if (!act_parties_ || !act_business_units_)
        BOOST_LOG_SEV(lg(), warn) << "One or more toolbar actions are uninitialised.";
    return {act_parties_, act_business_units_};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — destroy all controllers in reverse dependency order
// ---------------------------------------------------------------------------
void PartyPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    businessUnitTypeController_.reset();
    businessUnitController_.reset();
    partyController_.reset();
    partyIdSchemeController_.reset();
    partyStatusController_.reset();

    ctx_ = {};
}

} // namespace ores::qt

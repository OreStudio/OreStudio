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

#include <QMenu>
#include <QAction>

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/PartyTypeController.hpp"
#include "ores.qt/PartyStatusController.hpp"
#include "ores.qt/PartyIdSchemeController.hpp"
#include "ores.qt/ContactTypeController.hpp"
#include "ores.qt/PartyController.hpp"
#include "ores.qt/CounterpartyController.hpp"
#include "ores.qt/BusinessCentreController.hpp"
#include "ores.qt/BusinessUnitController.hpp"
#include "ores.qt/BusinessUnitTypeController.hpp"

namespace ores::qt {

PartyPlugin::PartyPlugin(QObject* parent) : QObject(parent) {}

PartyPlugin::~PartyPlugin() = default;

// ---------------------------------------------------------------------------
// Helper: wire standard controller signals to PartyPlugin forwarding slots.
// ---------------------------------------------------------------------------
void PartyPlugin::connect_controller_signals(QObject* ctrl) {
    connect(ctrl, SIGNAL(statusMessage(const QString&)),
            this, SLOT(on_status_message(const QString&)));
    connect(ctrl, SIGNAL(errorMessage(const QString&)),
            this, SLOT(on_status_message(const QString&)));
    connect(ctrl, SIGNAL(detachableWindowCreated(DetachableMdiSubWindow*)),
            this, SLOT(on_window_created(DetachableMdiSubWindow*)));
    connect(ctrl, SIGNAL(detachableWindowDestroyed(DetachableMdiSubWindow*)),
            this, SLOT(on_window_destroyed(DetachableMdiSubWindow*)));
}

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers
// ---------------------------------------------------------------------------
void PartyPlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

    partyTypeController_ = std::make_unique<PartyTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(partyTypeController_.get());

    partyStatusController_ = std::make_unique<PartyStatusController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(partyStatusController_.get());

    partyIdSchemeController_ = std::make_unique<PartyIdSchemeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(partyIdSchemeController_.get());

    contactTypeController_ = std::make_unique<ContactTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(contactTypeController_.get());

    partyController_ = std::make_unique<PartyController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connect_controller_signals(partyController_.get());

    counterpartyController_ = std::make_unique<CounterpartyController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connect_controller_signals(counterpartyController_.get());

    businessCentreController_ = std::make_unique<BusinessCentreController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(businessCentreController_.get());

    businessUnitController_ = std::make_unique<BusinessUnitController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connect_controller_signals(businessUnitController_.get());

    businessUnitTypeController_ = std::make_unique<BusinessUnitTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(businessUnitTypeController_.get());
}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — return party/organisation domain menus.
// ---------------------------------------------------------------------------
QList<QMenu*> PartyPlugin::create_menus() {
    using IC = IconUtils;
    auto ico = [](Icon i) { return IC::createRecoloredIcon(i, IC::DefaultIconColor); };

    // ---- Organization -----------------------------------------------
    auto* menuOrg = new QMenu(tr("&Organization"));

    auto* actBusinessCentres = menuOrg->addAction(
        ico(Icon::BuildingBank), tr("&Business Centres"));
    connect(actBusinessCentres, &QAction::triggered, this, [this]() {
        if (businessCentreController_) businessCentreController_->showListWindow();
    });
    menuOrg->addSeparator();
    auto* actParties = menuOrg->addAction(ico(Icon::Organization), tr("&Parties"));
    connect(actParties, &QAction::triggered, this, [this]() {
        if (partyController_) partyController_->showListWindow();
    });
    auto* actCounterparties = menuOrg->addAction(ico(Icon::Handshake), tr("&Counterparties"));
    connect(actCounterparties, &QAction::triggered, this, [this]() {
        if (counterpartyController_) counterpartyController_->showListWindow();
    });
    menuOrg->addSeparator();
    auto* actBizUnits = menuOrg->addAction(ico(Icon::PeopleTeam), tr("Business &Units"));
    connect(actBizUnits, &QAction::triggered, this, [this]() {
        if (businessUnitController_) businessUnitController_->showListWindow();
    });
    auto* actBizUnitTypes = menuOrg->addAction(ico(Icon::PeopleTeam), tr("Business Unit &Types"));
    connect(actBizUnitTypes, &QAction::triggered, this, [this]() {
        if (businessUnitTypeController_) businessUnitTypeController_->showListWindow();
    });

    menuOrg->addSeparator();

    // Auxiliary types submenu
    auto* menuAux = menuOrg->addMenu(tr("A&uxiliary Types"));
    auto* actPartyTypes = menuAux->addAction(ico(Icon::Tag), tr("Party &Types"));
    connect(actPartyTypes, &QAction::triggered, this, [this]() {
        if (partyTypeController_) partyTypeController_->showListWindow();
    });
    auto* actPartyStatuses = menuAux->addAction(ico(Icon::Flag), tr("Party &Statuses"));
    connect(actPartyStatuses, &QAction::triggered, this, [this]() {
        if (partyStatusController_) partyStatusController_->showListWindow();
    });
    auto* actPartyIdSchemes = menuAux->addAction(ico(Icon::Key), tr("Party &ID Schemes"));
    connect(actPartyIdSchemes, &QAction::triggered, this, [this]() {
        if (partyIdSchemeController_) partyIdSchemeController_->showListWindow();
    });
    auto* actContactTypes = menuAux->addAction(ico(Icon::PersonAccounts), tr("&Contact Types"));
    connect(actContactTypes, &QAction::triggered, this, [this]() {
        if (contactTypeController_) contactTypeController_->showListWindow();
    });

    return {menuOrg};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — destroy all controllers in reverse dependency order
// ---------------------------------------------------------------------------
void PartyPlugin::on_logout() {
    businessUnitTypeController_.reset();
    businessUnitController_.reset();
    businessCentreController_.reset();
    counterpartyController_.reset();
    partyController_.reset();
    contactTypeController_.reset();
    partyIdSchemeController_.reset();
    partyStatusController_.reset();
    partyTypeController_.reset();

    ctx_ = {};
}

} // namespace ores::qt

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
#include "ores.qt/AdminPlugin.hpp"

#include <QMenu>
#include <QAction>
#include <QMdiArea>
#include <QMainWindow>
#include <QStatusBar>

#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/AccountController.hpp"
#include "ores.qt/RoleController.hpp"
#include "ores.qt/TenantController.hpp"
#include "ores.qt/TenantTypeController.hpp"
#include "ores.qt/TenantOnboardingWizard.hpp"
#include "ores.qt/SystemSettingController.hpp"
#include "ores.qt/BadgeDefinitionController.hpp"
#include "ores.qt/BadgeSeverityController.hpp"
#include "ores.qt/AppController.hpp"
#include "ores.qt/AppVersionController.hpp"

namespace ores::qt {

AdminPlugin::AdminPlugin(QObject* parent) : PluginBase(parent) {}

AdminPlugin::~AdminPlugin() = default;

void AdminPlugin::show_onboarding_wizard() {
    auto* wizard = new TenantOnboardingWizard(ctx_.client_manager, ctx_.main_window);
    wizard->setWindowModality(Qt::ApplicationModal);
    wizard->setAttribute(Qt::WA_DeleteOnClose);

    connect(wizard, &TenantOnboardingWizard::onboardingCompleted,
            this, [this](const QString& tenantName) {
        if (ctx_.status_bar)
            ctx_.status_bar->showMessage(
                tr("Tenant '%1' onboarded successfully.").arg(tenantName));
    });

    wizard->show();
}

void AdminPlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

    accountController_ = std::make_unique<AccountController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username,
        ctx_.change_reason_cache, ctx_.badge_cache, this);
    connectControllerSignals(accountController_.get());

    roleController_ = std::make_unique<RoleController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(roleController_.get());

    tenantController_ = std::make_unique<TenantController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(tenantController_.get());
    connect(tenantController_.get(), &TenantController::onboardRequested,
            this, &AdminPlugin::show_onboarding_wizard);

    tenantTypeController_ = std::make_unique<TenantTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(tenantTypeController_.get());

    systemSettingController_ = std::make_unique<SystemSettingController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(systemSettingController_.get());

    badgeDefinitionController_ = std::make_unique<BadgeDefinitionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(badgeDefinitionController_.get());

    badgeSeverityController_ = std::make_unique<BadgeSeverityController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(badgeSeverityController_.get());

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
}

void AdminPlugin::setup_menus(QMenu* system_menu, QMenu* /*reference_data_menu*/) {
    // ---- System > Identity -----------------------------------------------
    auto* identity = system_menu->addMenu(tr("&Identity"));

    act_accounts_ = identity->addAction(tr("&Accounts"));
    connect(act_accounts_, &QAction::triggered, this,
        [this]() { if (accountController_) accountController_->showListWindow(); });

    auto* actRoles = identity->addAction(tr("&Roles"));
    connect(actRoles, &QAction::triggered, this,
        [this]() { if (roleController_) roleController_->showListWindow(); });

    act_tenants_ = identity->addAction(tr("&Tenants"));
    connect(act_tenants_, &QAction::triggered, this,
        [this]() { if (tenantController_) tenantController_->showListWindow(); });

    auto* actTenantTypes = identity->addAction(tr("Tenant &Types"));
    connect(actTenantTypes, &QAction::triggered, this,
        [this]() { if (tenantTypeController_) tenantTypeController_->showListWindow(); });

    identity->addSeparator();

    auto* actOnboardTenant = identity->addAction(tr("&Onboard Tenant..."));
    connect(actOnboardTenant, &QAction::triggered, this, &AdminPlugin::show_onboarding_wizard);

    // ---- System > Configuration ------------------------------------------
    auto* config = system_menu->addMenu(tr("&Configuration"));

    act_system_settings_ = config->addAction(tr("&System Settings"));
    connect(act_system_settings_, &QAction::triggered, this,
        [this]() { if (systemSettingController_) systemSettingController_->showListWindow(); });

    config->addSeparator();

    auto* actBadgeDefs = config->addAction(tr("Badge &Definitions"));
    connect(actBadgeDefs, &QAction::triggered, this,
        [this]() { if (badgeDefinitionController_) badgeDefinitionController_->showListWindow(); });

    auto* actBadgeSevs = config->addAction(tr("Badge &Severities"));
    connect(actBadgeSevs, &QAction::triggered, this,
        [this]() { if (badgeSeverityController_) badgeSeverityController_->showListWindow(); });

    config->addSeparator();

    auto* actApps = config->addAction(tr("&Apps"));
    connect(actApps, &QAction::triggered, this,
        [this]() { if (appController_) appController_->showListWindow(); });

    auto* actAppVersions = config->addAction(tr("App &Versions"));
    connect(actAppVersions, &QAction::triggered, this,
        [this]() { if (appVersionController_) appVersionController_->showListWindow(); });
}

QList<QMenu*> AdminPlugin::create_menus() {
    return {};  // all items contributed via setup_menus()
}

QList<QAction*> AdminPlugin::toolbar_actions() {
    return {act_accounts_, act_tenants_, act_system_settings_};
}

void AdminPlugin::on_logout() {
    appVersionController_.reset();
    appController_.reset();
    badgeSeverityController_.reset();
    badgeDefinitionController_.reset();
    systemSettingController_.reset();
    tenantTypeController_.reset();
    tenantController_.reset();
    roleController_.reset();
    accountController_.reset();
    ctx_ = {};
}

} // namespace ores::qt

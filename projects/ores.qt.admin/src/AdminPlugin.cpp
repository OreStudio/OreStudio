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
#include <QMessageBox>
#include <QPointer>
#include <QInputDialog>
#include <QLineEdit>
#include <QtConcurrent>
#include <QFutureWatcher>

#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/AccountController.hpp"
#include "ores.qt/RoleController.hpp"
#include "ores.qt/TenantController.hpp"
#include "ores.qt/TenantTypeController.hpp"
#include "ores.qt/TenantOnboardingWizard.hpp"
#include "ores.qt/SystemSettingController.hpp"
#include "ores.qt/BadgeDefinitionController.hpp"
#include "ores.qt/BadgeSeverityController.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.iam.api/messaging/reset_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.admin_plugin");
    return instance;
}

auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}

}

AdminPlugin::AdminPlugin(QObject* parent) : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

AdminPlugin::~AdminPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

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
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
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
        ctx_.change_reason_cache, ctx_.username, ctx_.badge_cache, this);
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

}

void AdminPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
        << " identity=" << (smc.identity_menu ? "ok" : "null")
        << " system=" << (smc.system_menu ? "ok" : "null")
        << " telemetry=" << (smc.telemetry_menu ? "ok" : "null");
    // ---- Identity menu (top-level, pre-created by MainWindow) ---------------
    if (smc.identity_menu) {
        act_accounts_ = smc.identity_menu->addAction(
            ico(Icon::PersonAccounts), tr("&Accounts"));
        connect(act_accounts_, &QAction::triggered, this,
            [this]() { if (accountController_) accountController_->showListWindow(); });

        auto* actRoles = smc.identity_menu->addAction(tr("&Roles"));
        connect(actRoles, &QAction::triggered, this,
            [this]() { if (roleController_) roleController_->showListWindow(); });

        smc.identity_menu->addSeparator();

        act_tenants_ = smc.identity_menu->addAction(
            ico(Icon::BuildingSkyscraper), tr("&Tenants"));
        connect(act_tenants_, &QAction::triggered, this,
            [this]() { if (tenantController_) tenantController_->showListWindow(); });

        auto* actTenantTypes = smc.identity_menu->addAction(tr("Tenant &Types"));
        connect(actTenantTypes, &QAction::triggered, this,
            [this]() { if (tenantTypeController_) tenantTypeController_->showListWindow(); });

        smc.identity_menu->addSeparator();

        auto* actOnboardTenant = smc.identity_menu->addAction(tr("&Onboard Tenant..."));
        connect(actOnboardTenant, &QAction::triggered,
                this, &AdminPlugin::show_onboarding_wizard);
    }

    // ---- System > Configuration ------------------------------------------
    if (smc.system_menu && smc.telemetry_menu) {
        auto* telemetryAction = smc.telemetry_menu->menuAction();
        auto* config = new QMenu(tr("&Configuration"), smc.system_menu);
        smc.system_menu->insertMenu(telemetryAction, config);

        act_system_settings_ = config->addAction(ico(Icon::Flag), tr("&System Settings"));
        connect(act_system_settings_, &QAction::triggered, this,
            [this]() { if (systemSettingController_) systemSettingController_->showListWindow(); });

        config->addSeparator();

        auto* actBadgeDefs = config->addAction(tr("Badge &Definitions"));
        connect(actBadgeDefs, &QAction::triggered, this,
            [this]() { if (badgeDefinitionController_) badgeDefinitionController_->showListWindow(); });

        auto* actBadgeSevs = config->addAction(tr("Badge &Severities"));
        connect(actBadgeSevs, &QAction::triggered, this,
            [this]() { if (badgeSeverityController_) badgeSeverityController_->showListWindow(); });
        // Apps and App Versions live in the Compute menu (see ComputePlugin).

        smc.system_menu->addSeparator();

        act_reset_system_ = smc.system_menu->addAction(
            ico(Icon::Warning), tr("Reset &System..."));
        act_reset_system_->setToolTip(
            tr("Reset the entire system to pre-bootstrap state (SuperAdmin only)"));
        connect(act_reset_system_, &QAction::triggered,
                this, &AdminPlugin::on_reset_system);
    }
}

QList<QMenu*> AdminPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "No standalone menus — all entries contributed via shared menus.";
    return {};  // all items contributed via setup_menus()
}

QList<QAction*> AdminPlugin::toolbar_actions() {
    if (!act_accounts_ || !act_tenants_ || !act_system_settings_)
        BOOST_LOG_SEV(lg(), warn) << "One or more toolbar actions are uninitialised.";
    return {act_accounts_, act_tenants_, act_system_settings_};
}

void AdminPlugin::on_reset_system() {
    if (!ctx_.client_manager || !ctx_.client_manager->isConnected()) {
        MessageBoxHelper::warning(ctx_.main_window, "Disconnected",
            "Cannot reset system while disconnected.");
        return;
    }

    const QString confirmMessage =
        "Reset the entire system to pre-bootstrap state?\n\n"
        "This will:\n"
        "  • Hard-delete all non-system tenants and their data\n"
        "  • Soft-delete all system admin accounts and sessions\n"
        "  • Re-enable bootstrap mode so the system provisioner wizard\n"
        "    re-fires on next startup\n\n"
        "This is a destructive, irreversible operation.\n"
        "Type YES to confirm.";

    bool ok = false;
    const QString typed = QInputDialog::getText(
        ctx_.main_window, "Reset System", confirmMessage,
        QLineEdit::Normal, {}, &ok);
    if (!ok || typed.trimmed() != "YES") {
        BOOST_LOG_SEV(lg(), debug) << "System reset cancelled by user";
        return;
    }

    QPointer<AdminPlugin> self = this;
    using ResetResult = std::pair<bool, std::string>;

    auto task = [self]() -> ResetResult {
        if (!self || !self->ctx_.client_manager)
            return {false, "Plugin unloaded"};
        BOOST_LOG_SEV(lg(), info) << "Sending reset-system request";
        iam::messaging::reset_system_command request;
        auto result = self->ctx_.client_manager->process_authenticated_request(
            std::move(request));
        if (!result)
            return {false, "Failed to communicate with server"};
        return {result->success, result->message};
    };

    auto* watcher = new QFutureWatcher<ResetResult>(this);
    connect(watcher, &QFutureWatcher<ResetResult>::finished,
            this, [self, watcher]() {
        ResetResult res;
        try {
            res = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "system reset task threw: " << e.what();
            res = {false, e.what()};
        }
        auto [success, message] = res;
        watcher->deleteLater();
        if (!self) return;

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "System reset complete";
            if (self->ctx_.status_bar)
                self->ctx_.status_bar->showMessage(
                    tr("System has been reset to pre-bootstrap state."));
            MessageBoxHelper::information(self->ctx_.main_window,
                "System Reset", "System has been reset to pre-bootstrap state.\n"
                "Please restart the application.");
        } else {
            BOOST_LOG_SEV(lg(), error) << "System reset failed: " << message;
            MessageBoxHelper::critical(self->ctx_.main_window,
                "Reset Failed", QString::fromStdString(message));
        }
    });

    QFuture<ResetResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void AdminPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
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

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
#include "ores.iam.api/messaging/reset_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/AccountController.hpp"
#include "ores.qt/BadgeDefinitionController.hpp"
#include "ores.qt/BadgeSeverityController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/QaValidationRunnerWidget.hpp"
#include "ores.qt/RoleController.hpp"
#include "ores.qt/SettingGatedActionController.hpp"
#include "ores.qt/SystemSettingController.hpp"
#include "ores.qt/TenantController.hpp"
#include "ores.qt/TenantOnboardingWizard.hpp"
#include "ores.qt/TenantTypeController.hpp"
#include <QAction>
#include <QFutureWatcher>
#include <QInputDialog>
#include <QLineEdit>
#include <QMainWindow>
#include <QMdiArea>
#include <QMenu>
#include <QMessageBox>
#include <QPointer>
#include <QStatusBar>
#include <QtConcurrent>

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

AdminPlugin::AdminPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

AdminPlugin::~AdminPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void AdminPlugin::show_onboarding_wizard() {
    if (!ctx_.client_manager || !ctx_.client_manager->isConnected()) {
        MessageBoxHelper::warning(
            ctx_.main_window, "Disconnected", "Cannot onboard a tenant while disconnected.");
        return;
    }

    auto* wizard = new TenantOnboardingWizard(ctx_.client_manager, ctx_.main_window);
    wizard->setWindowModality(Qt::ApplicationModal);
    wizard->setAttribute(Qt::WA_DeleteOnClose);

    connect(wizard,
            &TenantOnboardingWizard::onboardingCompleted,
            this,
            [this](const QString& tenantName) {
                if (ctx_.status_bar)
                    ctx_.status_bar->showMessage(
                        tr("Tenant '%1' onboarded successfully.").arg(tenantName));
            });

    wizard->show();
}

void AdminPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    if (configMenu_)
        configMenu_->setEnabled(true);
    if (adminMenu_)
        adminMenu_->setEnabled(true);
    if (act_reset_system_)
        act_reset_system_->setEnabled(true);

    accountController_ = std::make_unique<AccountController>(ctx_.main_window,
                                                             ctx_.mdi_area,
                                                             ctx_.client_manager,
                                                             ctx_.username,
                                                             ctx_.change_reason_cache,
                                                             ctx_.badge_cache,
                                                             this);
    connectControllerSignals(accountController_.get());

    roleController_ = std::make_unique<RoleController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(roleController_.get());

    tenantController_ = std::make_unique<TenantController>(ctx_.main_window,
                                                           ctx_.mdi_area,
                                                           ctx_.client_manager,
                                                           ctx_.change_reason_cache,
                                                           ctx_.username,
                                                           ctx_.badge_cache,
                                                           this);
    connectControllerSignals(tenantController_.get());
    connect(tenantController_.get(),
            &TenantController::onboardRequested,
            this,
            &AdminPlugin::show_onboarding_wizard);

    tenantTypeController_ = std::make_unique<TenantTypeController>(ctx_.main_window,
                                                                   ctx_.mdi_area,
                                                                   ctx_.client_manager,
                                                                   ctx_.change_reason_cache,
                                                                   ctx_.username,
                                                                   this);
    connectControllerSignals(tenantTypeController_.get());

    systemSettingController_ = std::make_unique<SystemSettingController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         this);
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
                               << " account=" << (smc.account_menu ? "ok" : "null")
                               << " system=" << (smc.system_menu ? "ok" : "null")
                               << " telemetry=" << (smc.telemetry_menu ? "ok" : "null");

    if (!(smc.system_menu && smc.telemetry_menu))
        return;

    // ---- System > Configuration (inserted before Telemetry) --------------
    auto* telemetryAction = smc.telemetry_menu->menuAction();
    auto* config = new QMenu(tr("&Configuration"), smc.system_menu);
    smc.system_menu->insertMenu(telemetryAction, config);
    configMenu_ = config;
    configMenu_->setEnabled(false); // enabled on login, like everything below it

    act_system_settings_ = config->addAction(ico(Icon::Flag), tr("&System Settings"));
    connect(act_system_settings_, &QAction::triggered, this, [this]() {
        if (systemSettingController_)
            systemSettingController_->showListWindow();
    });

    config->addSeparator();

    auto* actBadgeDefs = config->addAction(tr("Badge &Definitions"));
    connect(actBadgeDefs, &QAction::triggered, this, [this]() {
        if (badgeDefinitionController_)
            badgeDefinitionController_->showListWindow();
    });

    auto* actBadgeSevs = config->addAction(tr("Badge &Severities"));
    connect(actBadgeSevs, &QAction::triggered, this, [this]() {
        if (badgeSeverityController_)
            badgeSeverityController_->showListWindow();
    });

    // ---- System > Administration (appended at end, admin-only) -----------
    smc.system_menu->addSeparator();

    auto* admin = smc.system_menu->addMenu(tr("&Administration"));
    adminMenu_ = admin;
    adminMenu_->setEnabled(false); // enabled on login

    act_accounts_ = admin->addAction(ico(Icon::PersonAccounts), tr("&Accounts"));
    connect(act_accounts_, &QAction::triggered, this, [this]() {
        if (accountController_)
            accountController_->showListWindow();
    });

    auto* actRoles = admin->addAction(tr("&Roles"));
    connect(actRoles, &QAction::triggered, this, [this]() {
        if (roleController_)
            roleController_->showListWindow();
    });

    admin->addSeparator();

    act_tenants_ = admin->addAction(ico(Icon::BuildingSkyscraper), tr("&Tenants"));
    connect(act_tenants_, &QAction::triggered, this, [this]() {
        if (tenantController_)
            tenantController_->showListWindow();
    });

    auto* actTenantTypes = admin->addAction(tr("Tenant &Types"));
    connect(actTenantTypes, &QAction::triggered, this, [this]() {
        if (tenantTypeController_)
            tenantTypeController_->showListWindow();
    });

    admin->addSeparator();

    auto* actOnboardTenant = admin->addAction(tr("&Onboard Tenant..."));
    connect(actOnboardTenant, &QAction::triggered, this, &AdminPlugin::show_onboarding_wizard);

    // ---- Reset System (appended after Administration) --------------------
    smc.system_menu->addSeparator();

    act_reset_system_ = smc.system_menu->addAction(ico(Icon::Warning), tr("Reset &System..."));
    act_reset_system_->setToolTip(
        tr("Reset the entire system to pre-bootstrap state (SuperAdmin only)"));
    act_reset_system_->setEnabled(false); // enabled on login
    connect(act_reset_system_, &QAction::triggered, this, &AdminPlugin::on_reset_system);

    // ---- System > Testing > QA Validation Runner --------------------------
    // A local, file-based testing tool (parses/rewrites test_scenario org
    // docs directly, no domain data) — unlike everything else this plugin
    // wires up, it needs neither a session nor NATS, so it's built here in
    // setup_menus() using main_window/mdi_area/client_manager rather than
    // waiting for on_login(), which would hide it before login. A regular
    // MDI subwindow (like every other window in the app), not a dock —
    // the tester needs to move/resize it freely while working through a
    // scenario alongside the dialog under test.
    if (smc.main_window && smc.mdi_area) {
        auto* testingMenu = smc.system_menu->addMenu(tr("&Testing"));
        auto* actQaRunner = testingMenu->addAction(tr("Scenario Runner"));
        connect(actQaRunner,
               &QAction::triggered,
               this,
               [this, main_window = smc.main_window, mdi_area = smc.mdi_area]() {
                   // Same lifecycle as every other list window in the app
                   // (see EntityController::bring_window_to_front): closing
                   // destroys it (WA_DeleteOnClose), so "open" always either
                   // reuses the still-live window or builds a fresh one —
                   // never re-shows a widget that was hidden-not-destroyed.
                   // (An earlier version kept this one alive across close
                   // for convenience, which hit what looks like a Qt/
                   // QMdiSubWindow repaint bug on reshow; this sidesteps it
                   // entirely rather than working around it.)
                   if (!qaValidationRunnerWindow_) {
                       qaValidationRunnerWidget_ = new QaValidationRunnerWidget(main_window);
                       qaValidationRunnerWidget_->setMdiArea(mdi_area);
                       qaValidationRunnerWindow_ = new DetachableMdiSubWindow();
                       qaValidationRunnerWindow_->setWidget(qaValidationRunnerWidget_);
                       qaValidationRunnerWindow_->setWindowTitle(tr("Scenario Runner"));
                       qaValidationRunnerWindow_->setWindowIcon(ico(Icon::TasksApp));
                       qaValidationRunnerWindow_->setAttribute(Qt::WA_DeleteOnClose);
                       mdi_area->addSubWindow(qaValidationRunnerWindow_);

                       connect(qaValidationRunnerWindow_,
                              &QObject::destroyed,
                              this,
                              [this]() {
                                  qaValidationRunnerWindow_ = nullptr;
                                  qaValidationRunnerWidget_ = nullptr;
                              });
                       connect(qaValidationRunnerWidget_,
                              &QaValidationRunnerWidget::statusMessage,
                              main_window,
                              [main_window](const QString& message) {
                                  if (auto* bar = main_window->statusBar())
                                      bar->showMessage(message, 5000);
                              });
                       connect(qaValidationRunnerWidget_,
                              &QaValidationRunnerWidget::errorOccurred,
                              main_window,
                              [main_window](const QString& message) {
                                  QMessageBox::warning(
                                      main_window, tr("Scenario Runner"), message);
                              });
                   }

                   qaValidationRunnerWindow_->setVisible(true);
                   mdi_area->setActiveSubWindow(qaValidationRunnerWindow_);
                   qaValidationRunnerWindow_->show();
                   qaValidationRunnerWindow_->raise();
                   qaValidationRunnerWindow_->activateWindow();
               });

        // Gated by a runtime system setting (not a compile-time macro): a
        // fleet-wide manual-testing tool, not something to strip from
        // release builds — same mechanism as the synthetic-data-generation
        // gating elsewhere. registerAction() defaults new actions to hidden
        // until it can confirm the setting via an authenticated request —
        // override that here so the action stays visible before login too;
        // refresh() (on login/reconnect) then corrects it if the setting is
        // explicitly false.
        qaValidationRunnerSettingGate_ =
            new SettingGatedActionController(smc.client_manager, this);
        qaValidationRunnerSettingGate_->registerAction(actQaRunner,
                                                       QStringLiteral("system.qa_validation_runner_enabled"),
                                                       /*guard=*/{},
                                                       /*default_when_missing=*/true);
        actQaRunner->setVisible(true);

        // --open-scenario: launch straight into a scenario, so the tester
        // doesn't have to click through System > Testing and Open every
        // time they restart the client to pick up a rebuild.
        if (!smc.open_scenario_path.isEmpty()) {
            qaValidationRunnerWidget_->loadScenario(smc.open_scenario_path);
            qaValidationRunnerWindow_->setVisible(true);
            smc.mdi_area->setActiveSubWindow(qaValidationRunnerWindow_);
            qaValidationRunnerWindow_->show();
            qaValidationRunnerWindow_->raise();
        }
    }
}

QList<QMenu*> AdminPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "No standalone menus — all entries contributed via shared menus.";
    return {}; // all items contributed via setup_menus()
}

QList<QAction*> AdminPlugin::toolbar_actions() {
    if (!act_accounts_ || !act_tenants_ || !act_system_settings_)
        BOOST_LOG_SEV(lg(), warn) << "One or more toolbar actions are uninitialised.";
    return {act_accounts_, act_tenants_, act_system_settings_};
}

void AdminPlugin::on_reset_system() {
    if (!ctx_.client_manager || !ctx_.client_manager->isConnected()) {
        MessageBoxHelper::warning(
            ctx_.main_window, "Disconnected", "Cannot reset system while disconnected.");
        return;
    }

    const QString confirmMessage = "Reset the entire system to pre-bootstrap state?\n\n"
                                   "This will:\n"
                                   "  • Hard-delete all non-system tenants and their data\n"
                                   "  • Soft-delete all system admin accounts and sessions\n"
                                   "  • Re-enable bootstrap mode so the system provisioner wizard\n"
                                   "    re-fires on next startup\n\n"
                                   "This is a destructive, irreversible operation.\n"
                                   "Type YES to confirm.";

    bool ok = false;
    const QString typed = QInputDialog::getText(
        ctx_.main_window, "Reset System", confirmMessage, QLineEdit::Normal, {}, &ok);
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
        auto result = self->ctx_.client_manager->process_authenticated_request(std::move(request));
        if (!result)
            return {false, "Failed to communicate with server"};
        return {result->success, result->message};
    };

    auto* watcher = new QFutureWatcher<ResetResult>(this);
    connect(watcher, &QFutureWatcher<ResetResult>::finished, this, [self, watcher]() {
        ResetResult res;
        try {
            res = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "system reset task threw: " << e.what();
            res = {false, e.what()};
        }
        auto [success, message] = res;
        watcher->deleteLater();
        if (!self)
            return;

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "System reset complete";
            if (self->ctx_.status_bar)
                self->ctx_.status_bar->showMessage(
                    tr("System has been reset to pre-bootstrap state."));
            MessageBoxHelper::information(self->ctx_.main_window,
                                          "System Reset",
                                          "System has been reset to pre-bootstrap state.\n"
                                          "Please restart the application.");
        } else {
            BOOST_LOG_SEV(lg(), error) << "System reset failed: " << message;
            MessageBoxHelper::critical(
                self->ctx_.main_window, "Reset Failed", QString::fromStdString(message));
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

    if (configMenu_)
        configMenu_->setEnabled(false);
    if (adminMenu_)
        adminMenu_->setEnabled(false);
    if (act_reset_system_)
        act_reset_system_->setEnabled(false);
}

} // namespace ores::qt

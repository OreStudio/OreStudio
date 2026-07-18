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
#ifndef ORES_QT_ADMIN_PLUGIN_HPP
#define ORES_QT_ADMIN_PLUGIN_HPP

#include "ores.qt/PluginBase.hpp"
#include <QList>
#include <memory>

class QAction;
class QMenu;

namespace ores::qt {

class AccountController;
class RoleController;
class TenantController;
class TenantTypeController;
class SystemSettingController;
class QaValidationRunnerWidget;
class SettingGatedActionController;
class DetachableMdiSubWindow;

/**
 * @brief Plugin owning all admin-domain entity controllers.
 *
 * Manages accounts, roles, tenants, system settings, and apps.
 * Loaded as a shared library by QPluginLoader at application startup.
 */
class AdminPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit AdminPlugin(QObject* parent = nullptr);
    ~AdminPlugin() override;

    QString name() const override {
        return QStringLiteral("ores.qt.admin");
    }
    int load_order() const override {
        return 50;
    } // setup_menus only; no standalone menus

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    QList<QAction*> toolbar_actions() override;
    void on_logout() override;

private:
    void show_onboarding_wizard();
    void on_reset_system();

    plugin_context ctx_;

    QAction* act_accounts_{nullptr};
    QAction* act_tenants_{nullptr};
    QAction* act_system_settings_{nullptr};
    QAction* act_reset_system_{nullptr};

    /**
     * @brief The User Accounts submenu, disabled until login — unlike
     * &Test Scenario Runner (which needs neither a session nor NATS and
     * stays reachable pre-login), everything under here needs a live
     * session. System Settings is gated individually via
     * act_system_settings_ instead, since it's a sibling of Test
     * Scenario Runner inside the always-enabled &System submenu.
     */
    QMenu* adminMenu_{nullptr};

    std::unique_ptr<AccountController> accountController_;
    std::unique_ptr<RoleController> roleController_;
    std::unique_ptr<TenantController> tenantController_;
    std::unique_ptr<TenantTypeController> tenantTypeController_;
    std::unique_ptr<SystemSettingController> systemSettingController_;

    /**
     * @brief The QA Validation Runner's dock, built in setup_menus()
     * (not on_login()) since it must stay usable before login — see
     * shared_menus_context::main_window/client_manager.
     */
    DetachableMdiSubWindow* qaValidationRunnerWindow_{nullptr};
    QaValidationRunnerWidget* qaValidationRunnerWidget_{nullptr};
    SettingGatedActionController* qaValidationRunnerSettingGate_{nullptr};
};

}

#endif

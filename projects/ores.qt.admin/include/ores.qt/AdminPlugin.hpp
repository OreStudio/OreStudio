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

#include <memory>
#include <QList>
#include "ores.qt/PluginBase.hpp"

namespace ores::qt {

class AccountController;
class RoleController;
class TenantController;
class TenantTypeController;
class SystemSettingController;
class BadgeDefinitionController;
class BadgeSeverityController;
class AppController;
class AppVersionController;

/**
 * @brief Plugin owning all admin-domain entity controllers.
 *
 * Manages accounts, roles, tenants, system settings, badges, and apps.
 * Loaded as a shared library by QPluginLoader at application startup.
 */
class AdminPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit AdminPlugin(QObject* parent = nullptr);
    ~AdminPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.admin"); }
    int load_order() const override { return 100; }

    void on_login(const plugin_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

private:
    void show_onboarding_wizard();

    plugin_context ctx_;

    std::unique_ptr<AccountController>          accountController_;
    std::unique_ptr<RoleController>             roleController_;
    std::unique_ptr<TenantController>           tenantController_;
    std::unique_ptr<TenantTypeController>       tenantTypeController_;
    std::unique_ptr<SystemSettingController>    systemSettingController_;
    std::unique_ptr<BadgeDefinitionController>  badgeDefinitionController_;
    std::unique_ptr<BadgeSeverityController>    badgeSeverityController_;
    std::unique_ptr<AppController>              appController_;
    std::unique_ptr<AppVersionController>       appVersionController_;
};

}

#endif

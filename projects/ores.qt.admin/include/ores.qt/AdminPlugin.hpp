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
#include <QObject>
#include <QList>
#include "ores.qt/IPlugin.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;
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
 * Extracted from LegacyPlugin in Step 3 of the Qt plugin refactor.
 * Manages accounts, roles, tenants, system settings, badges, and apps.
 */
class AdminPlugin : public QObject, public IPlugin {
    Q_OBJECT

public:
    explicit AdminPlugin(QObject* parent = nullptr);
    ~AdminPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.admin"); }
    int load_order() const override { return 100; }

    void on_login(const plugin_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

signals:
    void status_message(const QString& msg);
    void window_created(DetachableMdiSubWindow* window);
    void window_destroyed(DetachableMdiSubWindow* window);

private slots:
    void on_status_message(const QString& msg) { emit status_message(msg); }
    void on_window_created(DetachableMdiSubWindow* w) { emit window_created(w); }
    void on_window_destroyed(DetachableMdiSubWindow* w) { emit window_destroyed(w); }

private:
    void connect_controller_signals(QObject* ctrl);
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

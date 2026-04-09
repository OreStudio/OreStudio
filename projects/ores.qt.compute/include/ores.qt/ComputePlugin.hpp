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
#ifndef ORES_QT_COMPUTE_PLUGIN_HPP
#define ORES_QT_COMPUTE_PLUGIN_HPP

#include <memory>
#include <QList>
#include "ores.qt/PluginBase.hpp"

class QAction;

namespace ores::qt {

class AppController;
class AppVersionController;
class ComputeDashboardController;
class ComputeConsoleController;
class ServiceDashboardController;
class QueueMonitorController;
class ReportTypeController;
class ConcurrencyPolicyController;
class ReportDefinitionController;
class ReportInstanceController;

/**
 * @brief Plugin owning all compute, reporting and job controllers.
 *
 * Manages compute dashboard, console, service dashboard, job definitions,
 * queue monitor, report types/definitions/instances, concurrency policies,
 * and ORE import.
 * Loaded as a shared library by QPluginLoader at application startup.
 */
class ComputePlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit ComputePlugin(QObject* parent = nullptr);
    ~ComputePlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.compute"); }
    int load_order() const override { return 400; }

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    QList<QAction*> toolbar_actions() override;
    void on_logout() override;

private:

    plugin_context ctx_;

    QAction* act_report_definitions_{nullptr};
    QAction* act_report_instances_{nullptr};

    std::unique_ptr<AppController>               appController_;
    std::unique_ptr<AppVersionController>        appVersionController_;
    std::unique_ptr<ComputeDashboardController>  computeDashboardController_;
    std::unique_ptr<ComputeConsoleController>    computeConsoleController_;
    std::unique_ptr<ServiceDashboardController>  serviceDashboardController_;
    std::unique_ptr<QueueMonitorController>      queueMonitorController_;
    std::unique_ptr<ReportTypeController>        reportTypeController_;
    std::unique_ptr<ConcurrencyPolicyController> concurrencyPolicyController_;
    std::unique_ptr<ReportDefinitionController>  reportDefinitionController_;
    std::unique_ptr<ReportInstanceController>    reportInstanceController_;
};

}

#endif

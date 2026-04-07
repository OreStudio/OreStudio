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
#include <QObject>
#include <QList>
#include "ores.qt/IPlugin.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;
class ComputeDashboardController;
class ComputeConsoleController;
class ServiceDashboardController;
class JobDefinitionController;
class QueueMonitorController;
class ReportTypeController;
class ConcurrencyPolicyController;
class ReportDefinitionController;
class ReportInstanceController;
class OreImportController;

/**
 * @brief Plugin owning all compute, reporting and job controllers.
 *
 * Extracted from LegacyPlugin in Step 4 of the Qt plugin refactor.
 * Manages compute dashboard, console, service dashboard, job definitions,
 * queue monitor, report types/definitions/instances, concurrency policies,
 * and ORE import.
 */
class ComputePlugin : public QObject, public IPlugin {
    Q_OBJECT

public:
    explicit ComputePlugin(QObject* parent = nullptr);
    ~ComputePlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.compute"); }
    int load_order() const override { return 200; }

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

    plugin_context ctx_;

    std::unique_ptr<ComputeDashboardController>  computeDashboardController_;
    std::unique_ptr<ComputeConsoleController>    computeConsoleController_;
    std::unique_ptr<ServiceDashboardController>  serviceDashboardController_;
    std::unique_ptr<JobDefinitionController>     jobDefinitionController_;
    std::unique_ptr<QueueMonitorController>      queueMonitorController_;
    std::unique_ptr<ReportTypeController>        reportTypeController_;
    std::unique_ptr<ConcurrencyPolicyController> concurrencyPolicyController_;
    std::unique_ptr<ReportDefinitionController>  reportDefinitionController_;
    std::unique_ptr<ReportInstanceController>    reportInstanceController_;
    std::unique_ptr<OreImportController>         oreImportController_;
};

}

#endif

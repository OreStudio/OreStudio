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
#ifndef ORES_QT_WORKFLOW_MDI_WINDOW_HPP
#define ORES_QT_WORKFLOW_MDI_WINDOW_HPP

#include <vector>
#include <QTimer>
#include <QLabel>
#include <QAction>
#include <QComboBox>
#include <QDateTime>
#include <QGroupBox>
#include <QLineEdit>
#include <QSplitter>
#include <QTabWidget>
#include <QTableWidget>
#include <QToolBar>
#include <QStringList>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::qt {

/**
 * @brief MDI window for monitoring workflow instances.
 *
 * Provides two tabs:
 *  - Execution List: searchable, filterable table of all workflow instances
 *    with status badges. Selecting a row loads its steps in a detail panel
 *    below (service-dashboard style). Double-clicking a step with an error
 *    opens a read-only error detail dialog.
 *  - Dashboard: summary counts (active/failed) and recent failures table.
 *
 * Refreshes automatically on "ores.workflow.workflow_instance_changed" NATS
 * events (via the stale-indicator / markAsStale() mechanism) and supports an
 * optional auto-refresh timer.
 */
class WorkflowMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.workflow_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    struct FetchResult {
        bool success = false;
        QString error;
        std::vector<ores::workflow::messaging::workflow_instance_summary> instances;
    };

    struct StepsFetchResult {
        bool success = false;
        QString error;
        std::vector<ores::workflow::messaging::workflow_step_summary> steps;
    };


public:
    explicit WorkflowMdiWindow(ClientManager* clientManager,
                               QWidget* parent = nullptr);
    ~WorkflowMdiWindow() override = default;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

protected:
    void doReload() override;
    void closeEvent(QCloseEvent* event) override;
    QString normalRefreshTooltip() const override {
        return tr("Refresh workflow list");
    }

private slots:
    void onFetchFinished();
    void onRefreshToggled(bool checked);
    void onInstanceSelectionChanged();
    void onStepsFetchFinished();
    void onStepDoubleClicked(int row, int col);
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                const QStringList& entityIds, const QString& tenantId);
    void onFilterChanged();

private:
    void setupUi();
    void setupToolbar();
    void setupTabs();
    void setupDashboardTab(QWidget* tab);
    void setupExecutionListTab(QWidget* tab);
    void setupEventSubscriptions();

    void populateDashboard(
        const std::vector<ores::workflow::messaging::workflow_instance_summary>& instances);
    void populateExecutionList(
        const std::vector<ores::workflow::messaging::workflow_instance_summary>& instances);
    void loadStepsForInstance(const QString& instanceId);
    void populateSteps(
        const std::vector<ores::workflow::messaging::workflow_step_summary>& steps);

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* refreshAction_;
    QAction* autoRefreshAction_;
    QTimer* autoRefreshTimer_;

    QTabWidget* tabs_;

    // Dashboard tab widgets
    QLabel* activeCountLabel_;
    QLabel* failedCountLabel_;
    QTableWidget* failuresTable_;

    // Execution List tab widgets
    QLineEdit* searchEdit_;
    QComboBox* statusFilter_;
    QTableWidget* instanceTable_;
    QSplitter* splitter_;
    QGroupBox* stepsGroup_;
    QTableWidget* stepsTable_;

    // Async fetch — instances
    QFutureWatcher<FetchResult>* watcher_;
    std::vector<ores::workflow::messaging::workflow_instance_summary> currentInstances_;

    // Async fetch — steps
    QFutureWatcher<StepsFetchResult>* stepsWatcher_;
    QString selectedInstanceId_;
};

}

#endif

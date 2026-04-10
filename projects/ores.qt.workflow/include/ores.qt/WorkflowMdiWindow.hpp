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
#include <QLineEdit>
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
 *    with status badges. Double-click opens WorkflowInstanceDetailDialog.
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

public:
    explicit WorkflowMdiWindow(ClientManager* clientManager,
                               QWidget* parent = nullptr);
    ~WorkflowMdiWindow() override = default;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

protected:
    void doReload() override;
    QString normalRefreshTooltip() const override {
        return tr("Refresh workflow list");
    }

private slots:
    void onFetchFinished();
    void onRefreshToggled(bool checked);
    void onInstanceDoubleClicked(const QModelIndex& index);
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

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* refreshAction_;
    QAction* autoRefreshAction_;
    QTimer* autoRefreshTimer_;

    QTabWidget* tabs_;

    // Dashboard tab widgets
    QLabel* activeCountLabel_;
    QLabel* compensatingCountLabel_;
    QLabel* failedCountLabel_;
    QTableWidget* failuresTable_;

    // Execution List tab widgets
    QLineEdit* searchEdit_;
    QComboBox* statusFilter_;
    QTableWidget* instanceTable_;

    // Async fetch
    QFutureWatcher<FetchResult>* watcher_;
    std::vector<ores::workflow::messaging::workflow_instance_summary> currentInstances_;
};

}

#endif

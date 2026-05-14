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
#ifndef ORES_QT_WORKFLOW_STEPS_WIDGET_HPP
#define ORES_QT_WORKFLOW_STEPS_WIDGET_HPP

#include <vector>
#include <QLabel>
#include <QTimer>
#include <QUuid>
#include <QWidget>
#include <QTableWidget>
#include <QFutureWatcher>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::qt {

/**
 * @brief Embeddable widget showing the step progress of a workflow instance.
 *
 * Fetches step data from workflow.v1.instances.steps and auto-refreshes
 * every 3 seconds while the instance is running. The timer stops when a
 * terminal state is detected and instanceReachedTerminalState is emitted.
 *
 * Typical usage:
 * @code
 *   auto* w = new WorkflowStepsWidget(clientManager, this);
 *   w->setInstance(QUuid::fromString(workflowId));
 * @endcode
 */
class WorkflowStepsWidget final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.workflow_steps_widget";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    struct FetchResult {
        bool success = false;
        QString error;
        std::vector<ores::workflow::messaging::workflow_step_summary> steps;
    };

public:
    explicit WorkflowStepsWidget(ClientManager* clientManager,
        QWidget* parent = nullptr);

    /**
     * @brief Binds the widget to a workflow instance and starts auto-refresh.
     *
     * Pass a null QUuid to clear the binding and stop the timer.
     */
    void setInstance(const QUuid& instanceId);

    /**
     * @brief Triggers an immediate step fetch for the bound instance.
     */
    void refresh();

    /**
     * @brief Limits the number of steps shown (default: all steps).
     */
    void setMaxVisibleSteps(int n);

signals:
    /**
     * @brief Emitted once when the bound instance enters a terminal state.
     *
     * @p success is true if all steps completed successfully; false if any
     * step failed or the instance was compensated.
     */
    void instanceReachedTerminalState(bool success);

    /**
     * @brief Emitted when a step is found in the failed state.
     *
     * May be emitted multiple times if the same failure is observed across
     * refreshes — callers should de-duplicate by stepIndex if needed.
     */
    void stepFailed(int stepIndex, const QString& errorMessage);

private slots:
    void onFetchFinished();

private:
    void setupUi();
    void populateSteps(
        const std::vector<ores::workflow::messaging::workflow_step_summary>& steps);

    ClientManager* clientManager_;
    QUuid instanceId_;
    QLabel* headerLabel_;
    QTableWidget* stepsTable_;
    QTimer* refreshTimer_;
    QFutureWatcher<FetchResult>* watcher_;
    int maxVisibleSteps_ = -1;
    bool terminalReached_ = false;
};

}  // namespace ores::qt

#endif

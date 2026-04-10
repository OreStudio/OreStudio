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
#ifndef ORES_QT_WORKFLOW_INSTANCE_DETAIL_DIALOG_HPP
#define ORES_QT_WORKFLOW_INSTANCE_DETAIL_DIALOG_HPP

#include <vector>
#include <QDialog>
#include <QLabel>
#include <QTableWidget>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::qt {

/**
 * @brief Modal dialog showing steps for a specific workflow instance.
 *
 * Fetches the step list from the server when opened and displays each step
 * in a table with a status badge, timing information, and any error message.
 */
class WorkflowInstanceDetailDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.workflow_instance_detail_dialog";

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
    WorkflowInstanceDetailDialog(
        ClientManager* clientManager,
        const QString& workflowId,
        const QString& workflowType,
        const QString& workflowStatus,
        QWidget* parent = nullptr);

    void loadSteps();

private slots:
    void onFetchFinished();

private:
    void setupUi();
    void populateSteps(
        const std::vector<ores::workflow::messaging::workflow_step_summary>& steps);

    ClientManager* clientManager_;
    QString workflowId_;
    QString workflowType_;
    QString workflowStatus_;

    QLabel* statusLabel_;
    QTableWidget* stepsTable_;
    QFutureWatcher<FetchResult>* watcher_;
};

}

#endif

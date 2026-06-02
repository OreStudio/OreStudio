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

#include <QDialog>
#include <QLabel>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/WorkflowStepsWidget.hpp"
#include "ores.qt/WorkflowStepLogWidget.hpp"

namespace ores::qt {

/**
 * @brief Modal dialog showing steps for a specific workflow instance.
 *
 * Wraps WorkflowStepsWidget in a dialog with instance metadata in the header.
 * Steps auto-refresh every 3 seconds while the instance is running.
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

public:
    WorkflowInstanceDetailDialog(
        ClientManager* clientManager,
        const QString& workflowId,
        const QString& workflowType,
        const QString& workflowStatus,
        QWidget* parent = nullptr);

    /**
     * @brief Triggers an immediate step fetch (delegates to WorkflowStepsWidget).
     */
    void loadSteps();

private:
    void setupUi();

    ClientManager* clientManager_;
    QString workflowId_;
    QString workflowType_;
    QString workflowStatus_;

    QLabel* statusLabel_;
    WorkflowStepsWidget* stepsWidget_;
    WorkflowStepLogWidget* logWidget_;
};

}  // namespace ores::qt

#endif

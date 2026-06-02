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
#ifndef ORES_QT_WORKFLOW_STEP_LOG_WIDGET_HPP
#define ORES_QT_WORKFLOW_STEP_LOG_WIDGET_HPP

#include <vector>
#include <QLabel>
#include <QWidget>
#include <QTableWidget>
#include "ores.qt/WorkflowExport.hpp"
#include "ores.workflow.api/messaging/step_log_types.hpp"

namespace ores::qt {

/**
 * @brief Panel showing structured log entries for a single workflow step.
 *
 * Displays one row per step_log_entry with colour-coded level badge,
 * message text, and optional context field.  Populated via showLog()
 * whenever the user selects a step in WorkflowStepsWidget.
 */
class ORES_QT_WORKFLOW_EXPORT WorkflowStepLogWidget final : public QWidget {
    Q_OBJECT

public:
    explicit WorkflowStepLogWidget(QWidget* parent = nullptr);

    /**
     * @brief Populate the panel with log entries from the named step.
     *
     * Replaces any previously displayed entries.  If entries is empty a
     * placeholder row is shown.
     */
    void showLog(const QString& stepName,
        const std::vector<ores::workflow::messaging::step_log_entry>& entries);

    /**
     * @brief Reset to the "no step selected" placeholder state.
     */
    void clear();

private:
    void setupUi();

    QLabel* headerLabel_;
    QTableWidget* logTable_;
};

}  // namespace ores::qt

#endif

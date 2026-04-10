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
#ifndef ORES_QT_WORKFLOW_DEFINITION_MDI_WINDOW_HPP
#define ORES_QT_WORKFLOW_DEFINITION_MDI_WINDOW_HPP

#include <vector>
#include <QSplitter>
#include <QGroupBox>
#include <QTableWidget>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::qt {

/**
 * @brief MDI window for viewing registered workflow definitions.
 *
 * Shows all workflow types known to the engine with their descriptions
 * and step details. Selecting a definition shows its steps in a detail
 * panel below (master-detail layout).
 */
class WorkflowDefinitionMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.workflow_definition_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    struct FetchResult {
        bool success = false;
        QString error;
        std::vector<ores::workflow::messaging::workflow_definition_summary>
            definitions;
    };

public:
    explicit WorkflowDefinitionMdiWindow(ClientManager* clientManager,
                                         QWidget* parent = nullptr);
    ~WorkflowDefinitionMdiWindow() override = default;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

protected:
    void doReload() override;
    void closeEvent(QCloseEvent* event) override;
    QString normalRefreshTooltip() const override {
        return tr("Refresh workflow definitions");
    }

private slots:
    void onFetchFinished();
    void onDefinitionSelectionChanged();

private:
    void setupUi();
    void setupToolbar();
    void populateDefinitions(
        const std::vector<ores::workflow::messaging::workflow_definition_summary>&
            definitions);
    void populateSteps(
        const std::vector<ores::workflow::messaging::workflow_step_definition_summary>&
            steps);

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* refreshAction_;

    QTableWidget* definitionTable_;
    QSplitter* splitter_;
    QGroupBox* stepsGroup_;
    QTableWidget* stepsTable_;

    QFutureWatcher<FetchResult>* watcher_;
    std::vector<ores::workflow::messaging::workflow_definition_summary>
        currentDefinitions_;
};

}

#endif

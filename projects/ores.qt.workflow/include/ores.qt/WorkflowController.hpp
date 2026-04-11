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
#ifndef ORES_QT_WORKFLOW_CONTROLLER_HPP
#define ORES_QT_WORKFLOW_CONTROLLER_HPP

#include <QObject>
#include <QPointer>
#include <QMdiArea>
#include <QMainWindow>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

class WorkflowMdiWindow;
class WorkflowDefinitionMdiWindow;

/**
 * @brief Controller for the workflow monitor window.
 *
 * Creates and manages the lifetime of the WorkflowMdiWindow within the MDI
 * area. Follows the same pattern as ServiceDashboardController.
 */
class WorkflowController final : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.workflow_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    WorkflowController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        QObject* parent = nullptr);

    void showListWindow();
    void showDefinitionsWindow();
    void closeAllWindows();

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);
    void detachableWindowCreated(DetachableMdiSubWindow* window);
    void detachableWindowDestroyed(DetachableMdiSubWindow* window);

private:
    QMainWindow* mainWindow_;
    QMdiArea* mdiArea_;
    ClientManager* clientManager_;

    QPointer<WorkflowMdiWindow> listWindow_;
    QPointer<DetachableMdiSubWindow> listSubWindow_;

    QPointer<WorkflowDefinitionMdiWindow> defsWindow_;
    QPointer<DetachableMdiSubWindow> defsSubWindow_;
};

}

#endif

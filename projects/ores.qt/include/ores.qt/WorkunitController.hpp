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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_WORKUNIT_CONTROLLER_HPP
#define ORES_QT_WORKUNIT_CONTROLLER_HPP

#include <string>
#include <QMdiArea>
#include <QMainWindow>
#include "ores.qt/EntityController.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/workunit.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"

namespace ores::qt {

class WorkunitMdiWindow;
class DetachableMdiSubWindow;
class ChangeReasonCache;

/**
 * @brief Controller for managing workunit windows and operations.
 *
 * Manages the lifecycle of workunit list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class WorkunitController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.workunit_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    WorkunitController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        ChangeReasonCache* changeReasonCache,
        const QString& username,
        QObject* parent = nullptr);

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;
    void setHttpBaseUrl(const std::string& url);

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);

protected:
    EntityListMdiWindow* listWindow() const override;

private slots:
    void onShowDetails(const compute::domain::workunit& workunit);
    void onAddNewRequested();
    void onShowHistory(const compute::domain::workunit& workunit);
    void onRevertVersion(const compute::domain::workunit& workunit);
    void onOpenVersion(const compute::domain::workunit& workunit,
                       int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const compute::domain::workunit& workunit);
    void showHistoryWindow(const compute::domain::workunit& workunit);

    ChangeReasonCache* changeReasonCache_{nullptr};
    WorkunitMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
    std::string httpBaseUrl_;
};

}

#endif

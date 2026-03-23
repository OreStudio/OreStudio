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
#ifndef ORES_QT_REPORT_DEFINITION_CONTROLLER_HPP
#define ORES_QT_REPORT_DEFINITION_CONTROLLER_HPP

#include <vector>
#include <QMdiArea>
#include <QMainWindow>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/EntityController.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_definition.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"

namespace ores::qt {

class ReportDefinitionMdiWindow;
class DetachableMdiSubWindow;
class ChangeReasonCache;

/**
 * @brief Controller for managing report definition windows and operations.
 *
 * Manages the lifecycle of report definition list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class ReportDefinitionController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.report_definition_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    ReportDefinitionController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        ChangeReasonCache* changeReasonCache,
        const QString& username,
        QObject* parent = nullptr);

    ~ReportDefinitionController() override;

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);

protected:
    EntityListMdiWindow* listWindow() const override;

private slots:
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                const QStringList& entityIds, const QString& tenantId);
    void onShowDetails(const reporting::domain::report_definition& definition);
    void onAddNewRequested();
    void onShowHistory(const reporting::domain::report_definition& definition);
    void onRevertVersion(const reporting::domain::report_definition& definition);
    void onOpenVersion(const reporting::domain::report_definition& definition,
                       int versionNumber);
    void onScheduleRequested(const std::vector<boost::uuids::uuid>& ids);
    void onUnscheduleRequested(const std::vector<boost::uuids::uuid>& ids);

private:
    void showAddWindow();
    void showDetailWindow(const reporting::domain::report_definition& definition);
    void showHistoryWindow(const reporting::domain::report_definition& definition);

    ChangeReasonCache* changeReasonCache_;
    ReportDefinitionMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif

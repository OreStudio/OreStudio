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
#ifndef ORES_QT_REPORT_DEFINITION_MDI_WINDOW_HPP
#define ORES_QT_REPORT_DEFINITION_MDI_WINDOW_HPP

#include <vector>
#include <QToolBar>
#include <QTableView>
#include <QSortFilterProxyModel>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientReportDefinitionModel.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_definition.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing report definitions.
 *
 * Provides a table view of report definitions with toolbar actions
 * for reload, add, edit, delete, and viewing history.
 */
class ReportDefinitionMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.report_definition_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ReportDefinitionMdiWindow(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);
    ~ReportDefinitionMdiWindow() override = default;

public slots:
    void doReload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showDefinitionDetails(const reporting::domain::report_definition& definition);
    void addNewRequested();
    void definitionDeleted(const QString& code);
    void showDefinitionHistory(const reporting::domain::report_definition& definition);
    void scheduleRequested(const std::vector<boost::uuids::uuid>& ids);
    void unscheduleRequested(const std::vector<boost::uuids::uuid>& ids);

public slots:
    void addNew();
    void editSelected();
    void deleteSelected();
    void viewHistorySelected();
    void scheduleSelected();
    void unscheduleSelected();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh report definitions");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupConnections();
    void updateActionStates();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientReportDefinitionModel* model_;
    QSortFilterProxyModel* proxyModel_;
    PaginationWidget* paginationWidget_;

    // Toolbar actions
    QAction* reloadAction_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;
    QAction* scheduleAction_;
    QAction* unscheduleAction_;
};

}

#endif

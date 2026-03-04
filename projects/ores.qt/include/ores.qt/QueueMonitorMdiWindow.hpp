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
#ifndef ORES_QT_QUEUE_MONITOR_MDI_WINDOW_HPP
#define ORES_QT_QUEUE_MONITOR_MDI_WINDOW_HPP

#include <QToolBar>
#include <QTableView>
#include <QSortFilterProxyModel>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientQueueModel.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief MDI window for monitoring and managing message queues.
 *
 * Shows a merged view of queue definitions and statistics in a sortable table.
 * Supports create, delete, and purge queue operations, opening a detail dialog
 * for message publish/read, and a chart window for time-series metrics.
 */
class QueueMonitorMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.queue_monitor_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit QueueMonitorMdiWindow(ClientManager* clientManager,
                                   QWidget* parent = nullptr);
    ~QueueMonitorMdiWindow() override = default;

public slots:
    void reload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void viewChartRequested(const QString& queueId, const QString& queueName);
    void openDetailsRequested(const QString& queueName);
    void createQueueRequested();
    void deleteQueueRequested(const QString& queueName);
    void purgeQueueRequested(const QString& queueName);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh queue metrics");
    }

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onRowDoubleClicked(const QModelIndex& index);
    void onViewChart();
    void onCreateQueue();
    void onDeleteQueue();
    void onPurgeQueue();

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupConnections();
    void updateActionStates();

    ClientManager* clientManager_;
    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientQueueModel* model_;
    QSortFilterProxyModel* proxyModel_;
    QAction* reloadAction_;
    QAction* chartAction_;
    QAction* createAction_;
    QAction* deleteQueueAction_;
    QAction* purgeAction_;
};

}

#endif

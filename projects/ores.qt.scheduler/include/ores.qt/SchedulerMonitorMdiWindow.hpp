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
#ifndef ORES_QT_SCHEDULER_MONITOR_MDI_WINDOW_HPP
#define ORES_QT_SCHEDULER_MONITOR_MDI_WINDOW_HPP

#include <vector>
#include <QTimer>
#include <QWidget>
#include <QAction>
#include <QSpinBox>
#include <QToolBar>
#include <QTableWidget>
#include <QVBoxLayout>
#include <QCloseEvent>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.scheduler.api/messaging/scheduler_protocol.hpp"

namespace ores::qt {

/**
 * @brief Singleton MDI window showing a live per-job scheduler status.
 *
 * Columns: Job Name | Schedule | Active | Last Run | Last Status | Next Fire | Running
 * Auto-refreshes on a configurable interval (default: 30 seconds).
 */
class SchedulerMonitorMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.scheduler_monitor_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SchedulerMonitorMdiWindow(
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~SchedulerMonitorMdiWindow() override = default;

    QSize sizeHint() const override { return {860, 500}; }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error);

public slots:
    void refresh();

private slots:
    void onAutoRefreshToggled(bool enabled);
    void onAutoRefreshIntervalChanged(int seconds);

private:
    void setupUi();
    void applyStatus(const std::vector<scheduler::messaging::job_schedule_status>& jobs);

    enum Col {
        ColJobName   = 0,
        ColSchedule  = 1,
        ColActive    = 2,
        ColLastRun   = 3,
        ColLastStatus= 4,
        ColNextFire  = 5,
        ColRunning   = 6,
        ColCount     = 7
    };

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* refreshAction_;
    QAction* autoRefreshAction_;
    QSpinBox* intervalSpin_;
    QTableWidget* table_;
    QTimer* autoRefreshTimer_;
};

}

#endif

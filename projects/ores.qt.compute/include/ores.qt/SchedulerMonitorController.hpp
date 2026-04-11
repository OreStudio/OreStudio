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
#ifndef ORES_QT_SCHEDULER_MONITOR_CONTROLLER_HPP
#define ORES_QT_SCHEDULER_MONITOR_CONTROLLER_HPP

#include <QObject>
#include <QMdiArea>
#include <QMainWindow>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

class SchedulerMonitorMdiWindow;
class DetachableMdiSubWindow;

/**
 * @brief Controller for the Scheduler Monitor singleton window.
 */
class SchedulerMonitorController final : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.scheduler_monitor_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    static constexpr std::string_view event_subject =
        "scheduler.v1.job-instance-events";

public:
    SchedulerMonitorController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        QObject* parent = nullptr);
    ~SchedulerMonitorController() override;

    void showWindow();
    void closeWindow();

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);

private:
    QMainWindow* mainWindow_;
    QMdiArea* mdiArea_;
    ClientManager* clientManager_;
    SchedulerMonitorMdiWindow* window_{nullptr};
    DetachableMdiSubWindow* mdiSubWindow_{nullptr};
};

}

#endif

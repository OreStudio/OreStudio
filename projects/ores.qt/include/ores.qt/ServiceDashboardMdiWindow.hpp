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
#ifndef ORES_QT_SERVICE_DASHBOARD_MDI_WINDOW_HPP
#define ORES_QT_SERVICE_DASHBOARD_MDI_WINDOW_HPP

#include <QTimer>
#include <QLabel>
#include <QWidget>
#include <QToolBar>
#include <QTableWidget>
#include <QVBoxLayout>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief MDI window showing the live status of all running services.
 *
 * Queries telemetry.v1.services.list every 30 seconds and displays one row per
 * (service_name, instance_id) with a RAG indicator derived from sampled_at:
 *   Green  — last heartbeat < 30 s ago
 *   Amber  — 30–90 s ago
 *   Red    — > 90 s ago (service likely down)
 */
class ServiceDashboardMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.service_dashboard_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ServiceDashboardMdiWindow(
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~ServiceDashboardMdiWindow() override = default;

    QSize sizeHint() const override { return {700, 400}; }

public slots:
    void refresh();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onRefreshToggled(bool checked);

private:
    void setupUi();
    void setupToolbar();
    void loadSamples();

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* refreshAction_;
    QAction* autoRefreshAction_;

    QTableWidget* table_;

    // Auto-refresh timer (30 seconds)
    QTimer* autoRefreshTimer_;
};

}

#endif

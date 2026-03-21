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
#ifndef ORES_QT_COMPUTE_DASHBOARD_MDI_WINDOW_HPP
#define ORES_QT_COMPUTE_DASHBOARD_MDI_WINDOW_HPP

#include <QTimer>
#include <QLabel>
#include <QWidget>
#include <QToolBar>
#include <QGroupBox>
#include <QGridLayout>
#include <QVBoxLayout>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief MDI window showing a summary of the compute grid state.
 *
 * Displays aggregate counts: total/online hosts, pending/running/completed
 * workunits, and recent results. Auto-refreshes every 10 seconds.
 */
class ComputeDashboardMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.compute_dashboard_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ComputeDashboardMdiWindow(
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~ComputeDashboardMdiWindow() override = default;

    QSize sizeHint() const override { return {600, 400}; }

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
    void loadCounts();
    void updateCountLabel(QLabel* label, int value);

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* refreshAction_;
    QAction* autoRefreshAction_;

    // Host stats
    QLabel* totalHostsLabel_;
    QLabel* onlineHostsLabel_;

    // Workunit stats
    QLabel* pendingWorkunitLabel_;
    QLabel* activeWorkunitLabel_;
    QLabel* completedWorkunitLabel_;

    // Result stats
    QLabel* totalResultsLabel_;
    QLabel* recentResultsLabel_;

    // Auto-refresh timer (10 seconds)
    QTimer* autoRefreshTimer_;
};

}

#endif

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

#include <string>
#include <vector>
#include <QTimer>
#include <QLabel>
#include <QWidget>
#include <QAction>
#include <QSpinBox>
#include <QToolBar>
#include <QGroupBox>
#include <QSplitter>
#include <QPushButton>
#include <QTableWidget>
#include <QVBoxLayout>
#include <QCloseEvent>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.controller.api/domain/service_definition.hpp"

namespace ores::qt {

/**
 * @brief MDI window showing the live status of all running services.
 *
 * Overview panel: one row per service (aggregated across all replicas).
 * Status badge (Online/Degraded/Offline) derived from heartbeat age.
 * Replica count shows X running / Y desired.
 *
 * Detail panel: per-instance replica table with phase badges, plus
 * Stop All / Restart All buttons for the selected service.
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

    QSize sizeHint() const override { return {900, 600}; }

protected:
    void closeEvent(QCloseEvent* event) override;

public slots:
    void refresh();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onRefreshToggled(bool checked);
    void onRowSelected(int row);
    void onApplyReplicas();
    void onStopService();
    void onRestartService();

private:
    void setupUi();
    void setupToolbar();
    void loadSamples();
    void loadInstanceDetails(const QString& serviceName);

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* refreshAction_;
    QAction* autoRefreshAction_;

    // Overview table (one row per service)
    QTableWidget* table_;

    // Detail panel
    QGroupBox* detailGroup_;
    QLabel* detailServiceLabel_;
    QSpinBox* replicasSpinBox_;
    QPushButton* applyReplicasButton_;
    QPushButton* stopButton_;
    QPushButton* restartButton_;
    QTableWidget* detailTable_;

    QSplitter* splitter_;
    QTimer* autoRefreshTimer_;

    std::string selectedServiceName_;
    std::vector<controller::api::domain::service_definition> serviceDefinitions_;
};

}

#endif

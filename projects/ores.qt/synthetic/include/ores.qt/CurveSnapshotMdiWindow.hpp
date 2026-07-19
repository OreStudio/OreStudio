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
#ifndef ORES_QT_CURVE_SNAPSHOT_MDI_WINDOW_HPP
#define ORES_QT_CURVE_SNAPSHOT_MDI_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include <QWidget>
#include <string>

class QToolBar;
class QAction;
class QTabWidget;
class QTableWidget;
class QLabel;
class QChart;
class QChartView;
class QBarCategoryAxis;
class QValueAxis;
class QSpinBox;
class QComboBox;

namespace ores::qt {

class ClientManager;

/**
 * @brief Curve Snapshot viewer: an as-of snapshot of one IR curve's raw instrument grid (Grid
 * tab, table + current-shape chart) and its recent evolution (History tab, per-bucket bp-delta
 * table + overlaid fading chart) -- reads market_observation via the curve-snapshot NATS
 * queries, own MDI sub-window (not modal), reload-on-demand only. See task
 * curve-snapshot-builder-viewer for the signed-off requirements this implements.
 */
class CurveSnapshotMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic.curve_snapshot_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    CurveSnapshotMdiWindow(ClientManager* clientManager,
                           std::string currencyCode,
                           std::string indexName,
                           QWidget* parent = nullptr);
    ~CurveSnapshotMdiWindow() override = default;

    const std::string& currencyCode() const { return currencyCode_; }
    const std::string& indexName() const { return indexName_; }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& message);

private slots:
    void reload();

private:
    void setupUi();
    QWidget* buildGridTab();
    QWidget* buildHistoryTab();
    void loadGrid();
    void loadHistory();

    ClientManager* clientManager_;
    std::string currencyCode_;
    std::string indexName_;    // full code, e.g. "USD-SOFR"
    std::string qualifier_;    // series lookup qualifier, e.g. "USD/SOFR"

    QToolBar* toolbar_;
    QAction* reloadAction_;
    QTabWidget* tabs_;

    // Grid tab
    QLabel* gridHeaderLabel_;
    QTableWidget* gridTable_;
    QChart* gridChart_;
    QChartView* gridChartView_;
    QBarCategoryAxis* gridAxisX_;
    QValueAxis* gridAxisY_;
    QLabel* gridEmptyLabel_;

    // History tab
    QSpinBox* bucketSizeSpin_;
    QComboBox* bucketUnitCombo_;
    QSpinBox* bucketCountSpin_;
    QTableWidget* historyTable_;
    QChart* historyChart_;
    QChartView* historyChartView_;
    QBarCategoryAxis* historyAxisX_;
    QValueAxis* historyAxisY_;
    QLabel* historyEmptyLabel_;
};

}

#endif

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
#include "ores.marketdata.api/domain/market_observation.hpp"
#include <QWidget>
#include <string>
#include <vector>

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
class QTimer;

namespace ores::qt {

class ClientManager;
class ImageCache;

/**
 * @brief Curve Snapshot viewer: an as-of snapshot of one market-data curve's raw instrument
 * grid (Grid tab, table + current-shape chart) and its recent evolution (History tab,
 * per-bucket bp-delta table + overlaid fading chart).
 *
 * Reads market_observation via the curve-snapshot NATS queries, keyed only by the official
 * market_series identity (series_type/metric/qualifier) -- completely independent of how the
 * series was produced (=ores.synthetic= today, a real vendor feed like Bloomberg tomorrow).
 * Own MDI sub-window (not modal), reload-on-demand only. See task
 * curve-snapshot-builder-viewer for the signed-off requirements this implements.
 */
class CurveSnapshotMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.marketdata.curve_snapshot_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    CurveSnapshotMdiWindow(ClientManager* clientManager,
                           ImageCache* imageCache,
                           std::string seriesType,
                           std::string metric,
                           std::string qualifier,
                           QWidget* parent = nullptr);
    ~CurveSnapshotMdiWindow() override = default;

    const std::string& seriesType() const { return seriesType_; }
    const std::string& metric() const { return metric_; }
    const std::string& qualifier() const { return qualifier_; }

    QSize sizeHint() const override { return {1500, 750}; }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& message);

private slots:
    void reload();
    void onRefreshIntervalChanged();
    void exportGridToCsv();
    void exportGridToOre();

private:
    void setupUi();
    QWidget* buildGridTab();
    QWidget* buildHistoryTab();
    void loadGrid();
    void loadHistory();
    QString exportFileNameSlug() const;

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    std::string seriesType_;
    std::string metric_;
    std::string qualifier_;    // official market_series qualifier, e.g. "USD/SOFR"
    std::vector<ores::marketdata::domain::market_observation> lastGridObservations_;

    QToolBar* toolbar_;
    QAction* reloadAction_;
    QAction* exportCsvAction_;
    QAction* exportOreAction_;
    QTabWidget* tabs_;
    QComboBox* refreshIntervalCombo_;
    QTimer* autoRefreshTimer_;

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
    QWidget* historyLegend_;
};

}

#endif

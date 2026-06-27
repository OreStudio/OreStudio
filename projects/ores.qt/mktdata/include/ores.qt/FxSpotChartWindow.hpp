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
#ifndef ORES_QT_FX_SPOT_CHART_WINDOW_HPP
#define ORES_QT_FX_SPOT_CHART_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.client/fx_spot_subscription.hpp"
#include "ores.qt/ClientManager.hpp"
#include <QComboBox>
#include <QFutureWatcher>
#include <QPointF>
#include <QToolBar>
#include <QWidget>
#include <QtCharts/QChartView>
#include <QtCharts/QDateTimeAxis>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <memory>
#include <vector>

namespace ores::qt {

/**
 * @brief MDI window showing a live time-series chart of an FX spot mid price.
 *
 * The chart is deliberately agnostic of where its data comes from: on open it
 * backfills the most recent observations for the series from the marketdata
 * service, then appends live ticks delivered over NATS. The synthetic feed is
 * merely the current publisher; any source writing to the series' ORE key tick
 * subject and persisting observations will render identically.
 */
class FxSpotChartWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.fx_spot_chart_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    enum class TimeRange { Last5Min, LastHour, Last6Hours, Last24Hours, AllTime };

public:
    explicit FxSpotChartWindow(ClientManager* clientManager,
                               const marketdata::domain::market_series& series,
                               QWidget* parent = nullptr);
    ~FxSpotChartWindow() override = default;

    [[nodiscard]] const boost::uuids::uuid& seriesId() const {
        return series_.id;
    }

    QSize sizeHint() const override {
        return {900, 500};
    }

    /// Re-run the historical backfill (live subscription is left untouched).
    void reload();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onBackfillLoaded();
    void onTimeRangeChanged(int index);

private:
    /// Result of the asynchronous historical backfill fetch.
    struct BackfillResult {
        bool success = true;
        QString error_message;
        QString error_details;
        std::vector<QPointF> points; // x = ms since epoch, y = mid
    };

    void setupUi();
    void setupToolbar();
    void setupChart();
    void startBackfill();
    void startLiveSubscription();
    void appendPoint(qint64 ms, double mid);
    void rescaleAxes();

    marketdata::domain::market_series series_;
    QString oreKey_;
    ClientManager* clientManager_;
    TimeRange currentRange_{TimeRange::Last5Min};

    QToolBar* toolbar_;
    QAction* reloadAction_;
    QComboBox* rangeCombo_;
    QChartView* chartView_;
    QLineSeries* lineSeries_;
    QDateTimeAxis* axisX_;
    QValueAxis* axisY_;

    QFutureWatcher<BackfillResult>* backfillWatcher_;
    std::unique_ptr<marketdata::client::fx_spot_subscription> subscription_;
};

}

#endif

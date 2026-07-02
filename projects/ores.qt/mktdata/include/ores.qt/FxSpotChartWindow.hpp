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
#include <QLabel>
#include <QPointF>
#include <QTimer>
#include <QToolBar>
#include <QWidget>
#include <QtCharts/QBarCategoryAxis>
#include <QtCharts/QCandlestickSeries>
#include <QtCharts/QChartView>
#include <QtCharts/QDateTimeAxis>
#include <QtCharts/QLineSeries>
#include <QtCharts/QScatterSeries>
#include <QtCharts/QValueAxis>
#include <deque>
#include <map>
#include <memory>
#include <vector>

namespace ores::qt {

/**
 * @brief Live candlestick (OHLC) chart of an FX spot price.
 *
 * The chart is agnostic of where its data comes from: on open it backfills the
 * most recent observations for the series from the marketdata service, then
 * appends live ticks delivered over NATS. Both the backfill and the live stream
 * are aggregated into OHLC candles at a selectable interval; the rightmost
 * candle updates in place as ticks arrive. The synthetic feed is merely the
 * current publisher — any source writing to the series' tick subject and
 * persisting observations renders identically.
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
    void onIntervalChanged(int index);
    void onModeChanged();
    void onFlash(); // pulse the current-position marker

private:
    enum class Mode { Line, Candles };

    /// One OHLC bar.
    struct Candle {
        double open = 0.0;
        double high = 0.0;
        double low = 0.0;
        double close = 0.0;
    };

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

    void addSample(qint64 ms, double mid); // fold one tick into its candle
    void rebuildFromPoints();              // re-bucket all samples after interval change
    void refreshSeries();                  // rebuild whichever view is active
    void refreshCandles();
    void refreshLine();
    void applyYRange(double minV, double maxV);
    void applyMode(); // show/hide series + axes for the current mode

    marketdata::domain::market_series series_;
    QString oreKey_;
    ClientManager* clientManager_;
    Mode mode_{Mode::Candles};
    qint64 intervalMs_{5000}; // candle width; default 5s

    // Raw samples retained so we can re-bucket when the interval changes.
    // A deque gives O(1) pop-front when trimming to k_max_samples on each tick.
    std::deque<QPointF> samples_;
    // Aggregated candles keyed by bucket-start (ms since epoch), time-ordered.
    std::map<qint64, Candle> candles_;

    QToolBar* toolbar_;
    QAction* reloadAction_;
    QAction* lineAction_;
    QAction* candleAction_;
    QComboBox* intervalCombo_;
    QChartView* chartView_;
    QCandlestickSeries* candleSeries_;
    QLineSeries* lineSeries_;
    QLineSeries* trackerLine_;  // dashed horizontal at current price → Y-axis (line view)
    QScatterSeries* posMarker_; // pulsing marker at the latest point (line view)
    QBarCategoryAxis* axisX_;   // categorical, for candlesticks
    QDateTimeAxis* axisXTime_;  // time, for the line view
    QValueAxis* axisY_;         // shared price axis (right)

    QTimer* flashTimer_;
    bool flashBig_{false};

    QFutureWatcher<BackfillResult>* backfillWatcher_;
    std::unique_ptr<marketdata::client::fx_spot_subscription> subscription_;

    QLabel* statusOverlay_{nullptr}; // shown when chart has no data or an error
};

}

#endif

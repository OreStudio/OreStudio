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
#include "ores.qt/FxSpotChartWindow.hpp"
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WatermarkChartView.hpp"
#include <QActionGroup>
#include <QApplication>
#include <QBrush>
#include <QColor>
#include <QDateTime>
#include <QFont>
#include <QFontDatabase>
#include <QLabel>
#include <QMargins>
#include <QPaintEvent>
#include <QPainter>
#include <QPen>
#include <QPointer>
#include <QVBoxLayout>
#include <QtCharts/QCandlestickSet>
#include <QtCharts/QChart>
#include <QtConcurrent>
#include <algorithm>
#include <chrono>
#include <cmath>

namespace ores::qt {

using namespace ores::logging;

namespace {

/// Number of trailing observations to backfill on open.
constexpr int k_backfill_count = 100;

/// Maximum candles rendered (the visible window auto-scrolls to the latest).
constexpr int k_max_candles = 120;

/// Maximum points rendered in line mode.
constexpr int k_max_line_points = 2'000;

/// Cap on retained raw samples so a long-lived window does not grow unbounded.
constexpr int k_max_samples = 20'000;

QString ore_key_of(const marketdata::domain::market_series& s) {
    return QString::fromStdString(s.series_type + "/" + s.metric + "/" + s.qualifier);
}

qint64 to_ms(std::chrono::system_clock::time_point tp) {
    using namespace std::chrono;
    return duration_cast<milliseconds>(tp.time_since_epoch()).count();
}

/// A "nice" 1/2/5×10ⁿ step that divides @p range into roughly @p target ticks.
double nice_step(double range, int target) {
    if (range <= 0.0)
        return 0.0001;
    const double raw = range / std::max(1, target);
    const double mag = std::pow(10.0, std::floor(std::log10(raw)));
    const double norm = raw / mag;
    double step;
    if (norm < 1.5)
        step = 1.0;
    else if (norm < 3.0)
        step = 2.0;
    else if (norm < 7.0)
        step = 5.0;
    else
        step = 10.0;
    return step * mag;
}

/// Pick a clean time-tick step (ms) for ~6 gridlines on round boundaries.
qint64 nice_time_step_ms(qint64 span_ms) {
    static const qint64 candidates[] = {1'000,
                                        5'000,
                                        10'000,
                                        15'000,
                                        30'000,
                                        60'000,
                                        120'000,
                                        300'000,
                                        600'000,
                                        900'000,
                                        1'800'000,
                                        3'600'000,
                                        7'200'000,
                                        10'800'000,
                                        21'600'000,
                                        43'200'000,
                                        86'400'000};
    const qint64 target = std::max<qint64>(span_ms / 6, 1);
    for (qint64 c : candidates)
        if (c >= target)
            return c;
    return 86'400'000;
}

QString pretty_pair(const QString& oreKey) {
    const auto parts = oreKey.split('/');
    if (parts.size() >= 2)
        return parts.mid(parts.size() - 2).join('/');
    return oreKey;
}

/// The chart's preferred UI font (Inter) when available, else the application
/// default — so the chart renders correctly on hosts without Inter installed.
QFont ui_font(double pointSize, bool bold = false) {
    QFont f = QFontDatabase::hasFamily("Inter") ? QFont("Inter") : QApplication::font();
    f.setPointSizeF(pointSize);
    f.setBold(bold);
    return f;
}

} // namespace

using ores::qt::WatermarkChartView;

FxSpotChartWindow::FxSpotChartWindow(ClientManager* clientManager,
                                     const marketdata::domain::market_series& series,
                                     QWidget* parent)
    : QWidget(parent)
    , series_(series)
    , oreKey_(ore_key_of(series))
    , clientManager_(clientManager)
    , toolbar_(nullptr)
    , reloadAction_(nullptr)
    , lineAction_(nullptr)
    , candleAction_(nullptr)
    , intervalCombo_(nullptr)
    , chartView_(nullptr)
    , candleSeries_(nullptr)
    , lineSeries_(nullptr)
    , trackerLine_(nullptr)
    , posMarker_(nullptr)
    , axisX_(nullptr)
    , axisXTime_(nullptr)
    , axisY_(nullptr)
    , flashTimer_(new QTimer(this))
    , backfillWatcher_(new QFutureWatcher<BackfillResult>(this)) {

    BOOST_LOG_SEV(lg(), debug) << "Creating FX spot chart window for " << oreKey_.toStdString();
    setupUi();

    flashTimer_->setInterval(550);
    connect(flashTimer_, &QTimer::timeout, this, &FxSpotChartWindow::onFlash);

    connect(backfillWatcher_,
            &QFutureWatcher<BackfillResult>::finished,
            this,
            &FxSpotChartWindow::onBackfillLoaded);

    startBackfill();
}

void FxSpotChartWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupChart();
    layout->addWidget(chartView_);

    applyMode();
}

void FxSpotChartWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor), tr("Reload"));
    reloadAction_->setToolTip(tr("Re-fetch historical observations"));
    connect(reloadAction_, &QAction::triggered, this, &FxSpotChartWindow::reload);

    toolbar_->addSeparator();

    // View mode: line vs candlesticks (mutually exclusive).
    auto* modeGroup = new QActionGroup(this);
    modeGroup->setExclusive(true);

    candleAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Histogram, IconUtils::DefaultIconColor),
        tr("Candles"));
    candleAction_->setToolTip(tr("Candlestick (OHLC) view"));
    candleAction_->setCheckable(true);
    candleAction_->setChecked(true);
    modeGroup->addAction(candleAction_);
    connect(candleAction_, &QAction::triggered, this, &FxSpotChartWindow::onModeChanged);

    lineAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowTrending, IconUtils::DefaultIconColor),
        tr("Line"));
    lineAction_->setToolTip(tr("Line (mid price) view"));
    lineAction_->setCheckable(true);
    modeGroup->addAction(lineAction_);
    connect(lineAction_, &QAction::triggered, this, &FxSpotChartWindow::onModeChanged);

    toolbar_->addSeparator();

    auto* intervalLabel = new QLabel(tr("  Interval:  "), toolbar_);
    toolbar_->addWidget(intervalLabel);

    intervalCombo_ = new QComboBox(toolbar_);
    intervalCombo_->addItem(tr("1s"), QVariant::fromValue<qint64>(1'000));
    intervalCombo_->addItem(tr("5s"), QVariant::fromValue<qint64>(5'000));
    intervalCombo_->addItem(tr("15s"), QVariant::fromValue<qint64>(15'000));
    intervalCombo_->addItem(tr("1m"), QVariant::fromValue<qint64>(60'000));
    intervalCombo_->addItem(tr("5m"), QVariant::fromValue<qint64>(300'000));
    intervalCombo_->setCurrentIndex(1); // 5s
    toolbar_->addWidget(intervalCombo_);

    connect(intervalCombo_,
            QOverload<int>::of(&QComboBox::currentIndexChanged),
            this,
            &FxSpotChartWindow::onIntervalChanged);
}

void FxSpotChartWindow::setupChart() {
    const QColor up(0x22, 0xC5, 0x5E);
    const QColor down(0xEF, 0x44, 0x44);
    const QColor grid(255, 255, 255, 18);
    const QColor plotBg(0x12, 0x17, 0x1F);
    const QColor labelColor(0xCB, 0xD5, 0xE1);
    const QFont labelFont = ui_font(8.0);

    candleSeries_ = new QCandlestickSeries();
    candleSeries_->setIncreasingColor(up);
    candleSeries_->setDecreasingColor(down);
    candleSeries_->setBodyOutlineVisible(false);
    candleSeries_->setCapsVisible(false);

    lineSeries_ = new QLineSeries();
    lineSeries_->setPointsVisible(false); // no per-point circles — just the line
    {
        QPen pen(up);
        pen.setWidthF(2.0);
        pen.setCosmetic(true);
        lineSeries_->setPen(pen);
    }

    // Dashed horizontal price-tracker line: extends from left edge to terminal dot.
    trackerLine_ = new QLineSeries();
    {
        QPen pen(QColor(0x22, 0xC5, 0x5E, 100));
        pen.setWidthF(1.0);
        pen.setCosmetic(true);
        pen.setStyle(Qt::DashLine);
        trackerLine_->setPen(pen);
    }

    // Pulsing current-position marker for the line view.
    posMarker_ = new QScatterSeries();
    posMarker_->setMarkerShape(QScatterSeries::MarkerShapeCircle);
    posMarker_->setColor(up);
    posMarker_->setBorderColor(QColor(0xE2, 0xF5, 0xEA));
    posMarker_->setMarkerSize(9.0);

    auto* chart = new QChart();
    chart->setTheme(QChart::ChartThemeDark);
    chart->setBackgroundRoundness(0);
    chart->legend()->setVisible(false);
    chart->setMargins(QMargins(8, 8, 8, 8));
    chart->setTitleFont(ui_font(10.0, true));
    chart->setTitleBrush(QBrush(labelColor));
    chart->setTitle(tr("%1 (Mid)").arg(oreKey_));
    chart->setPlotAreaBackgroundBrush(plotBg);
    chart->setPlotAreaBackgroundVisible(true);
    chart->addSeries(candleSeries_);
    chart->addSeries(lineSeries_);
    chart->addSeries(trackerLine_);
    chart->addSeries(posMarker_);

    // Categorical X (candlesticks).
    axisX_ = new QBarCategoryAxis();
    axisX_->setGridLineColor(grid);
    axisX_->setLabelsColor(labelColor);
    axisX_->setLabelsFont(labelFont);
    axisX_->setLabelsAngle(-45);
    axisX_->setLinePenColor(grid);
    chart->addAxis(axisX_, Qt::AlignBottom);
    candleSeries_->attachAxis(axisX_);

    // Time X (line view).
    axisXTime_ = new QDateTimeAxis();
    axisXTime_->setFormat("HH:mm:ss");
    axisXTime_->setGridLineColor(grid);
    axisXTime_->setLabelsColor(labelColor);
    axisXTime_->setLabelsFont(labelFont);
    axisXTime_->setLinePenColor(grid);
    chart->addAxis(axisXTime_, Qt::AlignBottom);
    lineSeries_->attachAxis(axisXTime_);
    trackerLine_->attachAxis(axisXTime_);
    posMarker_->attachAxis(axisXTime_);

    // Shared price axis on the right — trading convention.
    axisY_ = new QValueAxis();
    axisY_->setLabelFormat("%.5f");
    axisY_->setGridLineColor(grid);
    axisY_->setMinorGridLineVisible(false);
    axisY_->setLabelsColor(labelColor);
    axisY_->setLabelsFont(labelFont);
    axisY_->setLinePenColor(grid);
    chart->addAxis(axisY_, Qt::AlignRight);
    candleSeries_->attachAxis(axisY_);
    lineSeries_->attachAxis(axisY_);
    trackerLine_->attachAxis(axisY_);
    posMarker_->attachAxis(axisY_);

    chartView_ = new WatermarkChartView(chart, this, pretty_pair(oreKey_));
    chartView_->setRenderHint(QPainter::Antialiasing);

    // Overlay shown while loading or when no data/error is available.
    statusOverlay_ = new QLabel(tr("Loading…"), chartView_);
    statusOverlay_->setAlignment(Qt::AlignCenter);
    statusOverlay_->setAttribute(Qt::WA_TransparentForMouseEvents);
    statusOverlay_->setStyleSheet(
        "background: transparent; color: rgba(203,213,225,180); font-size: 16px;");
    statusOverlay_->setGeometry(chartView_->rect());
    statusOverlay_->raise();
    statusOverlay_->show();
}

void FxSpotChartWindow::reload() {
    startBackfill();
}

void FxSpotChartWindow::startBackfill() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected; cannot load observations"));
        return;
    }
    if (backfillWatcher_->isRunning())
        return;

    const auto series_id = series_.id;
    QPointer<FxSpotChartWindow> self = this;
    // Guard the ClientManager (a QObject) as well as the window: the worker may
    // outlive either. clientManager_ is owned by the session and normally
    // outlives all child windows, but the QPointer makes the assumption explicit
    // and fails safe if that invariant is ever broken.
    QPointer<ClientManager> cm = clientManager_;
    QFuture<BackfillResult> future = QtConcurrent::run([self, cm, series_id]() -> BackfillResult {
        return exception_helper::wrap_async_fetch<BackfillResult>(
            [&]() -> BackfillResult {
                if (!self || !cm)
                    return {};
                marketdata::messaging::get_market_observations_request req;
                req.series_id = boost::uuids::to_string(series_id);
                req.limit = 500;
                auto result = cm->process_authenticated_request(std::move(req));
                BackfillResult r;
                if (!result) {
                    r.success = false;
                    r.error_message = QString::fromStdString(result.error());
                    return r;
                }
                const auto sid = boost::uuids::to_string(series_id);
                auto obs = std::move(result->market_observations);
                obs.erase(std::remove_if(obs.begin(), obs.end(),
                    [&](const auto& o) { return boost::uuids::to_string(o.series_id) != sid; }),
                    obs.end());
                r.points.reserve(obs.size());
                for (const auto& o : obs) {
                    try {
                        r.points.emplace_back(static_cast<double>(to_ms(o.observation_datetime)),
                                              std::stod(o.value));
                    } catch (const std::exception&) {
                        // Skip non-numeric values; they cannot be charted.
                    }
                }
                std::sort(r.points.begin(), r.points.end(), [](const QPointF& a, const QPointF& b) {
                    return a.x() < b.x();
                });
                if (static_cast<int>(r.points.size()) > k_backfill_count)
                    r.points.erase(r.points.begin(), r.points.end() - k_backfill_count);
                return r;
            },
            "FX spot observations");
    });
    backfillWatcher_->setFuture(future);
}

void FxSpotChartWindow::onBackfillLoaded() {
    const auto result = backfillWatcher_->result();
    if (!result.success) {
        if (statusOverlay_) {
            statusOverlay_->setText(tr("⚠  Could not load data\n%1").arg(result.error_message));
            statusOverlay_->show();
        }
        emit errorOccurred(result.error_message);
        return;
    }

    // Live ticks that arrived during the in-flight backfill are dropped here when
    // samples_ is replaced; the live subscription resumes appending from this
    // point. Acceptable for the PoC — see the feed-binding story for the fix.
    samples_.assign(result.points.begin(), result.points.end());
    while (static_cast<int>(samples_.size()) > k_max_samples)
        samples_.pop_front();
    rebuildFromPoints();

    if (statusOverlay_) {
        if (samples_.empty())
            statusOverlay_->setText(tr("No data available"));
        else
            statusOverlay_->hide();
    }
    emit statusChanged(tr("Backfilled %1 observation(s) for %2").arg(samples_.size()).arg(oreKey_));

    startLiveSubscription();
}

void FxSpotChartWindow::startLiveSubscription() {
    if (subscription_)
        return;
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    try {
        QPointer<FxSpotChartWindow> self = this;
        subscription_ = std::make_unique<marketdata::client::fx_spot_subscription>(
            clientManager_->nats_client(),
            oreKey_.toStdString(),
            clientManager_->currentTenantId(),
            [self](const marketdata::domain::fx_spot_tick& tick) {
                const qint64 ms = to_ms(tick.datetime);
                const double mid = tick.mid;
                QMetaObject::invokeMethod(
                    self,
                    [self, ms, mid]() {
                        if (self)
                            self->addSample(ms, mid);
                    },
                    Qt::QueuedConnection);
            });
        BOOST_LOG_SEV(lg(), debug) << "Live subscription started for " << oreKey_.toStdString();
    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to subscribe to live ticks: %1").arg(e.what()));
    }
}

void FxSpotChartWindow::addSample(qint64 ms, double mid) {
    samples_.emplace_back(static_cast<double>(ms), mid);
    if (static_cast<int>(samples_.size()) > k_max_samples)
        samples_.pop_front(); // O(1) on a deque

    const qint64 bucket = (ms / intervalMs_) * intervalMs_;
    auto it = candles_.find(bucket);
    if (it == candles_.end()) {
        candles_[bucket] = Candle{mid, mid, mid, mid};
        while (static_cast<int>(candles_.size()) > k_max_candles * 4)
            candles_.erase(candles_.begin());
    } else {
        Candle& c = it->second;
        c.high = std::max(c.high, mid);
        c.low = std::min(c.low, mid);
        c.close = mid;
    }
    refreshSeries();
}

void FxSpotChartWindow::rebuildFromPoints() {
    axisY_->setRange(0.0, 0.0); // reset so applyYRange sets unconditionally
    candles_.clear();
    for (const auto& p : samples_) {
        const qint64 ms = static_cast<qint64>(p.x());
        const double mid = p.y();
        const qint64 bucket = (ms / intervalMs_) * intervalMs_;
        auto it = candles_.find(bucket);
        if (it == candles_.end())
            candles_[bucket] = Candle{mid, mid, mid, mid};
        else {
            Candle& c = it->second;
            c.high = std::max(c.high, mid);
            c.low = std::min(c.low, mid);
            c.close = mid;
        }
    }
    refreshSeries();
}

void FxSpotChartWindow::applyYRange(double minV, double maxV) {
    const double rawRange = maxV - minV;
    const double pad = std::max({rawRange * 0.05, maxV * 0.002, 0.0001});
    const double pMin = minV - pad;
    const double pMax = maxV + pad;
    const double step = nice_step(pMax - pMin, 6);
    const double rMin = std::floor(pMin / step) * step;
    const double rMax = std::ceil(pMax / step) * step;

    // Only expand — never shrink. Axis contracts only on explicit reload so
    // the chart doesn't jump on every tick.
    const double curMin = axisY_->min();
    const double curMax = axisY_->max();
    if (curMin == 0.0 && curMax == 0.0) {
        // First call — set unconditionally.
        axisY_->setRange(rMin, rMax);
    } else {
        const double newMin = std::min(rMin, curMin);
        const double newMax = std::max(rMax, curMax);
        if (newMin != curMin || newMax != curMax)
            axisY_->setRange(newMin, newMax);
    }
    axisY_->setTickType(QValueAxis::TicksDynamic);
    axisY_->setTickAnchor(axisY_->min());
    axisY_->setTickInterval(step);
}

void FxSpotChartWindow::refreshSeries() {
    if (mode_ == Mode::Candles)
        refreshCandles();
    else
        refreshLine();
}

void FxSpotChartWindow::refreshCandles() {
    candleSeries_->clear();
    if (candles_.empty())
        return;

    std::vector<std::pair<qint64, Candle>> visible(candles_.begin(), candles_.end());
    if (static_cast<int>(visible.size()) > k_max_candles)
        visible.erase(visible.begin(), visible.end() - k_max_candles);

    // Compact label format; categorical axis slots are narrow.
    const QString fmt =
        intervalMs_ < 60'000 ? "HH:mm:ss" : (intervalMs_ < 86'400'000 ? "HH:mm" : "dd HH:mm");
    const int n = static_cast<int>(visible.size());
    const int labelEvery = std::max(1, n / 8);

    QStringList categories;
    categories.reserve(n);
    double minLow = visible.front().second.low;
    double maxHigh = visible.front().second.high;

    for (int i = 0; i < n; ++i) {
        const auto& [bucket, c] = visible[i];
        auto* set = new QCandlestickSet(
            c.open, c.high, c.low, c.close, static_cast<qreal>(bucket), candleSeries_);
        candleSeries_->append(set);

        if (i % labelEvery == 0)
            categories << QDateTime::fromMSecsSinceEpoch(bucket).toString(fmt);
        else
            // QBarCategoryAxis requires every category string to be unique, so a
            // blank label must still differ from its neighbours. We pad with a
            // per-index number of spaces (renders blank, stays unique). Only ~8
            // of the categories carry a real time label to avoid crowding.
            categories << QString(i + 1, ' ');

        minLow = std::min(minLow, c.low);
        maxHigh = std::max(maxHigh, c.high);
    }
    axisX_->setCategories(categories);
    applyYRange(minLow, maxHigh);

    const double lastClose = visible.back().second.close;
    if (auto* chart = chartView_ ? chartView_->chart() : nullptr)
        chart->setTitle(tr("%1 (Mid)   %2").arg(oreKey_).arg(lastClose, 0, 'f', 5));
}

void FxSpotChartWindow::refreshLine() {
    if (samples_.empty()) {
        lineSeries_->clear();
        return;
    }

    // Show the last k_max_line_points samples.
    int start = 0;
    if (static_cast<int>(samples_.size()) > k_max_line_points)
        start = static_cast<int>(samples_.size()) - k_max_line_points;

    QList<QPointF> pts;
    pts.reserve(static_cast<int>(samples_.size()) - start);
    double minY = samples_[start].y();
    double maxY = minY;
    for (int i = start; i < static_cast<int>(samples_.size()); ++i) {
        pts.append(samples_[i]);
        minY = std::min(minY, samples_[i].y());
        maxY = std::max(maxY, samples_[i].y());
    }
    lineSeries_->replace(pts);

    // Current-position marker at the latest point.
    posMarker_->clear();
    posMarker_->append(pts.back());

    // Clean, snapped time axis.
    qint64 lo = static_cast<qint64>(pts.front().x());
    qint64 hi = static_cast<qint64>(pts.back().x());
    if (hi <= lo)
        hi = lo + 1000;
    const qint64 tstep = nice_time_step_ms(hi - lo);
    lo = (lo / tstep) * tstep;
    hi = ((hi + tstep - 1) / tstep) * tstep;
    int xticks = std::clamp(static_cast<int>((hi - lo) / tstep) + 1, 2, 12);
    axisXTime_->setTickCount(xticks);
    axisXTime_->setFormat(tstep < 60'000 ? "HH:mm:ss" :
                                           (tstep < 86'400'000 ? "HH:mm" : "dd HH:mm"));
    axisXTime_->setRange(QDateTime::fromMSecsSinceEpoch(lo), QDateTime::fromMSecsSinceEpoch(hi));
    applyYRange(minY, maxY);

    // Price-tracker: dashed horizontal from snapped X-left-edge to terminal dot.
    const double currentPrice = pts.back().y();
    trackerLine_->replace({{static_cast<double>(lo), currentPrice},
                           {pts.back().x(),          currentPrice}});

    if (auto* chart = chartView_ ? chartView_->chart() : nullptr)
        chart->setTitle(tr("%1 (Mid)   %2").arg(oreKey_).arg(pts.back().y(), 0, 'f', 5));
}

void FxSpotChartWindow::applyMode() {
    const bool candles = (mode_ == Mode::Candles);

    candleSeries_->setVisible(candles);
    axisX_->setVisible(candles);

    lineSeries_->setVisible(!candles);
    axisXTime_->setVisible(!candles);
    trackerLine_->setVisible(!candles);
    posMarker_->setVisible(!candles); // current-position marker is line-view only

    // Clear the inactive series so no stale plot/axis lingers from the other
    // view; the active one is rebuilt from samples_/candles_ below.
    if (candles) {
        lineSeries_->clear();
        trackerLine_->clear();
        posMarker_->clear();
        flashTimer_->stop();
    } else {
        candleSeries_->clear();
        flashTimer_->start();
    }

    // The interval only governs candle aggregation.
    if (intervalCombo_)
        intervalCombo_->setEnabled(candles);

    refreshSeries();
}

void FxSpotChartWindow::onFlash() {
    if (mode_ != Mode::Line || !posMarker_ || posMarker_->count() == 0)
        return;
    flashBig_ = !flashBig_;
    posMarker_->setMarkerSize(flashBig_ ? 15.0 : 9.0);
}

void FxSpotChartWindow::onModeChanged() {
    mode_ = (lineAction_ && lineAction_->isChecked()) ? Mode::Line : Mode::Candles;
    applyMode();
}

void FxSpotChartWindow::onIntervalChanged(int index) {
    intervalMs_ = intervalCombo_->itemData(index).value<qint64>();
    rebuildFromPoints();
}

}

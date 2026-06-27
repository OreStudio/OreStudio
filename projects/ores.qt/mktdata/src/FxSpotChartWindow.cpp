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
#include <QDateTime>
#include <QLabel>
#include <QPainter>
#include <QPointer>
#include <QVBoxLayout>
#include <QtCharts/QChart>
#include <QtConcurrent>
#include <algorithm>
#include <chrono>

namespace ores::qt {

using namespace ores::logging;

namespace {

/// Cap on retained points so a long-lived window does not grow without bound.
constexpr int k_max_points = 5000;

/// Number of trailing observations to backfill on open.
constexpr int k_backfill_count = 100;

/// Reconstruct the ORE key from the series key components, e.g. "FX/RATE/EUR/USD".
QString ore_key_of(const marketdata::domain::market_series& s) {
    return QString::fromStdString(s.series_type + "/" + s.metric + "/" + s.qualifier);
}

qint64 to_ms(std::chrono::system_clock::time_point tp) {
    using namespace std::chrono;
    return duration_cast<milliseconds>(tp.time_since_epoch()).count();
}

/// Visible time span for each range, in milliseconds; 0 means "all time".
qint64 range_span_ms(int range) {
    using namespace std::chrono;
    switch (range) {
    case 0:
        return duration_cast<milliseconds>(minutes(5)).count();
    case 1:
        return duration_cast<milliseconds>(hours(1)).count();
    case 2:
        return duration_cast<milliseconds>(hours(6)).count();
    case 3:
        return duration_cast<milliseconds>(hours(24)).count();
    default:
        return 0;
    }
}

} // namespace

FxSpotChartWindow::FxSpotChartWindow(ClientManager* clientManager,
                                     const marketdata::domain::market_series& series,
                                     QWidget* parent)
    : QWidget(parent)
    , series_(series)
    , oreKey_(ore_key_of(series))
    , clientManager_(clientManager)
    , toolbar_(nullptr)
    , reloadAction_(nullptr)
    , rangeCombo_(nullptr)
    , chartView_(nullptr)
    , lineSeries_(nullptr)
    , axisX_(nullptr)
    , axisY_(nullptr)
    , backfillWatcher_(new QFutureWatcher<BackfillResult>(this)) {

    BOOST_LOG_SEV(lg(), debug) << "Creating FX spot chart window for " << oreKey_.toStdString();
    setupUi();

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

    auto* rangeLabel = new QLabel(tr("  Range:  "), toolbar_);
    toolbar_->addWidget(rangeLabel);

    rangeCombo_ = new QComboBox(toolbar_);
    rangeCombo_->addItem(tr("Last 5 min"), static_cast<int>(TimeRange::Last5Min));
    rangeCombo_->addItem(tr("Last 1 hour"), static_cast<int>(TimeRange::LastHour));
    rangeCombo_->addItem(tr("Last 6 hours"), static_cast<int>(TimeRange::Last6Hours));
    rangeCombo_->addItem(tr("Last 24 hours"), static_cast<int>(TimeRange::Last24Hours));
    rangeCombo_->addItem(tr("All time"), static_cast<int>(TimeRange::AllTime));
    rangeCombo_->setCurrentIndex(0);
    toolbar_->addWidget(rangeCombo_);

    connect(rangeCombo_,
            QOverload<int>::of(&QComboBox::currentIndexChanged),
            this,
            &FxSpotChartWindow::onTimeRangeChanged);
}

void FxSpotChartWindow::setupChart() {
    lineSeries_ = new QLineSeries();
    lineSeries_->setName(oreKey_);

    auto* chart = new QChart();
    chart->setTheme(QChart::ChartThemeDark);
    chart->setTitle(tr("FX Spot: %1").arg(oreKey_));
    chart->legend()->setVisible(false);
    chart->addSeries(lineSeries_);

    axisX_ = new QDateTimeAxis();
    axisX_->setFormat("hh:mm:ss");
    axisX_->setTitleText(tr("Time"));
    chart->addAxis(axisX_, Qt::AlignBottom);
    lineSeries_->attachAxis(axisX_);

    axisY_ = new QValueAxis();
    axisY_->setTitleText(tr("Mid"));
    axisY_->setLabelFormat("%.5f");
    chart->addAxis(axisY_, Qt::AlignLeft);
    lineSeries_->attachAxis(axisY_);

    chartView_ = new QChartView(chart, this);
    chartView_->setRenderHint(QPainter::Antialiasing);
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
    auto* cm = clientManager_;
    QFuture<BackfillResult> future = QtConcurrent::run([self, cm, series_id]() -> BackfillResult {
        return exception_helper::wrap_async_fetch<BackfillResult>(
            [&]() -> BackfillResult {
                if (!self)
                    return {};
                marketdata::messaging::get_market_observations_request req;
                req.series_id = boost::uuids::to_string(series_id);
                auto result = cm->process_authenticated_request(std::move(req));
                BackfillResult r;
                if (!result) {
                    r.success = false;
                    r.error_message = QString::fromStdString(result.error());
                    return r;
                }
                r.points.reserve(result->observations.size());
                for (const auto& o : result->observations) {
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
                    r.points.erase(r.points.begin(),
                                   r.points.end() - k_backfill_count);
                return r;
            },
            "FX spot observations");
    });
    backfillWatcher_->setFuture(future);
}

void FxSpotChartWindow::onBackfillLoaded() {
    const auto result = backfillWatcher_->result();
    if (!result.success) {
        emit errorOccurred(result.error_message);
        return;
    }

    QList<QPointF> points;
    points.reserve(static_cast<int>(result.points.size()));
    for (const auto& p : result.points)
        points.append(p);
    lineSeries_->replace(points);
    rescaleAxes();

    emit statusChanged(tr("Backfilled %1 observation(s) for %2")
                           .arg(points.size())
                           .arg(oreKey_));

    // Only start the live feed once the historical baseline is in place, so the
    // chart never shows live ticks ahead of its backfill.
    startLiveSubscription();
}

void FxSpotChartWindow::startLiveSubscription() {
    if (subscription_)
        return; // already live
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    try {
        QPointer<FxSpotChartWindow> self = this;
        subscription_ = std::make_unique<marketdata::client::fx_spot_subscription>(
            clientManager_->nats_client(),
            oreKey_.toStdString(),
            [self](const marketdata::domain::fx_spot_tick& tick) {
                // Delivered on a NATS thread; marshal onto the GUI thread before
                // touching any widget state.
                const qint64 ms = to_ms(tick.datetime);
                const double mid = tick.mid;
                QMetaObject::invokeMethod(
                    self,
                    [self, ms, mid]() {
                        if (self)
                            self->appendPoint(ms, mid);
                    },
                    Qt::QueuedConnection);
            });
        BOOST_LOG_SEV(lg(), debug) << "Live subscription started for " << oreKey_.toStdString();
    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to subscribe to live ticks: %1").arg(e.what()));
    }
}

void FxSpotChartWindow::appendPoint(qint64 ms, double mid) {
    lineSeries_->append(static_cast<double>(ms), mid);

    if (lineSeries_->count() > k_max_points)
        lineSeries_->removePoints(0, lineSeries_->count() - k_max_points);

    rescaleAxes();
}

void FxSpotChartWindow::rescaleAxes() {
    const auto points = lineSeries_->points();
    if (points.isEmpty())
        return;

    const qint64 latest = static_cast<qint64>(points.last().x());
    const qint64 earliest = static_cast<qint64>(points.first().x());
    const qint64 span = range_span_ms(rangeCombo_ ? rangeCombo_->currentIndex() : 0);

    const qint64 lo = (span == 0) ? earliest : std::max(earliest, latest - span);
    const qint64 hi = (latest > lo) ? latest : lo + 1000;

    axisX_->setRange(QDateTime::fromMSecsSinceEpoch(lo), QDateTime::fromMSecsSinceEpoch(hi));

    // Auto-fit Y to the points currently within the visible window.
    double minY = 0.0, maxY = 0.0;
    bool first = true;
    for (const auto& p : points) {
        if (p.x() < lo || p.x() > hi)
            continue;
        if (first) {
            minY = maxY = p.y();
            first = false;
        } else {
            minY = std::min(minY, p.y());
            maxY = std::max(maxY, p.y());
        }
    }
    if (first)
        return; // nothing visible

    const double pad = (maxY > minY) ? (maxY - minY) * 0.1 : std::max(maxY * 0.001, 1e-6);
    axisY_->setRange(minY - pad, maxY + pad);
}

void FxSpotChartWindow::onTimeRangeChanged(int index) {
    currentRange_ = static_cast<TimeRange>(rangeCombo_->itemData(index).toInt());
    rescaleAxes();
}

}

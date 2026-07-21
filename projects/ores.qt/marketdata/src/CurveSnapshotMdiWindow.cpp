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
#include "ores.qt/CurveSnapshotMdiWindow.hpp"
#include "ores.marketdata.api/messaging/curve_snapshot_protocol.hpp"
#include "ores.ore.core/market/market_data_serializer.hpp"
#include "ores.ore.core/market/market_datum.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include <QBrush>
#include <QComboBox>
#include <QDateTime>
#include <QDesktopServices>
#include <QFile>
#include <QFileDialog>
#include <QFont>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QMessageBox>
#include <QPainter>
#include <QPointer>
#include <QSpinBox>
#include <QTabWidget>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QTimeZone>
#include <QTimer>
#include <QToolBar>
#include <QUrl>
#include <QVBoxLayout>
#include <QtCharts/QBarCategoryAxis>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <QtConcurrent>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <limits>
#include <map>
#include <optional>
#include <sstream>

namespace ores::qt {

using namespace ores::logging;

namespace {

namespace m = ores::marketdata::messaging;
namespace md = ores::marketdata::domain;

// What the History delta grid's non-base columns show -- selected via historyDisplayModeCombo_.
enum class HistoryDisplayMode { Rate, DeltaRate, DeltaBp };

// Approximate days for tenor ordering (SPOT/1M/3M/2Y/10Y/...) -- point_id is the raw
// end_tenor_code (see ir_curve_template_resolver), not a sequence_index, so this is the only
// ordering signal available client-side without a second fetch of the template entries.
double approx_tenor_days(const std::string& tenor) {
    if (tenor.empty() || tenor == "SPOT" || tenor == "TODAY")
        return 0.0;
    char* end = nullptr;
    const double n = std::strtod(tenor.c_str(), &end);
    if (end == tenor.c_str())
        return std::numeric_limits<double>::max(); // unrecognised: sort last
    switch (*end) {
        case 'D':
            return n;
        case 'W':
            return n * 7.0;
        case 'M':
            return n * 30.0;
        case 'Y':
            return n * 365.0;
        default:
            return std::numeric_limits<double>::max();
    }
}

void sort_by_tenor(std::vector<md::market_observation>& obs) {
    std::sort(obs.begin(), obs.end(), [](const auto& a, const auto& b) {
        return approx_tenor_days(a.point_id) < approx_tenor_days(b.point_id);
    });
}

// For a RATES/YIELD series, qualifier is documented as "currency/index" -- the ORE-key segment
// structure, a producer contract (see ir_curve_tick::qualifier), not a guess.
std::string leading_currency_code(const std::string& qualifier) {
    const auto sep = qualifier.find('/');
    return sep == std::string::npos ? std::string{} : qualifier.substr(0, sep);
}

// Shades of blue, light (oldest) to dark/saturated (newest) -- same colour family throughout
// (not a rainbow), but each bucket still gets its own distinguishable shade rather than one
// hue faded by alpha alone. A default-constructed QPen is black, invisible on the dark chart
// theme, so this never falls back to that.
QColor history_series_color(int bucketIndex, int bucketCount) {
    static const std::array<QColor, 6> shades{
        QColor(0xbf, 0xdc, 0xfb), // pale blue
        QColor(0x93, 0xc5, 0xf8), //
        QColor(0x60, 0xa5, 0xf5), //
        QColor(0x3b, 0x82, 0xf6), //
        QColor(0x21, 0x63, 0xd9), //
        QColor(0x1e, 0x40, 0xaf), // deep navy
    };
    if (bucketCount <= 1)
        return shades.back();
    const auto idx = static_cast<std::size_t>(
        (static_cast<double>(bucketIndex) / (bucketCount - 1)) * (shades.size() - 1) + 0.5);
    return shades[std::min(idx, shades.size() - 1)];
}

// Same up/down arrow convention as the FX spot grid (FxSpotGridWindow::applyTick) -- the arrow
// itself carries the sign, so the magnitude is unsigned.
QString bp_arrow_text(double deltaBp) {
    return (deltaBp >= 0 ? QStringLiteral("↑ ") : QStringLiteral("↓ ")) +
           QString::number(std::abs(deltaBp), 'f', 1);
}

const QColor k_up_color{34, 197, 94};   // green-400, matching FxSpotGridWindow
const QColor k_down_color{239, 68, 68}; // red-400, matching FxSpotGridWindow

QString format_datetime(std::chrono::system_clock::time_point tp) {
    const auto t = std::chrono::system_clock::to_time_t(tp);
    QDateTime dt = QDateTime::fromSecsSinceEpoch(static_cast<qint64>(t), QTimeZone::UTC);
    return dt.toString("yyyy-MM-dd HH:mm:ss");
}

QChart* make_chart(const QString& title) {
    auto* chart = new QChart();
    chart->setTheme(QChart::ChartThemeDark);
    chart->setBackgroundBrush(Qt::NoBrush);
    chart->setPlotAreaBackgroundVisible(false);
    chart->setTitle(title);
    chart->setMargins(QMargins(4, 4, 4, 4));
    return chart;
}

void style_axes(QChart* chart, QBarCategoryAxis* axisX, QValueAxis* axisY) {
    const QColor textColor(0xCB, 0xD5, 0xE1);
    const QColor gridColor(255, 255, 255, 18);
    axisX->setTitleText(QObject::tr("Tenor"));
    axisY->setTitleText(QObject::tr("Rate"));
    axisY->setLabelFormat(QStringLiteral("%.4f"));
    QFont axisLabelFont;
    axisLabelFont.setPointSizeF(7.5);
    for (auto* axis : {static_cast<QAbstractAxis*>(axisX), static_cast<QAbstractAxis*>(axisY)}) {
        axis->setLabelsColor(textColor);
        axis->setLabelsFont(axisLabelFont);
        axis->setGridLineColor(gridColor);
        axis->setLinePenColor(gridColor);
    }
    axisX->setTitleBrush(textColor);
    axisY->setTitleBrush(textColor);
    chart->addAxis(axisX, Qt::AlignBottom);
    chart->addAxis(axisY, Qt::AlignLeft);
}

}

CurveSnapshotMdiWindow::CurveSnapshotMdiWindow(ClientManager* clientManager,
                                               ImageCache* imageCache,
                                               std::string seriesType,
                                               std::string metric,
                                               std::string qualifier,
                                               QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , imageCache_(imageCache)
    , seriesType_(std::move(seriesType))
    , metric_(std::move(metric))
    , qualifier_(std::move(qualifier))
    , toolbar_(nullptr)
    , reloadAction_(nullptr)
    , exportCsvAction_(nullptr)
    , exportOreAction_(nullptr)
    , tabs_(nullptr)
    , refreshIntervalCombo_(nullptr)
    , autoRefreshTimer_(nullptr)
    , gridHeaderLabel_(nullptr)
    , gridTable_(nullptr)
    , gridChart_(nullptr)
    , gridChartView_(nullptr)
    , gridAxisX_(nullptr)
    , gridAxisY_(nullptr)
    , gridEmptyLabel_(nullptr)
    , bucketSizeSpin_(nullptr)
    , bucketUnitCombo_(nullptr)
    , bucketCountSpin_(nullptr)
    , historyDisplayModeCombo_(nullptr)
    , historyTable_(nullptr)
    , historyChart_(nullptr)
    , historyChartView_(nullptr)
    , historyAxisX_(nullptr)
    , historyAxisY_(nullptr)
    , historyEmptyLabel_(nullptr)
    , historyLegend_(nullptr) {

    autoRefreshTimer_ = new QTimer(this);
    connect(autoRefreshTimer_, &QTimer::timeout, this, &CurveSnapshotMdiWindow::reload);

    setupUi();
    reload();
}

void CurveSnapshotMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));
    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    reloadAction_->setToolTip(tr("Reload the curve snapshot"));
    connect(reloadAction_, &QAction::triggered, this, &CurveSnapshotMdiWindow::reload);

    toolbar_->addSeparator();

    exportCsvAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ExportCsv, IconUtils::DefaultIconColor),
        tr("Export CSV"));
    exportCsvAction_->setToolTip(tr("Export the current grid snapshot to CSV"));
    connect(exportCsvAction_, &QAction::triggered, this, &CurveSnapshotMdiWindow::exportGridToCsv);

    exportOreAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ExportOre, IconUtils::DefaultIconColor),
        tr("Export ORE"));
    exportOreAction_->setToolTip(tr("Export the current grid snapshot as an ORE market data file"));
    connect(exportOreAction_, &QAction::triggered, this, &CurveSnapshotMdiWindow::exportGridToOre);

    layout->addWidget(toolbar_);

    tabs_ = new QTabWidget(this);
    tabs_->addTab(buildGridTab(), tr("Grid"));
    tabs_->addTab(buildHistoryTab(), tr("History"));
    layout->addWidget(tabs_);

    // Footer, same position/UX language as CrmCrossRatesMatrixMdiWindow's Update Interval.
    auto* footerLayout = new QHBoxLayout();
    footerLayout->addStretch(1);
    footerLayout->addWidget(new QLabel(tr("Update Interval:"), this));
    refreshIntervalCombo_ = new QComboBox(this);
    refreshIntervalCombo_->setMinimumWidth(120);
    refreshIntervalCombo_->addItem(tr("No auto-refresh"), 0);
    refreshIntervalCombo_->addItem(tr("5s"), 5);
    refreshIntervalCombo_->addItem(tr("10s"), 10);
    refreshIntervalCombo_->addItem(tr("30s"), 30);
    refreshIntervalCombo_->addItem(tr("60s"), 60);
    footerLayout->addWidget(refreshIntervalCombo_);
    connect(refreshIntervalCombo_,
            QOverload<int>::of(&QComboBox::currentIndexChanged),
            this,
            &CurveSnapshotMdiWindow::onRefreshIntervalChanged);
    refreshIntervalCombo_->setCurrentIndex(1); // 5s default, matching CrmCrossRatesMatrixMdiWindow
    onRefreshIntervalChanged(); // setCurrentIndex above may not fire if it's already index 1
    layout->addLayout(footerLayout);
}

void CurveSnapshotMdiWindow::onRefreshIntervalChanged() {
    const int secs = refreshIntervalCombo_->currentData().toInt();
    if (secs <= 0) {
        autoRefreshTimer_->stop();
        return;
    }
    autoRefreshTimer_->setInterval(secs * 1000);
    autoRefreshTimer_->start();
}

QWidget* CurveSnapshotMdiWindow::buildGridTab() {
    auto* page = new QWidget(this);
    auto* layout = new QVBoxLayout(page);

    auto* headerRow = new QHBoxLayout();
    if (imageCache_) {
        const auto ccy = leading_currency_code(qualifier_);
        if (!ccy.empty()) {
            auto* flagLabel = new QLabel(page);
            flagLabel->setPixmap(
                currency_flag_icon(*imageCache_, ccy).pixmap(single_flag_icon_size()));
            headerRow->addWidget(flagLabel);
        }
    }
    gridHeaderLabel_ = new QLabel(page);
    QFont headerFont = gridHeaderLabel_->font();
    headerFont.setBold(true);
    gridHeaderLabel_->setFont(headerFont);
    gridHeaderLabel_->setText(
        QString::fromStdString(seriesType_ + " / " + metric_ + " / " + qualifier_));
    headerRow->addWidget(gridHeaderLabel_);
    headerRow->addStretch(1);
    layout->addLayout(headerRow);

    auto* row = new QHBoxLayout();

    gridTable_ = new QTableWidget(0, 3, page);
    gridTable_->setHorizontalHeaderLabels({tr("Tenor"), tr("Rate"), tr("Observed")});
    gridTable_->horizontalHeader()->setStretchLastSection(false);
    gridTable_->verticalHeader()->setVisible(false);
    gridTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    gridTable_->setSelectionMode(QAbstractItemView::NoSelection);
    gridTable_->setAlternatingRowColors(true);
    row->addWidget(gridTable_, 1);

    gridChart_ = make_chart(tr("Curve Shape"));
    gridAxisX_ = new QBarCategoryAxis(this);
    gridAxisY_ = new QValueAxis(this);
    style_axes(gridChart_, gridAxisX_, gridAxisY_);
    gridChartView_ = new QChartView(gridChart_, page);
    gridChartView_->setRenderHint(QPainter::Antialiasing);
    gridChartView_->setMinimumHeight(220);
    gridChartView_->setMinimumWidth(280);
    row->addWidget(gridChartView_, 1);

    layout->addLayout(row);

    gridEmptyLabel_ = new QLabel(
        tr("No observations found yet. The feed may not have published, or the config may be "
           "disabled."),
        page);
    gridEmptyLabel_->setAlignment(Qt::AlignCenter);
    gridEmptyLabel_->setStyleSheet("color: gray; font-style: italic; padding: 24px;");
    gridEmptyLabel_->setVisible(false);
    layout->addWidget(gridEmptyLabel_);

    return page;
}

QWidget* CurveSnapshotMdiWindow::buildHistoryTab() {
    auto* page = new QWidget(this);
    auto* layout = new QVBoxLayout(page);

    auto* controls = new QHBoxLayout();
    controls->addWidget(new QLabel(tr("Bucket size:"), page));
    bucketSizeSpin_ = new QSpinBox(page);
    bucketSizeSpin_->setRange(1, 999);
    bucketSizeSpin_->setValue(10);
    controls->addWidget(bucketSizeSpin_);

    bucketUnitCombo_ = new QComboBox(page);
    bucketUnitCombo_->addItem(tr("minutes"), QVariant::fromValue<qlonglong>(60));
    bucketUnitCombo_->addItem(tr("hours"), QVariant::fromValue<qlonglong>(3600));
    bucketUnitCombo_->addItem(tr("days"), QVariant::fromValue<qlonglong>(86400));
    bucketUnitCombo_->setCurrentIndex(0); // minutes -- defaults must actually resolve to the
                                          // first item, not just rely on construction order
    controls->addWidget(bucketUnitCombo_);

    controls->addSpacing(16);
    controls->addWidget(new QLabel(tr("Show last:"), page));
    bucketCountSpin_ = new QSpinBox(page);
    bucketCountSpin_->setRange(2, 50);
    bucketCountSpin_->setValue(5);
    controls->addWidget(bucketCountSpin_);
    controls->addWidget(new QLabel(tr("buckets"), page));

    controls->addSpacing(16);
    controls->addWidget(new QLabel(tr("Show:"), page));
    historyDisplayModeCombo_ = new QComboBox(page);
    historyDisplayModeCombo_->addItem(tr("Rate"), static_cast<int>(HistoryDisplayMode::Rate));
    historyDisplayModeCombo_->addItem(tr("Δ Rate"),
                                      static_cast<int>(HistoryDisplayMode::DeltaRate));
    historyDisplayModeCombo_->addItem(tr("Δ (bp)"), static_cast<int>(HistoryDisplayMode::DeltaBp));
    historyDisplayModeCombo_->setCurrentIndex(2); // Δ (bp), matches the existing default view
    controls->addWidget(historyDisplayModeCombo_);
    controls->addStretch(1);

    connect(bucketSizeSpin_,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this,
            &CurveSnapshotMdiWindow::loadHistory);
    connect(bucketUnitCombo_,
            QOverload<int>::of(&QComboBox::currentIndexChanged),
            this,
            &CurveSnapshotMdiWindow::loadHistory);
    connect(bucketCountSpin_,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this,
            &CurveSnapshotMdiWindow::loadHistory);
    connect(historyDisplayModeCombo_,
            QOverload<int>::of(&QComboBox::currentIndexChanged),
            this,
            &CurveSnapshotMdiWindow::loadHistory);

    layout->addLayout(controls);

    auto* row = new QHBoxLayout();

    historyTable_ = new QTableWidget(0, 1, page);
    historyTable_->setHorizontalHeaderLabels({tr("Tenor")});
    historyTable_->horizontalHeader()->setStretchLastSection(false);
    historyTable_->verticalHeader()->setVisible(false);
    historyTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    historyTable_->setSelectionMode(QAbstractItemView::NoSelection);
    historyTable_->setAlternatingRowColors(true);
    row->addWidget(historyTable_, 1);

    historyChart_ = make_chart(tr("Curve Evolution"));
    historyAxisX_ = new QBarCategoryAxis(this);
    historyAxisY_ = new QValueAxis(this);
    style_axes(historyChart_, historyAxisX_, historyAxisY_);
    historyChartView_ = new QChartView(historyChart_, page);
    historyChartView_->setRenderHint(QPainter::Antialiasing);
    historyChartView_->setMinimumHeight(220);
    historyChartView_->setMinimumWidth(280);

    auto* chartCol = new QVBoxLayout();
    chartCol->addWidget(historyChartView_, 1);
    historyLegend_ = new QWidget(page);
    auto* legendLayout = new QHBoxLayout(historyLegend_);
    legendLayout->setContentsMargins(0, 0, 0, 0);
    chartCol->addWidget(historyLegend_);
    row->addLayout(chartCol, 1);

    layout->addLayout(row);

    historyEmptyLabel_ = new QLabel(
        tr("No observations found yet. The feed may not have published, or the config may be "
           "disabled."),
        page);
    historyEmptyLabel_->setAlignment(Qt::AlignCenter);
    historyEmptyLabel_->setStyleSheet("color: gray; font-style: italic; padding: 24px;");
    historyEmptyLabel_->setVisible(false);
    layout->addWidget(historyEmptyLabel_);

    return page;
}

void CurveSnapshotMdiWindow::reload() {
    loadGrid();
    loadHistory();
}

void CurveSnapshotMdiWindow::loadGrid() {
    emit statusChanged(tr("Loading curve snapshot..."));

    m::get_curve_snapshot_request req;
    req.series_type = seriesType_;
    req.metric = metric_;
    req.qualifier = qualifier_;

    QPointer<CurveSnapshotMdiWindow> self = this;
    auto* cm = clientManager_;

    struct Result {
        bool success = false;
        QString message;
        std::vector<md::market_observation> observations;
    };

    auto task = [cm, req]() -> Result {
        auto resp = cm->process_authenticated_request(req);
        if (!resp)
            return {false, QString::fromStdString(resp.error()), {}};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message), {}};
        return {true, {}, std::move(resp->observations)};
    };

    auto* watcher = new QFutureWatcher<Result>(self);
    connect(watcher, &QFutureWatcher<Result>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        if (!result.success) {
            emit self->errorOccurred(tr("Failed to load curve snapshot: %1").arg(result.message));
            return;
        }

        sort_by_tenor(result.observations);
        self->lastGridObservations_ = result.observations;

        const bool empty = result.observations.empty();
        self->gridTable_->setVisible(!empty);
        self->gridChartView_->setVisible(!empty);
        self->gridEmptyLabel_->setVisible(empty);

        self->gridTable_->setRowCount(static_cast<int>(result.observations.size()));
        QStringList categories;
        auto* series = new QLineSeries();
        double yMin = std::numeric_limits<double>::max();
        double yMax = std::numeric_limits<double>::lowest();

        for (std::size_t i = 0; i < result.observations.size(); ++i) {
            const auto& o = result.observations[i];
            const auto value = std::atof(o.value.c_str());

            auto* tenorItem = new QTableWidgetItem(QString::fromStdString(o.point_id));
            tenorItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
            self->gridTable_->setItem(static_cast<int>(i), 0, tenorItem);
            auto* rateItem = new QTableWidgetItem(QString::number(value, 'f', 6));
            rateItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
            self->gridTable_->setItem(static_cast<int>(i), 1, rateItem);
            auto* observedItem = new QTableWidgetItem(format_datetime(o.observation_datetime));
            observedItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
            self->gridTable_->setItem(static_cast<int>(i), 2, observedItem);

            categories << QString::fromStdString(o.point_id);
            series->append(static_cast<double>(i), value);
            yMin = std::min(yMin, value);
            yMax = std::max(yMax, value);
        }
        self->gridTable_->resizeColumnsToContents();

        self->gridChart_->removeAllSeries();
        self->gridAxisX_->clear();
        self->gridAxisX_->append(categories);
        self->gridChart_->addSeries(series);
        series->attachAxis(self->gridAxisX_);
        series->attachAxis(self->gridAxisY_);
        if (!result.observations.empty()) {
            if (yMin > yMax) {
                yMin = 0.0;
                yMax = 1.0;
            }
            const double pad = (yMax - yMin) * 0.1 + 1e-6;
            self->gridAxisY_->setRange(yMin - pad, yMax + pad);
        }

        emit self->statusChanged(tr("Loaded %1 points.").arg(result.observations.size()));
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void CurveSnapshotMdiWindow::loadHistory() {
    m::get_curve_snapshot_buckets_request req;
    req.series_type = seriesType_;
    req.metric = metric_;
    req.qualifier = qualifier_;
    const auto unit_seconds = bucketUnitCombo_->currentData().toLongLong();
    req.bucket_seconds = static_cast<std::int64_t>(bucketSizeSpin_->value()) * unit_seconds;
    req.bucket_count = static_cast<std::uint32_t>(bucketCountSpin_->value());

    QPointer<CurveSnapshotMdiWindow> self = this;
    auto* cm = clientManager_;

    struct Result {
        bool success = false;
        QString message;
        std::vector<std::vector<md::market_observation>> buckets;
    };

    auto task = [cm, req]() -> Result {
        auto resp = cm->process_authenticated_request(req);
        if (!resp)
            return {false, QString::fromStdString(resp.error()), {}};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message), {}};
        return {true, {}, std::move(resp->buckets)};
    };

    auto* watcher = new QFutureWatcher<Result>(self);
    connect(watcher, &QFutureWatcher<Result>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        if (!result.success) {
            emit self->errorOccurred(tr("Failed to load curve history: %1").arg(result.message));
            return;
        }

        for (auto& bucket : result.buckets)
            sort_by_tenor(bucket);

        // Tenor union across all buckets, in tenor order -- a point that only appears in some
        // buckets (e.g. it started ticking mid-window) still gets a row, with blank cells
        // elsewhere.
        std::vector<std::string> tenors;
        for (const auto& bucket : result.buckets)
            for (const auto& o : bucket)
                if (std::find(tenors.begin(), tenors.end(), o.point_id) == tenors.end())
                    tenors.push_back(o.point_id);
        std::sort(tenors.begin(), tenors.end(), [](const auto& a, const auto& b) {
            return approx_tenor_days(a) < approx_tenor_days(b);
        });

        const bool empty = tenors.empty();
        self->historyTable_->setVisible(!empty);
        self->historyChartView_->setVisible(!empty);
        self->historyEmptyLabel_->setVisible(empty);
        if (empty) {
            emit self->statusChanged(tr("No history data yet."));
            return;
        }

        const int bucketCount = static_cast<int>(result.buckets.size());
        const auto mode =
            static_cast<HistoryDisplayMode>(self->historyDisplayModeCombo_->currentData().toInt());

        self->historyTable_->setColumnCount(1 + bucketCount);
        QStringList headers;
        headers << tr("Tenor");
        for (int b = 0; b < bucketCount; ++b) {
            headers
                << (!result.buckets[b].empty() ?
                        format_datetime(result.buckets[b].front().observation_datetime).right(8) :
                        self->tr("--"));
        }
        self->historyTable_->setHorizontalHeaderLabels(headers);
        self->historyTable_->setRowCount(static_cast<int>(tenors.size()));

        // point_id -> value per bucket, for both the table and the delta calc.
        std::vector<std::map<std::string, double>> byBucket(bucketCount);
        for (int b = 0; b < bucketCount; ++b)
            for (const auto& o : result.buckets[b])
                byBucket[b][o.point_id] = std::atof(o.value.c_str());

        self->historyChart_->removeAllSeries();
        self->historyAxisX_->clear();
        QStringList categories;
        for (const auto& t : tenors)
            categories << QString::fromStdString(t);
        self->historyAxisX_->append(categories);

        // Rebuild the legend: one swatch + timestamp per bucket, same fade-with-age mapping as
        // the chart lines, so the reader can tell which line is which.
        auto* legendLayout = qobject_cast<QHBoxLayout*>(self->historyLegend_->layout());
        QLayoutItem* child;
        while ((child = legendLayout->takeAt(0)) != nullptr) {
            delete child->widget();
            delete child;
        }

        double yMin = std::numeric_limits<double>::max();
        double yMax = std::numeric_limits<double>::lowest();

        for (int b = 0; b < bucketCount; ++b) {
            for (std::size_t r = 0; r < tenors.size(); ++r) {
                const auto& tenor = tenors[r];
                auto it = byBucket[b].find(tenor);
                std::optional<double> delta; // rate units (mode DeltaRate) or bp (mode DeltaBp)
                if (it != byBucket[b].end() && b > 0) {
                    auto prevIt = byBucket[b - 1].find(tenor);
                    if (prevIt != byBucket[b - 1].end()) {
                        const double raw = it->second - prevIt->second;
                        delta = mode == HistoryDisplayMode::DeltaBp ? raw * 10000.0 : raw;
                    }
                }

                QString cellText = "--";
                // Base bucket always shows the actual rate regardless of mode -- nothing to
                // diff against yet. Rate mode shows the actual rate in every column.
                if (it != byBucket[b].end() && (b == 0 || mode == HistoryDisplayMode::Rate))
                    cellText = QString::number(it->second, 'f', 6);
                else if (delta)
                    cellText = mode == HistoryDisplayMode::DeltaBp ?
                                   bp_arrow_text(*delta) :
                                   (*delta >= 0 ? QStringLiteral("↑ ") : QStringLiteral("↓ ")) +
                                       QString::number(std::abs(*delta), 'f', 6);

                auto* item = new QTableWidgetItem(cellText);
                item->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
                if (delta && !(b == 0 || mode == HistoryDisplayMode::Rate))
                    item->setForeground(QBrush(*delta >= 0 ? k_up_color : k_down_color));
                self->historyTable_->setItem(static_cast<int>(r), 1 + b, item);
            }

            auto* series = new QLineSeries();
            for (std::size_t r = 0; r < tenors.size(); ++r) {
                auto it = byBucket[b].find(tenors[r]);
                if (it == byBucket[b].end())
                    continue;
                series->append(static_cast<double>(r), it->second);
                yMin = std::min(yMin, it->second);
                yMax = std::max(yMax, it->second);
            }
            // Shades of blue, light (oldest) to dark (newest) -- same colour family, still
            // distinguishable per bucket without relying on alpha fading alone.
            const bool isNewest = (b == bucketCount - 1);
            const QColor color = history_series_color(b, bucketCount);
            QPen pen(color);
            pen.setWidthF(isNewest ? 2.5 : 1.5);
            series->setPen(pen);

            self->historyChart_->addSeries(series);
            series->attachAxis(self->historyAxisX_);
            series->attachAxis(self->historyAxisY_);

            const QString label =
                !result.buckets[b].empty() ?
                    format_datetime(result.buckets[b].front().observation_datetime).right(8) :
                    self->tr("--");
            auto* swatch = new QLabel(self->historyLegend_);
            swatch->setFixedSize(14, 3);
            swatch->setStyleSheet(
                QString("background-color: %1;").arg(color.name(QColor::HexArgb)));
            auto* text =
                new QLabel(isNewest ? label + self->tr(" (current)") : label, self->historyLegend_);
            text->setStyleSheet(QString("color: %1; font-size: 9pt;").arg(color.name()));
            legendLayout->addWidget(swatch);
            legendLayout->addWidget(text);
            legendLayout->addSpacing(8);
        }
        legendLayout->addStretch(1);

        if (yMin <= yMax) {
            const double pad = (yMax - yMin) * 0.1 + 1e-6;
            self->historyAxisY_->setRange(yMin - pad, yMax + pad);
        }

        for (std::size_t r = 0; r < tenors.size(); ++r) {
            auto* tenorItem = new QTableWidgetItem(QString::fromStdString(tenors[r]));
            tenorItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
            self->historyTable_->setItem(static_cast<int>(r), 0, tenorItem);
        }
        self->historyTable_->resizeColumnsToContents();

        emit self->statusChanged(tr("Loaded %1 buckets.").arg(bucketCount));
    });

    watcher->setFuture(QtConcurrent::run(task));
}

QString CurveSnapshotMdiWindow::exportFileNameSlug() const {
    QString slug = QString::fromStdString(seriesType_ + "_" + metric_ + "_" + qualifier_);
    slug.replace('/', '_').replace(' ', '_');
    return slug;
}

void CurveSnapshotMdiWindow::exportGridToCsv() {
    if (lastGridObservations_.empty()) {
        QMessageBox::information(this, tr("No Data"), tr("There are no points to export."));
        return;
    }

    const QString fileName = QFileDialog::getSaveFileName(
        this,
        tr("Export to CSV"),
        QStringLiteral("%1_%2.csv")
            .arg(exportFileNameSlug(),
                 QDateTime::currentDateTimeUtc().toString(QStringLiteral("yyyyMMdd'T'HHmmss'Z'"))),
        tr("CSV Files (*.csv);;All Files (*)"));
    if (fileName.isEmpty())
        return;

    try {
        std::ostringstream out;
        out << std::fixed << std::setprecision(6);
        out << "series_type,metric,qualifier,tenor,rate,observed\n";
        for (const auto& o : lastGridObservations_) {
            out << seriesType_ << ',' << metric_ << ',' << qualifier_ << ',' << o.point_id << ','
                << std::atof(o.value.c_str()) << ','
                << format_datetime(o.observation_datetime).toStdString() << '\n';
        }

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            MessageBoxHelper::critical(
                this, tr("File Error"), tr("Could not open file for writing: %1").arg(fileName));
            return;
        }
        const auto csvData = out.str();
        file.write(csvData.c_str(), static_cast<qint64>(csvData.size()));
        file.close();

        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));
        emit statusChanged(tr("Successfully exported curve snapshot to %1").arg(fileName));
    } catch (const std::exception& e) {
        MessageBoxHelper::critical(
            this, tr("Export Error"), tr("Error during CSV export: %1").arg(e.what()));
    }
}

void CurveSnapshotMdiWindow::exportGridToOre() {
    if (lastGridObservations_.empty()) {
        QMessageBox::information(this, tr("No Data"), tr("There are no points to export."));
        return;
    }

    const QString fileName = QFileDialog::getSaveFileName(
        this,
        tr("Export to ORE Market Data"),
        QStringLiteral("%1_%2.txt")
            .arg(exportFileNameSlug(),
                 QDateTime::currentDateTimeUtc().toString(QStringLiteral("yyyyMMdd'T'HHmmss'Z'"))),
        tr("Text Files (*.txt);;All Files (*)"));
    if (fileName.isEmpty())
        return;

    std::vector<ores::ore::market::market_datum> data;
    data.reserve(lastGridObservations_.size());
    for (const auto& o : lastGridObservations_) {
        ores::ore::market::market_datum datum;
        datum.date = std::chrono::year_month_day{
            std::chrono::floor<std::chrono::days>(o.observation_datetime)};
        datum.key = seriesType_ + "/" + metric_ + "/" + qualifier_ + "/" + o.point_id;
        datum.value = o.value;
        data.push_back(std::move(datum));
    }

    try {
        std::ostringstream out;
        ores::ore::market::serialize_market_data(out, data);

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            MessageBoxHelper::critical(
                this, tr("File Error"), tr("Could not open file for writing: %1").arg(fileName));
            return;
        }
        const auto oreData = out.str();
        file.write(oreData.c_str(), static_cast<qint64>(oreData.size()));
        file.close();

        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));
        emit statusChanged(tr("Successfully exported curve snapshot to %1").arg(fileName));
    } catch (const std::exception& e) {
        MessageBoxHelper::critical(
            this, tr("Export Error"), tr("Error during ORE export: %1").arg(e.what()));
    }
}

}

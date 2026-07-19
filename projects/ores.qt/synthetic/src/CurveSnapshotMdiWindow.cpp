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
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.marketdata.api/messaging/curve_snapshot_protocol.hpp"
#include <QComboBox>
#include <QDateTime>
#include <QFont>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QPainter>
#include <QPointer>
#include <QSpinBox>
#include <QTabWidget>
#include <QTimeZone>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QToolBar>
#include <QVBoxLayout>
#include <QtCharts/QBarCategoryAxis>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <QtConcurrent>
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <map>
#include <optional>

namespace ores::qt {

using namespace ores::logging;

namespace {

namespace m = ores::marketdata::messaging;
namespace md = ores::marketdata::domain;

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
    case 'D': return n;
    case 'W': return n * 7.0;
    case 'M': return n * 30.0;
    case 'Y': return n * 365.0;
    default: return std::numeric_limits<double>::max();
    }
}

void sort_by_tenor(std::vector<md::market_observation>& obs) {
    std::sort(obs.begin(), obs.end(), [](const auto& a, const auto& b) {
        return approx_tenor_days(a.point_id) < approx_tenor_days(b.point_id);
    });
}

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
                                               std::string currencyCode,
                                               std::string indexName,
                                               QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , currencyCode_(std::move(currencyCode))
    , indexName_(std::move(indexName))
    , toolbar_(nullptr)
    , reloadAction_(nullptr)
    , tabs_(nullptr)
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
    , historyTable_(nullptr)
    , historyChart_(nullptr)
    , historyChartView_(nullptr)
    , historyAxisX_(nullptr)
    , historyAxisY_(nullptr)
    , historyEmptyLabel_(nullptr) {

    // The full index code carries a redundant "<CCY>-" prefix (see
    // ir_curve_generation_config.index_name doc); strip it to build the market_series
    // qualifier the same way curve_feed_ingest_loop/ir_curve_feed do.
    std::string suffix = indexName_;
    const std::string prefix = currencyCode_ + "-";
    if (suffix.starts_with(prefix))
        suffix = suffix.substr(prefix.size());
    qualifier_ = currencyCode_ + "/" + suffix;

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
    layout->addWidget(toolbar_);

    tabs_ = new QTabWidget(this);
    tabs_->addTab(buildGridTab(), tr("Grid"));
    tabs_->addTab(buildHistoryTab(), tr("History"));
    layout->addWidget(tabs_);
}

QWidget* CurveSnapshotMdiWindow::buildGridTab() {
    auto* page = new QWidget(this);
    auto* layout = new QVBoxLayout(page);

    gridHeaderLabel_ = new QLabel(page);
    QFont headerFont = gridHeaderLabel_->font();
    headerFont.setBold(true);
    gridHeaderLabel_->setFont(headerFont);
    gridHeaderLabel_->setText(
        QString::fromStdString(currencyCode_ + " / " + indexName_));
    layout->addWidget(gridHeaderLabel_);

    auto* row = new QHBoxLayout();

    gridTable_ = new QTableWidget(0, 3, page);
    gridTable_->setHorizontalHeaderLabels({tr("Tenor"), tr("Rate"), tr("Observed")});
    gridTable_->horizontalHeader()->setStretchLastSection(true);
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
    bucketSizeSpin_->setValue(30);
    controls->addWidget(bucketSizeSpin_);

    bucketUnitCombo_ = new QComboBox(page);
    bucketUnitCombo_->addItem(tr("minutes"), QVariant::fromValue<qlonglong>(60));
    bucketUnitCombo_->addItem(tr("hours"), QVariant::fromValue<qlonglong>(3600));
    bucketUnitCombo_->addItem(tr("days"), QVariant::fromValue<qlonglong>(86400));
    controls->addWidget(bucketUnitCombo_);

    controls->addSpacing(16);
    controls->addWidget(new QLabel(tr("Show last:"), page));
    bucketCountSpin_ = new QSpinBox(page);
    bucketCountSpin_->setRange(2, 50);
    bucketCountSpin_->setValue(5);
    controls->addWidget(bucketCountSpin_);
    controls->addWidget(new QLabel(tr("buckets"), page));
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
    row->addWidget(historyChartView_, 1);

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
    req.series_type = "RATES";
    req.metric = "YIELD";
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

            self->gridTable_->setItem(
                static_cast<int>(i), 0, new QTableWidgetItem(QString::fromStdString(o.point_id)));
            self->gridTable_->setItem(
                static_cast<int>(i),
                1,
                new QTableWidgetItem(QString::number(value, 'f', 6)));
            self->gridTable_->setItem(static_cast<int>(i),
                                      2,
                                      new QTableWidgetItem(format_datetime(o.observation_datetime)));

            categories << QString::fromStdString(o.point_id);
            series->append(static_cast<double>(i), value);
            yMin = std::min(yMin, value);
            yMax = std::max(yMax, value);
        }

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

        emit self->statusChanged(
            tr("Loaded %1 points.").arg(result.observations.size()));
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void CurveSnapshotMdiWindow::loadHistory() {
    m::get_curve_snapshot_buckets_request req;
    req.series_type = "RATES";
    req.metric = "YIELD";
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
        self->historyTable_->setColumnCount(1 + bucketCount);
        QStringList headers;
        headers << tr("Tenor");
        for (int b = 0; b < bucketCount; ++b) {
            const QString label = !result.buckets[b].empty() ?
                format_datetime(result.buckets[b].front().observation_datetime).right(8) :
                tr("--");
            headers << (b == 0 ? label : (label + QStringLiteral(" Δbp")));
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

        double yMin = std::numeric_limits<double>::max();
        double yMax = std::numeric_limits<double>::lowest();

        for (int b = 0; b < bucketCount; ++b) {
            std::optional<double> prevValueForFirstTenor;
            for (std::size_t r = 0; r < tenors.size(); ++r) {
                const auto& tenor = tenors[r];
                auto it = byBucket[b].find(tenor);
                QString cellText = "--";
                if (it != byBucket[b].end()) {
                    cellText = QString::number(it->second, 'f', 6);
                    if (b > 0) {
                        auto prevIt = byBucket[b - 1].find(tenor);
                        if (prevIt != byBucket[b - 1].end()) {
                            const double deltaBp = (it->second - prevIt->second) * 10000.0;
                            cellText += QString(" (%1%2)")
                                            .arg(deltaBp >= 0 ? "+" : "")
                                            .arg(deltaBp, 0, 'f', 1);
                        }
                    }
                }
                self->historyTable_->setItem(
                    static_cast<int>(r), 1 + b, new QTableWidgetItem(cellText));
                (void)prevValueForFirstTenor;
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
            // Newest (last) bucket bold/opaque; older buckets fade with age.
            const bool isNewest = (b == bucketCount - 1);
            QPen pen = series->pen();
            pen.setWidthF(isNewest ? 2.5 : 1.0);
            QColor color = pen.color().isValid() ? pen.color() : QColor(0x4f, 0x9e, 0xf5);
            color.setAlphaF(isNewest ? 1.0 :
                                       std::max(0.15, 0.15 + 0.85 * (double(b + 1) / bucketCount)));
            pen.setColor(color);
            series->setPen(pen);

            self->historyChart_->addSeries(series);
            series->attachAxis(self->historyAxisX_);
            series->attachAxis(self->historyAxisY_);
        }

        if (yMin <= yMax) {
            const double pad = (yMax - yMin) * 0.1 + 1e-6;
            self->historyAxisY_->setRange(yMin - pad, yMax + pad);
        }

        for (std::size_t r = 0; r < tenors.size(); ++r)
            self->historyTable_->setItem(
                static_cast<int>(r), 0, new QTableWidgetItem(QString::fromStdString(tenors[r])));

        emit self->statusChanged(
            tr("Loaded %1 buckets.").arg(bucketCount));
    });

    watcher->setFuture(QtConcurrent::run(task));
}

}

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
#include "ores.qt/QueueChartWindow.hpp"

#include <QLabel>
#include <QPainter>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <QtCharts/QChart>
#include <QtCharts/QLineSeries>
#include <QtCharts/QDateTimeAxis>
#include <QtCharts/QValueAxis>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.mq/messaging/mq_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

QueueChartWindow::QueueChartWindow(const QString& queueId,
                                   const QString& queueName,
                                   ClientManager* clientManager,
                                   QWidget* parent)
    : QWidget(parent),
      queueId_(queueId),
      queueName_(queueName),
      clientManager_(clientManager),
      toolbar_(nullptr),
      reloadAction_(nullptr),
      rangeCombo_(nullptr),
      chartView_(nullptr),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &QueueChartWindow::onDataLoaded);

    setupUi();
    reload();
}

void QueueChartWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupChart();
    layout->addWidget(chartView_);
}

void QueueChartWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise,
                                       IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered,
            this, &QueueChartWindow::reload);

    toolbar_->addSeparator();

    // Time range selector
    auto* rangeLabel = new QLabel(tr("  Range:  "), toolbar_);
    toolbar_->addWidget(rangeLabel);

    rangeCombo_ = new QComboBox(toolbar_);
    rangeCombo_->addItem(tr("Last 1 hour"),   static_cast<int>(TimeRange::LastHour));
    rangeCombo_->addItem(tr("Last 6 hours"),  static_cast<int>(TimeRange::Last6Hours));
    rangeCombo_->addItem(tr("Last 24 hours"), static_cast<int>(TimeRange::Last24Hours));
    rangeCombo_->addItem(tr("Last 7 days"),   static_cast<int>(TimeRange::Last7Days));
    rangeCombo_->addItem(tr("All time"),      static_cast<int>(TimeRange::AllTime));
    rangeCombo_->setCurrentIndex(0);
    toolbar_->addWidget(rangeCombo_);

    connect(rangeCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &QueueChartWindow::onTimeRangeChanged);
}

void QueueChartWindow::setupChart() {
    auto* chart = new QChart();
    chart->setTheme(QChart::ChartThemeDark);
    chart->setTitle(tr("Queue: %1  —  loading…").arg(queueName_));
    chart->legend()->setVisible(true);
    chart->legend()->setAlignment(Qt::AlignBottom);

    chartView_ = new QChartView(chart, this);
    chartView_->setRenderHint(QPainter::Antialiasing);
}

std::optional<std::chrono::system_clock::time_point>
QueueChartWindow::fromForRange() const {
    const auto now = std::chrono::system_clock::now();
    switch (currentRange_) {
    case TimeRange::LastHour:
        return now - std::chrono::hours(1);
    case TimeRange::Last6Hours:
        return now - std::chrono::hours(6);
    case TimeRange::Last24Hours:
        return now - std::chrono::hours(24);
    case TimeRange::Last7Days:
        return now - std::chrono::hours(24 * 7);
    case TimeRange::AllTime:
    default:
        return {};
    }
}

void QueueChartWindow::reload() {
    if (isFetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, skipping reload";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        clearChart(tr("Not connected to server"));
        return;
    }

    isFetching_ = true;
    reloadAction_->setEnabled(false);
    rangeCombo_->setEnabled(false);
    emit statusChanged(tr("Loading chart for '%1'…").arg(queueName_));

    const QString queueId = queueId_;
    const auto from = fromForRange();
    QPointer<QueueChartWindow> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, queueId]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .samples = {},
                        .error_message = "Window was destroyed", .error_details = {}};
            }

            mq::messaging::get_queue_stats_request request;

            auto result = self->clientManager_->process_authenticated_request(
                std::move(request));

            if (!result) {
                return {.success = false, .samples = {},
                        .error_message = tr("Request failed"),
                        .error_details = {}};
            }

            if (!result->success) {
                return {.success = false, .samples = {},
                        .error_message = QString::fromStdString(result->message),
                        .error_details = {}};
            }

            // Filter stats to only those matching this queue
            std::vector<mq::domain::queue_stats> filtered;
            const std::string queueIdStr = queueId.toStdString();
            for (const auto& s : result->stats) {
                if (boost::uuids::to_string(s.queue_id) == queueIdStr) {
                    filtered.push_back(s);
                }
            }

            return {.success = true, .samples = std::move(filtered),
                    .error_message = {}, .error_details = {}};
        }, "queue stats");
    });

    watcher_->setFuture(future);
}

void QueueChartWindow::onDataLoaded() {
    isFetching_ = false;
    reloadAction_->setEnabled(true);
    rangeCombo_->setEnabled(true);

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load chart data: "
                                   << result.error_message.toStdString();
        emit errorOccurred(result.error_message);
        clearChart(tr("Error: %1").arg(result.error_message));
        return;
    }

    if (result.samples.empty()) {
        clearChart(tr("Queue '%1' — no samples recorded yet.")
                   .arg(queueName_));
        emit statusChanged(tr("No data for '%1'").arg(queueName_));
        return;
    }

    populateChart(result.samples);
    emit statusChanged(tr("Loaded %1 samples for '%2'")
                       .arg(result.samples.size()).arg(queueName_));
}

void QueueChartWindow::clearChart(const QString& message) {
    auto* chart = chartView_->chart();
    chart->removeAllSeries();
    for (auto* axis : chart->axes())
        chart->removeAxis(axis);
    chart->setTitle(message);
}

void QueueChartWindow::populateChart(
    const std::vector<mq::domain::queue_stats>& samples) {

    auto* chart = chartView_->chart();
    chart->removeAllSeries();
    for (auto* axis : chart->axes())
        chart->removeAxis(axis);

    auto* pending_series = new QLineSeries();
    pending_series->setName(tr("Pending"));

    auto* archived_series = new QLineSeries();
    archived_series->setName(tr("Total Archived"));

    qreal max_pending  = 0;
    qreal max_archived = 0;

    for (const auto& s : samples) {
        const qreal t = static_cast<qreal>(
            std::chrono::duration_cast<std::chrono::milliseconds>(
                s.recorded_at.time_since_epoch()).count());
        const qreal pending  = static_cast<qreal>(s.pending_count);
        const qreal archived = static_cast<qreal>(s.total_archived);
        pending_series->append(t, pending);
        archived_series->append(t, archived);
        max_pending  = std::max(max_pending,  pending);
        max_archived = std::max(max_archived, archived);
    }

    chart->addSeries(pending_series);
    chart->addSeries(archived_series);

    // X axis: datetime
    auto* x_axis = new QDateTimeAxis();
    const bool wide_range = (currentRange_ == TimeRange::Last7Days ||
                             currentRange_ == TimeRange::AllTime);
    x_axis->setFormat(wide_range ? "MM-dd hh:mm" : "hh:mm");
    x_axis->setTitleText(tr("Time (UTC)"));
    x_axis->setTickCount(8);
    chart->addAxis(x_axis, Qt::AlignBottom);
    pending_series->attachAxis(x_axis);
    archived_series->attachAxis(x_axis);

    // Left Y axis: pending count
    auto* y_left = new QValueAxis();
    y_left->setTitleText(tr("Pending"));
    y_left->setLabelFormat("%d");
    y_left->setMin(0);
    y_left->setMax(std::max(max_pending * 1.1, 1.0));
    chart->addAxis(y_left, Qt::AlignLeft);
    pending_series->attachAxis(y_left);

    // Right Y axis: total archived
    auto* y_right = new QValueAxis();
    y_right->setTitleText(tr("Total Archived"));
    y_right->setLabelFormat("%d");
    y_right->setMin(0);
    y_right->setMax(std::max(max_archived * 1.1, 1.0));
    chart->addAxis(y_right, Qt::AlignRight);
    archived_series->attachAxis(y_right);

    chart->setTitle(tr("Queue: %1  (%2 samples)")
                    .arg(queueName_).arg(static_cast<int>(samples.size())));
}

void QueueChartWindow::onTimeRangeChanged(int index) {
    currentRange_ = static_cast<TimeRange>(rangeCombo_->itemData(index).toInt());
    reload();
}

}

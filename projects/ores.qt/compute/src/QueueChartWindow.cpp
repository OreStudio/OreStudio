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
#include <QVBoxLayout>
#include <QtCharts/QChart>
#include <QtCharts/QDateTimeAxis>
#include <QtCharts/QValueAxis>
#include "ores.qt/IconUtils.hpp"

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
      chartView_(nullptr) {

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

    // Time range selector (retained for future use)
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
    chart->setTitle(tr("Stream: %1").arg(queueName_));
    chart->legend()->setVisible(false);

    chartView_ = new QChartView(chart, this);
    chartView_->setRenderHint(QPainter::Antialiasing);
}

void QueueChartWindow::reload() {
    clearChart(tr("Stream: %1\n\nHistorical statistics recording is not yet available.\n"
                  "See backlog: 'Record NATS JetStream stats to TimescaleDB'.")
               .arg(queueName_));
    emit statusChanged(tr("No historical data for '%1'").arg(queueName_));
}

void QueueChartWindow::clearChart(const QString& message) {
    auto* chart = chartView_->chart();
    chart->removeAllSeries();
    for (auto* axis : chart->axes())
        chart->removeAxis(axis);
    chart->setTitle(message);
}

void QueueChartWindow::onTimeRangeChanged(int index) {
    currentRange_ = static_cast<TimeRange>(rangeCombo_->itemData(index).toInt());
    reload();
}

}

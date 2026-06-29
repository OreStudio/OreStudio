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
#include "ores.qt/ReturnDistributionChart.hpp"
#include <QPalette>
#include <QVBoxLayout>
#include <QtCharts/QAreaSeries>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <algorithm>
#include <cmath>

namespace ores::qt {

namespace {

constexpr double pi = 3.14159265358979323846;

// Smallest effective σ (in %) used for rendering, so a σ≈0 ("constant")
// component shows as a tall narrow spike rather than vanishing.
constexpr double min_render_sigma_pct = 0.002;

double gaussian(double x, double mean, double stdev) {
    const double s = std::max(stdev, min_render_sigma_pct);
    const double z = (x - mean) / s;
    return std::exp(-0.5 * z * z) / (s * std::sqrt(2.0 * pi));
}

}

ReturnDistributionChart::ReturnDistributionChart(QWidget* parent)
    : QWidget(parent)
    , chart_(new QChart())
    , view_(new QChartView(chart_, this))
    , axisX_(new QValueAxis(this))
    , axisY_(new QValueAxis(this)) {

    // Dark theme baseline; then make text/grid match the app palette.
    chart_->setTheme(QChart::ChartThemeDark);
    chart_->setBackgroundBrush(Qt::NoBrush);
    chart_->setPlotAreaBackgroundVisible(false);
    chart_->setTitle(tr("Live Return Distribution (GMM)"));
    chart_->legend()->setVisible(false);
    chart_->setMargins(QMargins(4, 4, 4, 4));

    const QColor textColor = palette().color(QPalette::WindowText);
    const QColor gridColor(textColor.red(), textColor.green(), textColor.blue(), 40);
    chart_->setTitleBrush(textColor);

    axisX_->setTitleText(tr("Return per Update (%)"));
    axisY_->setTitleText(tr("Probability"));
    for (auto* axis : {axisX_, axisY_}) {
        axis->setTitleBrush(textColor);
        axis->setLabelsColor(textColor);
        axis->setGridLineColor(gridColor);
        axis->setLinePenColor(gridColor);
    }
    chart_->addAxis(axisX_, Qt::AlignBottom);
    chart_->addAxis(axisY_, Qt::AlignLeft);

    view_->setRenderHint(QPainter::Antialiasing);
    view_->setBackgroundBrush(Qt::NoBrush);
    view_->setStyleSheet("background: transparent;");
    view_->setMinimumHeight(220);

    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(view_);
}

void ReturnDistributionChart::setComponents(const std::vector<Component>& components) {
    chart_->removeAllSeries();

    if (components.empty()) {
        axisX_->setRange(-1.0, 1.0);
        axisY_->setRange(0.0, 1.0);
        return;
    }

    // Display in percent: multiply means/stdevs by 100.
    double maxStdev = 0.0;
    double minMean = 0.0;
    double maxMean = 0.0;
    bool first = true;
    for (const auto& c : components) {
        const double m = c.mean * 100.0;
        const double s = c.stdev * 100.0;
        maxStdev = std::max(maxStdev, s);
        if (first) {
            minMean = maxMean = m;
            first = false;
        } else {
            minMean = std::min(minMean, m);
            maxMean = std::max(maxMean, m);
        }
    }

    // X range covers ±4σ around the spread of means. Use the effective render
    // sigma so the window scales monotonically with σ: σ≈0 → tight window (a
    // narrow spike fills it), larger σ → progressively wider window.
    const double effStdev = std::max(maxStdev, min_render_sigma_pct);
    const double span = effStdev * 4.0;
    const double xMin = minMean - span;
    const double xMax = maxMean + span;

    constexpr int samples = 256;
    auto* line = new QLineSeries(this);
    double yMax = 0.0;
    for (int i = 0; i <= samples; ++i) {
        const double x = xMin + (xMax - xMin) * i / samples;
        double y = 0.0;
        for (const auto& c : components)
            y += c.weight * gaussian(x, c.mean * 100.0, c.stdev * 100.0);
        line->append(x, y);
        yMax = std::max(yMax, y);
    }

    auto* area = new QAreaSeries(line);
    area->setName(tr("Mixture density"));
    QColor fill(70, 130, 180, 110); // steel blue, translucent
    area->setBrush(fill);
    QPen pen(QColor(70, 130, 180));
    pen.setWidth(2);
    area->setPen(pen);

    chart_->addSeries(area);
    area->attachAxis(axisX_);
    area->attachAxis(axisY_);

    axisX_->setRange(xMin, xMax);
    axisY_->setRange(0.0, yMax > 0.0 ? yMax * 1.1 : 1.0);
}

}

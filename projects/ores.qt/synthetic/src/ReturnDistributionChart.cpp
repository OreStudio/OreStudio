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
#include <QVBoxLayout>
#include <QtCharts/QAreaSeries>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <algorithm>
#include <cmath>
#include <utility>
#include <vector>

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

// Qualitative palette for per-component curves; deliberately excludes steel
// blue (the fixed combined-mixture colour, below) so components are always
// visually distinct from the mixture.
const QColor componentPalette[] = {
    QColor(0xFF, 0x6B, 0x6B), // coral
    QColor(0xFF, 0xB6, 0x27), // amber
    QColor(0x4E, 0xCD, 0xC4), // mint
    QColor(0xB3, 0x8D, 0xF5), // violet
    QColor(0xA8, 0xD8, 0x5A), // lime
    QColor(0xF7, 0x81, 0xBF), // pink
    QColor(0xC9, 0xC9, 0xC9), // grey
};
constexpr int componentPaletteSize =
    static_cast<int>(sizeof(componentPalette) / sizeof(componentPalette[0]));

}

QColor ReturnDistributionChart::componentColor(int index) {
    const int i = ((index % componentPaletteSize) + componentPaletteSize) % componentPaletteSize;
    return componentPalette[i];
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

    // The app's dark theme is applied via QSS only, so QWidget::palette() still
    // returns Qt's default (light-mode) colours here — use explicit theme
    // colours instead, matching FxSpotChartWindow's chart styling.
    const QColor textColor(0xCB, 0xD5, 0xE1);
    const QColor gridColor(255, 255, 255, 18);
    chart_->setTitleBrush(textColor);

    axisX_->setTitleText(tr("Return per Update (%)"));
    axisY_->setTitleText(tr("Relative likelihood"));
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

void ReturnDistributionChart::setDomain(Domain domain) {
    domain_ = domain;
    if (domain_ == Domain::Price) {
        chart_->setTitle(tr("Steady-State Price Distribution"));
        axisX_->setTitleText(tr("Price"));
        axisX_->setLabelFormat(QStringLiteral("%.4f")); // FX-rate precision, not %
    } else {
        chart_->setTitle(tr("Live Return Distribution (GMM)"));
        axisX_->setTitleText(tr("Return per Update (%)"));
        axisX_->setLabelFormat(QString()); // back to Qt's default numeric format
    }
}

void ReturnDistributionChart::setComponents(const std::vector<Component>& components) {
    chart_->removeAllSeries();

    if (components.empty()) {
        axisX_->setRange(-1.0, 1.0);
        axisY_->setRange(0.0, 1.0);
        return;
    }

    // Return domain displays in percent (×100); Price domain plots raw values
    // (a price level, not a return, so no rescaling applies).
    const double scale = domain_ == Domain::Price ? 1.0 : 100.0;

    double maxStdev = 0.0;
    double minMean = 0.0;
    double maxMean = 0.0;
    bool first = true;
    for (const auto& c : components) {
        const double m = c.mean * scale;
        const double s = c.stdev * scale;
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
    // Compute each component's weighted contribution alongside the raw mixture
    // density, then normalise everything to the mixture's peak of 1 so the
    // chart shows the distribution SHAPE on a clean 0..1 "relative likelihood"
    // scale, rather than raw density values (which can be huge for a narrow
    // distribution and read confusingly as a probability > 1).
    std::vector<std::pair<double, double>> pts;
    pts.reserve(samples + 1);
    std::vector<std::vector<double>> componentYs(components.size(),
                                                 std::vector<double>(samples + 1, 0.0));
    double yMax = 0.0;
    for (int i = 0; i <= samples; ++i) {
        const double x = xMin + (xMax - xMin) * i / samples;
        double y = 0.0;
        for (std::size_t ci = 0; ci < components.size(); ++ci) {
            const auto& c = components[ci];
            const double yi = c.weight * gaussian(x, c.mean * scale, c.stdev * scale);
            componentYs[ci][i] = yi;
            y += yi;
        }
        pts.emplace_back(x, y);
        yMax = std::max(yMax, y);
    }
    const double norm = yMax > 0.0 ? yMax : 1.0;

    auto* line = new QLineSeries(this);
    for (const auto& [x, y] : pts)
        line->append(x, y / norm);

    auto* area = new QAreaSeries(line);
    // QAreaSeries does not take ownership of its line, and chart_->removeAllSeries()
    // only deletes the area (the series in the chart's list). Parent the line to the
    // area so it is destroyed together instead of leaking on every refresh.
    line->setParent(area);
    area->setName(tr("Mixture density"));
    QColor fill(70, 130, 180, 110); // steel blue, translucent
    area->setBrush(fill);
    QPen pen(QColor(70, 130, 180));
    pen.setWidth(2);
    area->setPen(pen);

    chart_->addSeries(area);
    area->attachAxis(axisX_);
    area->attachAxis(axisY_);

    // Per-component contribution curves, drawn on top of the mixture fill in
    // colours matching the component table's colour indicator (componentColor()).
    for (std::size_t ci = 0; ci < components.size(); ++ci) {
        auto* compLine = new QLineSeries(this);
        for (int i = 0; i <= samples; ++i) {
            const double x = xMin + (xMax - xMin) * i / samples;
            compLine->append(x, componentYs[ci][i] / norm);
        }
        QPen compPen(componentColor(static_cast<int>(ci)));
        compPen.setWidth(2);
        compLine->setPen(compPen);
        // Named for potential tooling/debugging use, but the chart's own legend is
        // hidden (compact layout) — the colour-to-component mapping is conveyed via
        // FxSpotRateEditor's component-table swatch column, not an in-chart legend.
        compLine->setName(tr("Component %1").arg(ci + 1));
        chart_->addSeries(compLine);
        compLine->attachAxis(axisX_);
        compLine->attachAxis(axisY_);
    }

    axisX_->setRange(xMin, xMax);
    axisY_->setRange(0.0, 1.05);
}

}

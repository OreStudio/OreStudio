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
#include "ores.qt/SamplePricePathsChart.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QLabel>
#include <QPalette>
#include <QPointer>
#include <QPushButton>
#include <QSpinBox>
#include <QTimer>
#include <QVBoxLayout>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <QtConcurrent>
#include <algorithm>
#include <limits>

namespace ores::qt {

using namespace ores::logging;

SamplePricePathsChart::SamplePricePathsChart(ClientManager* cm, QWidget* parent)
    : QWidget(parent)
    , clientManager_(cm)
    , chart_(new QChart())
    , view_(new QChartView(chart_, this))
    , axisX_(new QValueAxis(this))
    , axisY_(new QValueAxis(this))
    , pathsSpin_(new QSpinBox(this))
    , ticksSpin_(new QSpinBox(this))
    , debounce_(new QTimer(this)) {

    // Dark theme baseline; then match text/grid to the app palette.
    chart_->setTheme(QChart::ChartThemeDark);
    chart_->setBackgroundBrush(Qt::NoBrush);
    chart_->setPlotAreaBackgroundVisible(false);
    chart_->setTitle(tr("Live Sample Price Paths"));
    chart_->legend()->setVisible(false);
    chart_->setMargins(QMargins(4, 4, 4, 4));

    const QColor textColor = palette().color(QPalette::WindowText);
    const QColor gridColor(textColor.red(), textColor.green(), textColor.blue(), 40);
    chart_->setTitleBrush(textColor);

    axisX_->setTitleText(tr("Update Steps"));
    axisY_->setTitleText(tr("Price"));
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

    pathsSpin_->setRange(1, 50); // default many paths for a fuller picture
    pathsSpin_->setValue(50);
    pathsSpin_->setToolTip(tr("Number of independent sample paths to draw."));

    ticksSpin_->setRange(10, 5000); // server clamp: ticks ≤ 5000
    ticksSpin_->setValue(100);
    ticksSpin_->setToolTip(tr("Number of update steps per path."));

    reseedBtn_ = new QPushButton(tr("Reseed"), this);
    connect(reseedBtn_, &QPushButton::clicked, this, &SamplePricePathsChart::onReseed);

    auto* topRow = new QHBoxLayout();
    topRow->addWidget(new QLabel(tr("Paths:"), this));
    topRow->addWidget(pathsSpin_);
    topRow->addSpacing(8);
    topRow->addWidget(new QLabel(tr("Ticks per path:"), this));
    topRow->addWidget(ticksSpin_);
    topRow->addStretch(1);
    statusLabel_ = new QLabel(this);
    statusLabel_->setStyleSheet("color:#d0a020;");
    statusLabel_->setVisible(false);
    topRow->addWidget(statusLabel_);
    topRow->addWidget(reseedBtn_);

    connect(pathsSpin_, &QSpinBox::valueChanged, this,
            [this](int) { scheduleRefresh(); });
    connect(ticksSpin_, &QSpinBox::valueChanged, this,
            [this](int) { scheduleRefresh(); });

    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addLayout(topRow);
    layout->addWidget(view_, 1);

    debounce_->setSingleShot(true);
    debounce_->setInterval(400);
    connect(debounce_, &QTimer::timeout, this, &SamplePricePathsChart::doRefresh);
}

void SamplePricePathsChart::setControlsEnabled(bool enabled) {
    pathsSpin_->setEnabled(enabled);
    ticksSpin_->setEnabled(enabled);
    reseedBtn_->setEnabled(enabled);
}

void SamplePricePathsChart::setBusy(bool busy) {
    setControlsEnabled(!busy);
    if (busy) {
        chart_->setTitle(tr("Live Sample Price Paths — generating…"));
        statusLabel_->setText(tr("⟳ Generating sample paths…"));
        statusLabel_->setStyleSheet("color:#d0a020;");
        statusLabel_->setVisible(true);
    } else {
        chart_->setTitle(tr("Live Sample Price Paths"));
        statusLabel_->setVisible(false);
    }
}

void SamplePricePathsChart::setComponents(const std::vector<Component>& components) {
    components_ = components;
}

void SamplePricePathsChart::setInitialPrice(double price) {
    initialPrice_ = price;
}

void SamplePricePathsChart::setProcessType(const std::string& processType) {
    processType_ = processType;
}

void SamplePricePathsChart::scheduleRefresh() {
    debounce_->start();
}

void SamplePricePathsChart::onReseed() {
    ++seed_;
    doRefresh();
}

void SamplePricePathsChart::doRefresh() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;
    if (components_.empty())
        return;
    if (inFlight_) {
        // A newer request arrived mid-flight; run it once the current one ends
        // (coalesced — only the latest params/seed matter).
        pending_ = true;
        return;
    }

    namespace m = synthetic::messaging;
    m::simulate_fx_spot_paths_request req;
    for (const auto& c : components_) {
        req.gmm_means.push_back(c.mean);
        req.gmm_stdevs.push_back(c.stdev);
        req.gmm_weights.push_back(c.weight);
    }
    req.initial_price = initialPrice_;
    req.process_type = processType_;
    req.num_ticks = std::clamp(ticksSpin_->value(), 10, 5000);
    req.num_paths = std::clamp(pathsSpin_->value(), 1, 50);
    req.seed = seed_;

    inFlight_ = true;
    pending_ = false;
    setBusy(true);
    BOOST_LOG_SEV(lg(), debug) << "Requesting " << req.num_paths << " sample paths (seed "
                               << req.seed << ").";

    QPointer<SamplePricePathsChart> self = this;
    auto* cm = clientManager_;

    struct Result {
        bool success = false;
        QString message;
        std::vector<std::vector<double>> paths;
    };

    auto task = [cm, req]() -> Result {
        auto resp = cm->process_authenticated_request(req);
        if (!resp)
            return {false, QString::fromStdString(resp.error()), {}};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message), {}};
        return {true, {}, std::move(resp->paths)};
    };

    auto* watcher = new QFutureWatcher<Result>(self);
    connect(watcher, &QFutureWatcher<Result>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        self->inFlight_ = false;
        self->setBusy(false);

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Sample-path simulation failed: " << result.message.toStdString();
            self->statusLabel_->setText(
                self->tr("Sample paths failed: %1").arg(result.message));
            self->statusLabel_->setStyleSheet("color:#d04030;");
            self->statusLabel_->setVisible(true);
            // Still honour a queued refresh (e.g. the user changed params again).
            if (self->pending_)
                self->doRefresh();
            return;
        }

        self->chart_->removeAllSeries();

        double yMin = std::numeric_limits<double>::max();
        double yMax = std::numeric_limits<double>::lowest();
        int maxLen = 0;

        for (const auto& path : result.paths) {
            auto* series = new QLineSeries(self);
            for (std::size_t i = 0; i < path.size(); ++i) {
                series->append(static_cast<double>(i), path[i]);
                yMin = std::min(yMin, path[i]);
                yMax = std::max(yMax, path[i]);
            }
            maxLen = std::max(maxLen, static_cast<int>(path.size()));
            self->chart_->addSeries(series);
            series->attachAxis(self->axisX_);
            series->attachAxis(self->axisY_);
        }

        if (maxLen > 0) {
            self->axisX_->setRange(0.0, maxLen - 1);
            if (yMin > yMax) {
                yMin = 0.0;
                yMax = 1.0;
            }
            const double pad = (yMax - yMin) * 0.05;
            self->axisY_->setRange(yMin - pad, yMax + pad);
        }

        BOOST_LOG_SEV(lg(), debug) << "Plotted " << result.paths.size() << " sample paths.";

        // If params changed while we were generating, run once more.
        if (self->pending_)
            self->doRefresh();
    });

    watcher->setFuture(QtConcurrent::run(task));
}

}

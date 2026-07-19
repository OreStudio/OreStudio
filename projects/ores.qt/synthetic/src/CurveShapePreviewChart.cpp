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
#include "ores.qt/CurveShapePreviewChart.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.synthetic.api/messaging/preview_ir_curve_shape_protocol.hpp"
#include <QFont>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QLabel>
#include <QPointer>
#include <QTime>
#include <QTimer>
#include <QVBoxLayout>
#include <QtCharts/QBarCategoryAxis>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <QtConcurrent>
#include <algorithm>
#include <limits>

namespace ores::qt {

using namespace ores::logging;

CurveShapePreviewChart::CurveShapePreviewChart(ClientManager* cm, QWidget* parent)
    : QWidget(parent)
    , clientManager_(cm)
    , chart_(new QChart())
    , view_(new QChartView(chart_, this))
    , axisX_(new QBarCategoryAxis(this))
    , axisY_(new QValueAxis(this))
    , debounce_(new QTimer(this)) {

    chart_->setTheme(QChart::ChartThemeDark);
    chart_->setBackgroundBrush(Qt::NoBrush);
    chart_->setPlotAreaBackgroundVisible(false);
    chart_->setTitle(tr("Curve Shape Preview"));
    chart_->legend()->setVisible(false);
    chart_->setMargins(QMargins(4, 4, 4, 4));

    const QColor textColor(0xCB, 0xD5, 0xE1);
    const QColor gridColor(255, 255, 255, 18);
    chart_->setTitleBrush(textColor);

    axisX_->setTitleText(tr("Tenor (short-end to long-end)"));
    axisY_->setTitleText(tr("Rate"));
    axisY_->setLabelFormat(QStringLiteral("%.4f"));
    QFont axisLabelFont;
    axisLabelFont.setPointSizeF(7.5); // category labels (tenor codes) are wider than a plain index
    for (auto* axis : {static_cast<QAbstractAxis*>(axisX_), static_cast<QAbstractAxis*>(axisY_)}) {
        axis->setLabelsColor(textColor);
        axis->setLabelsFont(axisLabelFont);
        axis->setGridLineColor(gridColor);
        axis->setLinePenColor(gridColor);
    }
    axisX_->setTitleBrush(textColor);
    axisY_->setTitleBrush(textColor);
    chart_->addAxis(axisX_, Qt::AlignBottom);
    chart_->addAxis(axisY_, Qt::AlignLeft);

    view_->setRenderHint(QPainter::Antialiasing);
    view_->setBackgroundBrush(Qt::NoBrush);
    view_->setStyleSheet("background: transparent;");
    view_->setMinimumHeight(220);

    statusLabel_ = new QLabel(this);
    statusLabel_->setStyleSheet("color:#d0a020;");
    statusLabel_->setVisible(false);

    auto* topRow = new QHBoxLayout();
    topRow->addStretch(1);
    topRow->addWidget(statusLabel_);

    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addLayout(topRow);
    layout->addWidget(view_, 1);

    debounce_->setSingleShot(true);
    debounce_->setInterval(400);
    connect(debounce_, &QTimer::timeout, this, &CurveShapePreviewChart::doRefresh);
}

void CurveShapePreviewChart::setBusy(bool busy) {
    statusLabel_->setStyleSheet("color:#d0a020;");
    if (busy) {
        statusLabel_->setText(tr("⟳ Computing curve shape…"));
    } else {
        statusLabel_->setText(
            tr("Updated at %1").arg(QTime::currentTime().toString(QStringLiteral("HH:mm:ss"))));
    }
    statusLabel_->setVisible(true);
}

void CurveShapePreviewChart::setParameters(const std::string& processType,
                                           double kappa,
                                           double theta,
                                           double sigma,
                                           double initialRate,
                                           const std::string& fixedLegPaymentFrequencyCode,
                                           const std::vector<TemplateRow>& entries) {
    processType_ = processType;
    kappa_ = kappa;
    theta_ = theta;
    sigma_ = sigma;
    initialRate_ = initialRate;
    fixedLegPaymentFrequencyCode_ = fixedLegPaymentFrequencyCode;
    entries_ = entries;
}

void CurveShapePreviewChart::scheduleRefresh() {
    debounce_->start();
}

void CurveShapePreviewChart::doRefresh() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;
    if (entries_.empty()) {
        chart_->removeAllSeries();
        return;
    }
    if (inFlight_) {
        pending_ = true;
        return;
    }

    namespace m = synthetic::messaging;
    m::preview_ir_curve_shape_request req;
    req.process_type = processType_;
    req.kappa = kappa_;
    req.theta = theta_;
    req.sigma = sigma_;
    req.initial_rate = initialRate_;
    req.fixed_leg_payment_frequency_code = fixedLegPaymentFrequencyCode_;
    for (const auto& e : entries_) {
        m::preview_ir_curve_template_row row;
        row.sequence_index = e.sequence_index;
        row.start_tenor_code = e.start_tenor_code;
        row.end_tenor_code = e.end_tenor_code;
        row.instrument_code = e.instrument_code;
        req.entries.push_back(std::move(row));
    }

    inFlight_ = true;
    pending_ = false;
    setBusy(true);

    QPointer<CurveShapePreviewChart> self = this;
    auto* cm = clientManager_;

    struct Result {
        bool success = false;
        QString message;
        std::vector<m::preview_ir_curve_shape_point> points;
    };

    auto task = [cm, req]() -> Result {
        auto resp = cm->process_authenticated_request(req);
        if (!resp)
            return {false, QString::fromStdString(resp.error()), {}};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message), {}};
        return {true, {}, std::move(resp->points)};
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
            self->statusLabel_->setText(self->tr("Curve preview failed: %1").arg(result.message));
            self->statusLabel_->setStyleSheet("color:#d04030;");
            self->statusLabel_->setVisible(true);
            if (self->pending_)
                self->doRefresh();
            return;
        }

        self->chart_->removeAllSeries();

        QStringList categories;
        for (const auto& pt : result.points)
            categories << QString::fromStdString(pt.end_tenor_code);
        self->axisX_->clear();
        self->axisX_->append(categories);

        auto* series = new QLineSeries(self);
        series->setPointsVisible(true);
        double yMin = std::numeric_limits<double>::max();
        double yMax = std::numeric_limits<double>::lowest();
        for (std::size_t i = 0; i < result.points.size(); ++i) {
            const auto& pt = result.points[i];
            series->append(static_cast<double>(i), pt.rate);
            yMin = std::min(yMin, pt.rate);
            yMax = std::max(yMax, pt.rate);
        }
        self->chart_->addSeries(series);
        series->attachAxis(self->axisX_);
        series->attachAxis(self->axisY_);

        if (!result.points.empty()) {
            if (yMin > yMax) {
                yMin = 0.0;
                yMax = 1.0;
            }
            const double pad = (yMax - yMin) * 0.1 + 1e-6;
            self->axisY_->setRange(yMin - pad, yMax + pad);
        }

        if (self->pending_)
            self->doRefresh();
    });

    watcher->setFuture(QtConcurrent::run(task));
}

}

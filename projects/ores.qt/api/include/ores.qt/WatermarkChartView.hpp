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
#ifndef ORES_QT_WATERMARK_CHART_VIEW_HPP
#define ORES_QT_WATERMARK_CHART_VIEW_HPP

#include <QPaintEvent>
#include <QPainter>
#include <QString>
#include <QtCharts/QChartView>

namespace ores::qt {

/**
 * @brief QChartView that paints a faint centred symbol watermark over the plot.
 */
class WatermarkChartView final : public QChartView {
public:
    WatermarkChartView(QChart* chart, QWidget* parent, QString text)
        : QChartView(chart, parent)
        , text_(std::move(text)) {}

    void setText(const QString& text) {
        text_ = text;
        viewport()->update();
    }

protected:
    void paintEvent(QPaintEvent* e) override {
        QChartView::paintEvent(e);
        if (text_.isEmpty())
            return;
        QPainter p(viewport());
        p.setRenderHint(QPainter::Antialiasing);
        QFont f;
        f.setPointSizeF(34);
        f.setBold(true);
        p.setFont(f);
        p.setPen(QColor(255, 255, 255, 24));
        p.drawText(viewport()->rect(), Qt::AlignCenter, text_);
    }

private:
    QString text_;
};

} // namespace ores::qt

#endif

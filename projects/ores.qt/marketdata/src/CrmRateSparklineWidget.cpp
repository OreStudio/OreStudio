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
#include "ores.qt/CrmRateSparklineWidget.hpp"
#include "ores.qt/ColorConstants.hpp"
#include <QPainter>
#include <QPainterPath>
#include <algorithm>

namespace ores::qt {

CrmRateSparklineWidget::CrmRateSparklineWidget(QWidget* parent)
    : QWidget(parent) {
    setMinimumHeight(60);
}

void CrmRateSparklineWidget::setValues(const std::deque<double>& values) {
    values_.assign(values.begin(), values.end());
    update();
}

void CrmRateSparklineWidget::paintEvent(QPaintEvent*) {
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    if (values_.size() < 2) {
        painter.setPen(color_constants::level_trace);
        painter.drawText(rect(), Qt::AlignCenter, tr("Not enough history yet"));
        return;
    }

    const auto min_it = std::min_element(values_.begin(), values_.end());
    const auto max_it = std::max_element(values_.begin(), values_.end());
    double min_v = *min_it;
    double max_v = *max_it;
    if (max_v - min_v < 1e-12) {
        min_v -= 1.0;
        max_v += 1.0;
    }

    const auto w = static_cast<double>(width());
    const auto h = static_cast<double>(height());
    const auto n = static_cast<double>(values_.size() - 1);

    QPainterPath path;
    for (std::size_t i = 0; i < values_.size(); ++i) {
        const auto x = n > 0 ? (static_cast<double>(i) / n) * w : 0.0;
        const auto normalized = (values_[i] - min_v) / (max_v - min_v);
        const auto y = h - (normalized * h);
        if (i == 0)
            path.moveTo(x, y);
        else
            path.lineTo(x, y);
    }

    const auto rising = values_.back() >= values_.front();
    painter.setPen(QPen(rising ? color_constants::level_info : color_constants::level_error, 2));
    painter.drawPath(path);
}

}

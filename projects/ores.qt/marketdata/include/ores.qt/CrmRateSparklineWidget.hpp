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
#ifndef ORES_QT_CRM_RATE_SPARKLINE_WIDGET_HPP
#define ORES_QT_CRM_RATE_SPARKLINE_WIDGET_HPP

#include <QWidget>
#include <deque>
#include <vector>

namespace ores::qt {

/**
 * @brief Minimal client-side sparkline: draws a polyline through a rolling
 * buffer of rates, one point per manual/auto reload of this session. No
 * server-side tick history exists for CRM rates yet -- see the "Add a
 * server-side rate-history endpoint for chart panels" backlog capture --
 * so this only ever shows what a panel itself has locally observed.
 */
class CrmRateSparklineWidget final : public QWidget {
public:
    explicit CrmRateSparklineWidget(QWidget* parent = nullptr);

    void setValues(const std::deque<double>& values);

protected:
    void paintEvent(QPaintEvent*) override;

private:
    std::vector<double> values_;
};

}

#endif

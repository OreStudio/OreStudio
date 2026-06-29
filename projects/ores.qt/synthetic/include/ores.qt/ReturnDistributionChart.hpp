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
#ifndef ORES_QT_RETURN_DISTRIBUTION_CHART_HPP
#define ORES_QT_RETURN_DISTRIBUTION_CHART_HPP

#include <QWidget>
#include <vector>

class QChart;
class QChartView;
class QValueAxis;

namespace ores::qt {

/**
 * @brief Live, client-rendered plot of a Gaussian-mixture return density.
 *
 * Plots f(x) = Σ wᵢ·N(x; μᵢ, σᵢ) where x is the per-update log-return shown
 * in percent (μ, σ are multiplied by 100 for display). No backend call: the
 * curve is recomputed instantly from the supplied components.
 */
class ReturnDistributionChart final : public QWidget {
    Q_OBJECT

public:
    /** @brief One Gaussian mixture component (in raw log-return units). */
    struct Component {
        double mean;
        double stdev;
        double weight;
    };

    explicit ReturnDistributionChart(QWidget* parent = nullptr);
    ~ReturnDistributionChart() override = default;

    /** @brief Recompute and redraw the mixture density. */
    void setComponents(const std::vector<Component>& components);

private:
    QChart* chart_;
    QChartView* view_;
    QValueAxis* axisX_;
    QValueAxis* axisY_;
};

}

#endif

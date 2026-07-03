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

#include <QColor>
#include <QWidget>
#include <vector>

class QChart;
class QChartView;
class QValueAxis;

namespace ores::qt {

/**
 * @brief Live, client-rendered plot of a Gaussian mixture density.
 *
 * Plots f(x) = Σ wᵢ·N(x; μᵢ, σᵢ). No backend call: the curve is recomputed
 * instantly from the supplied components. Two domains are supported (see
 * setDomain()): a per-update log-return mixture (the usual GMM case, shown in
 * percent) or a single-component steady-state price distribution (for a
 * mean-reverting engine like Ornstein-Uhlenbeck, shown in raw price units).
 */
class ReturnDistributionChart final : public QWidget {
    Q_OBJECT

public:
    /** @brief One Gaussian mixture component (in raw log-return units, or — in
     *  Domain::Price — the single steady-state component in price units). */
    struct Component {
        double mean;
        double stdev;
        double weight;
    };

    enum class Domain {
        Return, // per-update log-return mixture, shown in % (the GMM case)
        Price   // steady-state price distribution, shown in raw price units
    };

    explicit ReturnDistributionChart(QWidget* parent = nullptr);
    ~ReturnDistributionChart() override = default;

    /**
     * @brief Switch what setComponents() plots and how the chart is labelled.
     * Domain::Return (the default) scales values ×100 and reads "Return per
     * Update (%)"; Domain::Price plots raw values and reads "Price". Call
     * before setComponents() when switching (e.g. on an engine change).
     */
    void setDomain(Domain domain);

    /** @brief Recompute and redraw the mixture density. */
    void setComponents(const std::vector<Component>& components);

    /**
     * @brief Colour assigned to the component at @p index (cycles through a
     * fixed qualitative palette). Shared with FxSpotRateEditor's component
     * table so each row's colour indicator matches its PDF curve here.
     */
    static QColor componentColor(int index);

private:
    QChart* chart_;
    QChartView* view_;
    QValueAxis* axisX_;
    QValueAxis* axisY_;
    Domain domain_ = Domain::Return;
};

}

#endif

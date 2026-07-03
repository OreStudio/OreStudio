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
#ifndef ORES_QT_SAMPLE_PRICE_PATHS_CHART_HPP
#define ORES_QT_SAMPLE_PRICE_PATHS_CHART_HPP

#include "ores.logging/make_logger.hpp"
#include <QWidget>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

class QChart;
class QChartView;
class QGraphicsSimpleTextItem;
class QValueAxis;
class QSpinBox;
class QPushButton;
class QLabel;

namespace ores::qt {

class ClientManager;

/**
 * @brief Live preview of synthetic sample price paths.
 *
 * On refresh, calls the synthetic simulate service off the UI thread with the
 * current GMM components and initial price and plots each returned path as a
 * line (Price vs Update Steps). Auto-refresh is debounced; the Reseed button
 * forces a new seed and an immediate refresh.
 */
class SamplePricePathsChart final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic.sample_price_paths_chart";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    struct Component {
        double mean;
        double stdev;
        double weight;
    };

    explicit SamplePricePathsChart(ClientManager* cm, QWidget* parent = nullptr);
    ~SamplePricePathsChart() override = default;

    /** @brief Update the model inputs (does not refresh immediately). */
    void setComponents(const std::vector<Component>& components);
    void setInitialPrice(double price);

    /** @brief Set the price-process engine ("geometric" or "arithmetic"). */
    void setProcessType(const std::string& processType);

    /**
     * @brief Draw a dashed horizontal reference line at @p level (e.g. "ou"'s θ),
     * labelled on the chart. Pass std::nullopt to remove it (the default —
     * mean-reversion is "ou"-specific, other engines have no such level).
     */
    void setReferenceLevel(std::optional<double> level);

    /** @brief Schedule a debounced refresh (~400 ms after the last call). */
    void scheduleRefresh();

private slots:
    void onReseed();
    void doRefresh();

private:
    void setControlsEnabled(bool enabled);
    void setBusy(bool busy);

    ClientManager* clientManager_;

    QChart* chart_;
    QChartView* view_;
    QValueAxis* axisX_;
    QValueAxis* axisY_;
    QSpinBox* pathsSpin_;
    QSpinBox* ticksSpin_;
    QPushButton* reseedBtn_;
    QLabel* statusLabel_; // inline busy / error indicator over the chart

    class QTimer* debounce_;

    std::vector<Component> components_;
    double initialPrice_{1.0};
    std::string processType_{"geometric"};
    std::uint32_t seed_{1};
    bool inFlight_{false};
    bool pending_{false}; // a refresh was requested while one was in flight
    std::optional<double> referenceLevel_;
    QGraphicsSimpleTextItem* referenceLabelItem_{nullptr}; // owned by chart_'s scene
};

}

#endif

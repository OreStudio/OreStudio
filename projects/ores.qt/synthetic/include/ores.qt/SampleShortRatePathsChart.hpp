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
#ifndef ORES_QT_SAMPLE_SHORT_RATE_PATHS_CHART_HPP
#define ORES_QT_SAMPLE_SHORT_RATE_PATHS_CHART_HPP

#include "ores.logging/make_logger.hpp"
#include <QWidget>
#include <cstdint>
#include <string>
#include <vector>

class QChart;
class QChartView;
class QValueAxis;
class QSpinBox;
class QPushButton;
class QLabel;

namespace ores::qt {

class ClientManager;

/**
 * @brief Live preview of synthetic short-rate sample paths -- the IR curve analogue of
 * SamplePricePathsChart, calling simulate_ir_curve_paths_request instead of
 * simulate_fx_spot_paths_request. Same debounced-refresh/reseed shape.
 */
class SampleShortRatePathsChart final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.synthetic.sample_short_rate_paths_chart";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SampleShortRatePathsChart(ClientManager* cm, QWidget* parent = nullptr);
    ~SampleShortRatePathsChart() override = default;

    /** @brief Update the model inputs (does not refresh immediately). */
    void setParameters(const std::string& processType,
                       double kappa,
                       double theta,
                       double sigma,
                       double initialRate);

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
    QLabel* statusLabel_;

    class QTimer* debounce_;

    std::string processType_{"vasicek"};
    double kappa_{0.0};
    double theta_{0.0};
    double sigma_{0.0};
    double initialRate_{0.0};
    std::uint32_t seed_{1};
    bool inFlight_{false};
    bool pending_{false};
};

}

#endif

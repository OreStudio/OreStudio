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
#ifndef ORES_QT_CURVE_SHAPE_PREVIEW_CHART_HPP
#define ORES_QT_CURVE_SHAPE_PREVIEW_CHART_HPP

#include "ores.logging/make_logger.hpp"
#include <QWidget>
#include <string>
#include <vector>

class QChart;
class QChartView;
class QValueAxis;
class QLabel;

namespace ores::qt {

class ClientManager;

/**
 * @brief Live preview of the curve shape (rate vs. tenor) implied by the current process
 * parameters and Curve Template rows -- no FX equivalent, since FX publishes one scalar spot
 * rather than a tenor grid. Calls preview_ir_curve_shape_request, debounced, whenever the editor's
 * Process or Curve Template tab changes.
 */
class CurveShapePreviewChart final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic.curve_shape_preview_chart";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    struct TemplateRow {
        int sequence_index = 0;
        std::string start_tenor_code;
        std::string end_tenor_code;
        std::string instrument_code;
    };

    explicit CurveShapePreviewChart(ClientManager* cm, QWidget* parent = nullptr);
    ~CurveShapePreviewChart() override = default;

    /** @brief Update the model inputs (does not refresh immediately). */
    void setParameters(const std::string& processType,
                       double kappa,
                       double theta,
                       double sigma,
                       double initialRate,
                       const std::string& fixedLegPaymentFrequencyCode,
                       const std::vector<TemplateRow>& entries);

    /** @brief Schedule a debounced refresh (~400 ms after the last call). */
    void scheduleRefresh();

private slots:
    void doRefresh();

private:
    void setBusy(bool busy);

    ClientManager* clientManager_;

    QChart* chart_;
    QChartView* view_;
    QValueAxis* axisX_;
    QValueAxis* axisY_;
    QLabel* statusLabel_;

    class QTimer* debounce_;

    std::string processType_{"vasicek"};
    double kappa_{0.0};
    double theta_{0.0};
    double sigma_{0.0};
    double initialRate_{0.0};
    std::string fixedLegPaymentFrequencyCode_{"Annual"};
    std::vector<TemplateRow> entries_;
    bool inFlight_{false};
    bool pending_{false};
};

}

#endif

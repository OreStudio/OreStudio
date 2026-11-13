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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_GJR_GARCH_VARIANCE_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_GJR_GARCH_VARIANCE_PROCESS_HPP

#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/gjr_garch.hpp"
#include <memory>

namespace ores::analytics::quant::service {

/// How a step's return is read off the price: the change of its log, or its plain change.
enum class return_measure { log_return, increment };

/// The day of a GJR-GARCH variance as a feed's ticks see it.
struct daily_variance_frame final {
    /// Nominal ticks in a market day.
    double ticks_per_day;
    /// The variance of the inner law over one day at a multiplier of one.
    double inner_daily_variance;
    /// The mean of the inner law over one day, which the multiplier does not move.
    double inner_daily_drift;
    return_measure measure;
};

/**
 * @brief A price process whose daily variance follows a GJR-GARCH(1,1).
 *
 * Wraps a process that honours set_volatility_multiplier(). Within a market day the
 * multiplier is the square root of the day's variance over the inner law's own daily
 * variance, so the day's shocks keep the inner law's shape and take the GARCH size.
 * At the end of each day the day's return, less the inner drift, is the shock that
 * updates the variance. A step that crosses the end of a day is split there, so a
 * day's variance applies to exactly that day's share of every step.
 *
 * The state is the variance, the ticks into the day and the price the day opened at.
 * The day's return is read once, at its end, so a step costs the inner step and a
 * compare, and nothing is allocated after construction.
 */
class ORES_ANALYTICS_QUANT_EXPORT gjr_garch_variance_process final
    : public ores::analytics::quant::domain::i_stochastic_process {
public:
    /**
     * @throws std::invalid_argument for a null inner process, one that cannot scale its
     * volatility, a frame that is not positive and finite, or an initial variance that
     * is not.
     */
    gjr_garch_variance_process(std::unique_ptr<domain::i_stochastic_process> inner,
                               math::gjr_garch_model model,
                               daily_variance_frame frame,
                               double initial_daily_variance);

    double next() override;
    double next(double elapsed_ticks) override;
    double next_with_normal(double standard_normal) override;
    bool supports_external_normal() const override;
    bool supports_variable_step() const override;
    void set_tail_tilt(double tilt) override;
    double max_step() const override;
    double current() const override;

    /// The variance of the market day in progress.
    double daily_variance() const noexcept {
        return variance_;
    }
    const math::gjr_garch_model& model() const noexcept {
        return model_;
    }

private:
    void roll();

    std::unique_ptr<domain::i_stochastic_process> inner_;
    math::gjr_garch_model model_;
    daily_variance_frame frame_;
    double variance_;
    double ticks_into_day_ = 0.0;
    double day_open_ = 0.0;
};

}

#endif

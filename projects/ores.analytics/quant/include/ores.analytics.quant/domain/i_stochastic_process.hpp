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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_I_STOCHASTIC_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_I_STOCHASTIC_PROCESS_HPP

#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::domain {

/**
 * @brief Pure interface for all stochastic and deterministic price processes.
 *
 * Lives in ores.analytics.quant (not ores.synthetic) so that any consumer —
 * a calibration service, ores.marketdata, ores.synthetic — can instantiate
 * processes without depending on a heavier service binary.
 *
 * Each call to next() advances the process by one step and returns the
 * new price as a double. The process owns its state (current price, RNG
 * seed, parameter values) and is not thread-safe.
 */
class i_stochastic_process {
public:
    virtual ~i_stochastic_process() = default;

    /**
     * @brief Advance the process by one step and return the new price.
     */
    virtual double next() = 0;

    /**
     * @brief Advance the process by @p elapsed_ticks nominal ticks.
     *
     * A tick is the arrival process's unit, so a step of two ticks is a step of
     * twice the time. A process whose law is written for one tick cannot answer
     * for a longer step, and this default says so rather than pretending: one
     * tick is next(), anything else is refused. The three processes that do
     * have a time-scaled law override both this and supports_variable_step().
     */
    virtual double next(double elapsed_ticks) {
        // One tick arrives from a division on one side and a multiplication on
        // the other, so the two spellings differ in the last bit for about a
        // quarter of all rates. Anything within this tolerance is one tick.
        constexpr double one_tick_tolerance = 1e-9;
        if (std::abs(elapsed_ticks - 1.0) <= one_tick_tolerance)
            return next();
        throw std::invalid_argument("this process cannot step by other than one nominal tick");
    }

    /**
     * @brief Whether next(elapsed) accepts an elapsed other than one.
     *
     * The feed refuses a non-periodic arrival mode for a process that answers
     * false: irregular gaps would ask it for steps it cannot take.
     */
    virtual bool supports_variable_step() const {
        return false;
    }

    /**
     * @brief Tilt the law's shape toward its tails from the next step on, keeping its mean
     * and variance.
     *
     * The market's activity reaches a price's shape through this: a positive tilt
     * makes large moves likelier and small ones likelier too, a negative one the
     * reverse. Zero is the law as configured. A process with no shape to tilt
     * ignores it.
     */
    virtual void set_tail_tilt(double /*tilt*/) {}

    /**
     * @brief The largest step, in the law's own time unit, this process can be
     * stepped by without its scheme's error dominating.
     *
     * A process that implements the exact transition of its law — an affine or
     * Ornstein–Uhlenbeck family, or a mixture whose increment is exactly normal —
     * is the law for any step, and answers infinity. A process stepped by an Euler
     * scheme or a tree is an approximation, and its error grows with the step, so
     * it states the bound its scheme can carry and a caller refuses a step above
     * it rather than producing a path nobody calibrated.
     */
    virtual double max_step() const {
        return std::numeric_limits<double>::infinity();
    }

    /**
     * @brief Return the current price without advancing.
     */
    virtual double current() const = 0;
};

} // namespace ores::analytics::quant::domain

#endif

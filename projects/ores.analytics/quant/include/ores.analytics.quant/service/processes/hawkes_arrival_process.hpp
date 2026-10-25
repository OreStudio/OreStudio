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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_HAWKES_ARRIVAL_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_HAWKES_ARRIVAL_PROCESS_HPP

#include "ores.analytics.quant/domain/i_arrival_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief A self-exciting point process with an exponential kernel.
 *
 * An arrival raises the rate, and the raise fades at a decay. With the
 * exponential kernel this is a Markov process in one number, the intensity:
 * the excess over the baseline decays between events and the jump is added at
 * each one.
 *
 * The parameters are the observable triple: the long-run mean rate in arrivals
 * per second, the branching ratio, and the decay in units of one over a second.
 * The baseline and the jump follow from them:
 *
 *     mu    = mean_rate * (1 - branching_ratio)
 *     alpha = branching_ratio * decay
 *
 * A branching ratio of one or more has no stationary rate, so the constructor
 * refuses it. The value is the expected number of further arrivals one arrival
 * causes; the long-run mean rate is mu / (1 - n).
 *
 * The gaps are exact for this kernel: direct inversion draws two uniforms per
 * event and never rejects, because the next event is the earlier of a baseline
 * arrival and one from the fading excess. The thinning method in the Hawkes
 * note gives the same law and extends to other kernels.
 *
 * Inspired by the tick library (BSD 3-clause, the tick developers, CMAP); see
 * =doc/knowledge/external/tick.org=, and the Hawkes note for the law and the two
 * simulation methods.
 */
class ORES_ANALYTICS_QUANT_EXPORT hawkes_arrival_process final
    : public ores::analytics::quant::domain::i_arrival_process {
public:
    /**
     * @param mean_rate Long-run mean rate, arrivals per second. Finite and
     *        strictly positive.
     * @param branching_ratio Expected further arrivals per arrival, at least
     *        zero and strictly below one.
     * @param decay How fast a burst fades, in units of one over a second.
     *        Finite and strictly positive.
     * @param seed Seed of the random engine, so a run is reproducible.
     */
    hawkes_arrival_process(double mean_rate,
                           double branching_ratio,
                           double decay,
                           std::uint32_t seed = 42);

    double next_gap() override;

    /// @brief The baseline rate mu, in arrivals per second.
    [[nodiscard]] double baseline() const {
        return baseline_;
    }
    /// @brief The jump alpha added to the intensity at each arrival.
    [[nodiscard]] double jump() const {
        return jump_;
    }

private:
    double mean_rate_;
    double branching_ratio_;
    double decay_;
    double baseline_;
    double jump_;
    /// @brief The intensity minus the baseline, just after the last arrival.
    double excess_ = 0.0;
    std::uniform_real_distribution<double> uniform_;
    std::mt19937 rng_;
};

}

#endif

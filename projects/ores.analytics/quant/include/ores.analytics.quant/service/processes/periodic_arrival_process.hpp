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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_PERIODIC_ARRIVAL_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_PERIODIC_ARRIVAL_PROCESS_HPP

#include "ores.analytics.quant/domain/i_arrival_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief A point process whose gaps are all the same length.
 *
 * The deterministic mode, and the default for a feed that wants no randomness.
 * The parameter is the long-run mean rate, in arrivals per second, so that all
 * three arrival processes take the same argument; the gap is its reciprocal.
 *
 * The first gap is drawn uniformly from the mean gap, so two feeds of the same
 * rate do not tick in lockstep: without it every periodic feed of a run fires on
 * the same step of the shared clock, which no market does and which biases any
 * statistic that looks across series at one instant. After that gap the process
 * is exactly periodic, so the rate is the configured one from the first arrival
 * on; a feed starts somewhere inside its first gap, as a process already running
 * would, rather than always at the start of one.
 *
 * Inspired by the tick library (BSD 3-clause, the tick developers, CMAP); see
 * =doc/knowledge/external/tick.org=.
 */
class ORES_ANALYTICS_QUANT_EXPORT periodic_arrival_process final
    : public ores::analytics::quant::domain::i_arrival_process {
public:
    /**
     * @param mean_rate Arrivals per second. Must be finite and strictly positive.
     * @param seed the draw of the first gap's phase, so a run reproduces.
     */
    explicit periodic_arrival_process(double mean_rate, std::uint32_t seed = 0);

    double next_gap() override;
    void set_baseline_multiplier(double multiplier) override;

private:
    double mean_rate_;
    double multiplier_ = 1.0;
    double gap_;
    /// How far into its first gap the process starts, as a fraction of that gap.
    double phase_;
    bool first_ = true;
};

}

#endif

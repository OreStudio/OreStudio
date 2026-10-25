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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_POISSON_ARRIVAL_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_POISSON_ARRIVAL_PROCESS_HPP

#include "ores.analytics.quant/domain/i_arrival_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief A Poisson point process: exponential gaps at a constant rate.
 *
 * The gaps are independent and the count in a window is Poisson. The
 * parameter is the long-run mean rate, in arrivals per second, the same
 * argument the other two arrival processes take.
 *
 * Inspired by the tick library (BSD 3-clause, the tick developers, CMAP); see
 * =doc/knowledge/external/tick.org=.
 */
class ORES_ANALYTICS_QUANT_EXPORT poisson_arrival_process final
    : public ores::analytics::quant::domain::i_arrival_process {
public:
    /**
     * @param mean_rate Arrivals per second. Must be finite and strictly positive.
     * @param seed Seed of the random engine, so a run is reproducible.
     */
    explicit poisson_arrival_process(double mean_rate, std::uint32_t seed = 42);

    double next_gap() override;
    void set_baseline_multiplier(double multiplier) override;

private:
    double mean_rate_;
    std::exponential_distribution<double> gap_distribution_;
    std::mt19937 rng_;
};

}

#endif

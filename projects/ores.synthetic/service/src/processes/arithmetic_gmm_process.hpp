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
#ifndef ORES_SYNTHETIC_SERVICE_PROCESSES_ARITHMETIC_GMM_PROCESS_HPP
#define ORES_SYNTHETIC_SERVICE_PROCESSES_ARITHMETIC_GMM_PROCESS_HPP

#include "ores.marketdata.api/domain/i_stochastic_process.hpp"
#include <cstdint>
#include <random>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Arithmetic (additive) Gaussian Mixture Model price process.
 *
 * The counterpart to gmm_process: on each call to next() it draws an increment
 * from a K-component Gaussian mixture and applies it ADDITIVELY:
 *   price += increment
 *
 * This is arithmetic Brownian motion (Bachelier) when single-component. Unlike
 * the geometric engine the price may cross zero. Exists alongside gmm_process to
 * exercise the IStochasticProcess abstraction (we are not hardcoded to the
 * geometric case) and to let the UI contrast engines.
 */
class arithmetic_gmm_process final : public ores::marketdata::domain::IStochasticProcess {
public:
    arithmetic_gmm_process(std::vector<double> means,
                           std::vector<double> stdevs,
                           std::vector<double> weights,
                           double initial_price,
                           std::uint32_t seed = 42);

    double next() override;
    double current() const override;

private:
    std::vector<double> means_;
    std::vector<double> stdevs_;
    std::vector<double> weights_;
    double price_;
    std::mt19937 rng_;
    std::discrete_distribution<int> component_dist_;
};

}

#endif

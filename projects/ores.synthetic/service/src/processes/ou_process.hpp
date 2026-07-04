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
#ifndef ORES_SYNTHETIC_SERVICE_PROCESSES_OU_PROCESS_HPP
#define ORES_SYNTHETIC_SERVICE_PROCESSES_OU_PROCESS_HPP

#include "ores.marketdata.api/domain/i_stochastic_process.hpp"
#include <cstdint>
#include <random>

namespace ores::synthetic::service {

/**
 * @brief Ornstein-Uhlenbeck (mean-reverting) price process.
 *
 * dX = kappa * (theta - X) * dt + sigma * dW
 *
 * Advances using the exact per-tick discretisation (dt = 1 tick, matching the
 * per-update convention the GMM engines use for their increments):
 *   X_{t+1} = theta + (X_t - theta) * e^{-kappa} +
 *             sigma * sqrt((1 - e^{-2*kappa}) / (2*kappa)) * Z,  Z ~ N(0, 1)
 *
 * kappa <= 0 degenerates to a driftless random walk (sigma * Z per tick),
 * the kappa -> 0 limit of the variance term above.
 */
class ou_process final : public ores::marketdata::domain::IStochasticProcess {
public:
    ou_process(
        double kappa, double theta, double sigma, double initial_price, std::uint32_t seed = 42);

    double next() override;
    double current() const override;

private:
    double kappa_;
    double theta_;
    double sigma_;
    double price_;
    std::mt19937 rng_;
    std::normal_distribution<double> normal_;
};

}

#endif

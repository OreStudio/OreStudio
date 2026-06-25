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
#ifndef ORES_MARKETDATA_API_DOMAIN_I_STOCHASTIC_PROCESS_HPP
#define ORES_MARKETDATA_API_DOMAIN_I_STOCHASTIC_PROCESS_HPP

namespace ores::marketdata::domain {

/**
 * @brief Pure interface for all stochastic and deterministic price processes.
 *
 * Lives in ores.marketdata.api (not ores.synthetic) so that a future
 * calibration service can instantiate processes without depending on the
 * synthetic service binary.
 *
 * Each call to next() advances the process by one step and returns the
 * new price as a double. The process owns its state (current price, RNG
 * seed, parameter values) and is not thread-safe.
 */
class IStochasticProcess {
public:
    virtual ~IStochasticProcess() = default;

    /**
     * @brief Advance the process by one step and return the new price.
     */
    virtual double next() = 0;

    /**
     * @brief Return the current price without advancing.
     */
    virtual double current() const = 0;
};

}

#endif

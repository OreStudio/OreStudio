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
#ifndef ORES_SYNTHETIC_SERVICE_PROCESS_FACTORY_HPP
#define ORES_SYNTHETIC_SERVICE_PROCESS_FACTORY_HPP

#include "ores.marketdata.api/domain/i_stochastic_process.hpp"
#include <cstdint>
#include <memory>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Factory for IStochasticProcess implementations.
 *
 * Kept minimal at the scaffold stage; will grow as more process types
 * are added in later implementation steps.
 */
class process_factory {
public:
    static std::unique_ptr<ores::marketdata::domain::IStochasticProcess>
    make_gmm_process(std::vector<double> means,
                     std::vector<double> stdevs,
                     std::vector<double> weights,
                     double initial_price,
                     std::uint32_t seed = 42);
};

}

#endif

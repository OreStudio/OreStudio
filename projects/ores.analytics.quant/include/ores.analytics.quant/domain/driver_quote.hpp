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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_DRIVER_QUOTE_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_DRIVER_QUOTE_HPP

#include <chrono>
#include <string>

namespace ores::analytics::quant::domain {

/**
 * @brief One tick on a driver edge already present in a built @c crm_topology.
 *
 * Raw currency codes, like @c ccy_pair_input -- the caller adapts its own
 * quote representation at this boundary. @c rate_engine::update rejects a
 * pair that is not an edge of its topology.
 */
struct driver_quote {
    std::string base_code;
    std::string quote_code;
    /// Spot rate: 1 unit of @c base_code = @c rate units of @c quote_code.
    double rate;
    std::chrono::system_clock::time_point observed_at;
};

} // namespace ores::analytics::quant::domain

#endif

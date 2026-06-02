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
#include "ores.marketdata.api/generators/market_series_generator.hpp"

#include <atomic>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::marketdata::generator {

using ores::utility::generation::generation_keys;

domain::market_series generate_synthetic_market_series(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};

    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    domain::market_series r;
    r.id = ctx.generate_uuid();
    r.version = 0;
    r.series_type = "DISCOUNT";
    r.metric = "RATE";
    r.qualifier = "TST_" + ctx.alphanumeric(3) + "_" + std::to_string(++counter);
    r.asset_class = domain::asset_class::rates;
    r.subclass = domain::series_subclass::yield;
    r.is_scalar = false;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::market_series>
generate_synthetic_market_series(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::market_series> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_market_series(ctx));
    return r;
}

}

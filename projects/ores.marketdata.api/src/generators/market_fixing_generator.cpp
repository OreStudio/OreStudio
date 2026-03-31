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
#include "ores.marketdata.api/generators/market_fixing_generator.hpp"

#include <atomic>
#include <format>

namespace ores::marketdata::generator {

domain::market_fixing generate_synthetic_market_fixing(
    const boost::uuids::uuid& series_id,
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const int n = ++counter;

    auto tp = ctx.past_timepoint();
    auto dp = std::chrono::floor<std::chrono::days>(tp);

    domain::market_fixing r;
    r.id = ctx.generate_uuid();
    r.series_id = series_id;
    r.fixing_date = std::chrono::year_month_day{dp};
    r.value = std::format("{:.6f}", 0.03 + 0.001 * n);
    r.source = std::nullopt;
    r.recorded_at = tp;
    return r;
}

std::vector<domain::market_fixing>
generate_synthetic_market_fixings(std::size_t n,
    const boost::uuids::uuid& series_id,
    utility::generation::generation_context& ctx) {
    std::vector<domain::market_fixing> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_market_fixing(series_id, ctx));
    return r;
}

}

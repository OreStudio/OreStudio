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
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>

namespace ores::marketdata::generators {

using ores::utility::generation::generation_keys;

domain::market_fixing
generate_synthetic_market_fixing(utility::generation::generation_context& ctx) {
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::market_fixing r;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.party_id = ctx.generate_uuid();
    r.series_id = ctx.generate_uuid();
    r.fixing_date =
        std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(ctx.past_timepoint())};
    r.value = std::to_string(faker::number::decimal<double>(0.0, 100.0));
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::market_fixing>
generate_synthetic_market_fixings(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::market_fixing> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_market_fixing(ctx));
    return r;
}

}

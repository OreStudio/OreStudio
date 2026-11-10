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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_generator.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.api/generators/fx_forward_publish_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::marketdata::generators {

using ores::utility::generation::generation_keys;

domain::fx_forward_publish
generate_synthetic_fx_forward_publish(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::fx_forward_publish r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.request_id = ctx.generate_uuid();
    r.party_id = ctx.generate_uuid();
    r.pair_code = std::string("EUR/USD");
    r.as_of = ctx.past_timepoint();
    r.spot_date = std::chrono::year_month_day{
        std::chrono::year{2026}, std::chrono::month{11}, std::chrono::day{11}};
    r.spot_rate = 1.1;
    r.base_curve_publish_id = ctx.generate_uuid();
    r.quote_curve_publish_id = ctx.generate_uuid();
    r.convention_version = 1;
    r.calendar_codes = std::string("TARGET,US");
    r.method_code = std::string("covered_interest_parity");
    r.published_by = std::string("");
    r.published_at = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::fx_forward_publish>
generate_synthetic_fx_forward_publishes(std::size_t n,
                                        utility::generation::generation_context& ctx) {
    std::vector<domain::fx_forward_publish> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_fx_forward_publish(ctx));
    return r;
}

}

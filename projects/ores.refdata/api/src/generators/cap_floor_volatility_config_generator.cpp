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
#include "ores.refdata.api/generators/cap_floor_volatility_config_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::cap_floor_volatility_config
generate_synthetic_cap_floor_volatility_config(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::cap_floor_volatility_config r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.curve_definition_id = ctx.generate_uuid();
    r.volatility_type = std::nullopt;
    r.output_volatility_type = std::nullopt;
    r.model_shift = std::nullopt;
    r.output_shift = std::nullopt;
    r.extrapolation = std::nullopt;
    r.interpolation_method = std::nullopt;
    r.include_atm = std::nullopt;
    r.day_counter = std::nullopt;
    r.calendar = std::nullopt;
    r.business_day_convention = std::nullopt;
    r.tenors = std::nullopt;
    r.strikes = std::nullopt;
    r.optional_quotes = std::nullopt;
    r.ibor_index = std::nullopt;
    r.index = std::nullopt;
    r.rate_computation_period = std::nullopt;
    r.on_cap_settlement_days = std::nullopt;
    r.discount_curve = std::nullopt;
    r.atm_tenors = std::nullopt;
    r.settlement_days = std::nullopt;
    r.interpolate_on = std::nullopt;
    r.time_interpolation = std::nullopt;
    r.strike_interpolation = std::nullopt;
    r.input_type = std::nullopt;
    r.quote_includes_index_name = std::nullopt;
    r.flat_first_period = std::nullopt;
    r.use_effecive_volatility = std::nullopt;
    r.use_effective_volatility = std::nullopt;
    r.has_proxy_config = false;
    r.proxy_source_curve_id = std::nullopt;
    r.proxy_source_index = std::nullopt;
    r.proxy_source_rate_computation_period = std::nullopt;
    r.proxy_target_index = std::nullopt;
    r.proxy_target_rate_computation_period = std::nullopt;
    r.proxy_target_on_cap_settlement_days = std::nullopt;
    r.proxy_scaling_factor = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::cap_floor_volatility_config>
generate_synthetic_cap_floor_volatility_configs(std::size_t n,
                                                utility::generation::generation_context& ctx) {
    std::vector<domain::cap_floor_volatility_config> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_cap_floor_volatility_config(ctx));
    return r;
}

}

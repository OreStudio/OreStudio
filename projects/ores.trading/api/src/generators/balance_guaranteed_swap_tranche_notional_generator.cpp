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
#include "ores.trading.api/generators/balance_guaranteed_swap_tranche_notional_generator.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::trading::generators {

using ores::utility::generation::generation_keys;

domain::balance_guaranteed_swap_tranche_notional
generate_synthetic_balance_guaranteed_swap_tranche_notional(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::balance_guaranteed_swap_tranche_notional r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.trade_id = ctx.generate_uuid();
    r.tranche_number = faker::number::integer(1, 2);
    r.sequence_number = 0;
    r.trade_activity_id = ctx.generate_uuid();
    r.start_date =
        std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(ctx.past_timepoint())};
    r.notional = ores::utility::decimal::decimal::from_string("1000000").value();
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::balance_guaranteed_swap_tranche_notional>
generate_synthetic_balance_guaranteed_swap_tranche_notionals(
    std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::balance_guaranteed_swap_tranche_notional> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_balance_guaranteed_swap_tranche_notional(ctx));
    return r;
}

}

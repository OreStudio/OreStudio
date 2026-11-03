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
#include "ores.synthetic.api/generators/variance_calibration_input_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::synthetic::generators {

using ores::utility::generation::generation_keys;

domain::variance_calibration_input
generate_synthetic_variance_calibration_input(utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::variance_calibration_input r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.party_id = ctx.generate_uuid();
    r.base_currency_code = std::string("EUR") + "-" + std::to_string(idx);
    r.quote_currency_code = std::string("USD") + "-" + std::to_string(idx);
    r.fixing_uri = std::string("oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD") + "-" +
                   std::to_string(idx);
    r.window_end =
        std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(ctx.past_timepoint())};
    r.window_days = 1826;
    r.provider_code = std::string("ecb");
    r.inverted = faker::datatype::boolean();
    r.policy_id = ctx.generate_uuid();
    r.policy_version = 1;
    r.point_count = faker::number::integer(100, 1300);
    r.used_count = faker::number::integer(0, 100);
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::variance_calibration_input>
generate_synthetic_variance_calibration_inputs(std::size_t n,
                                               utility::generation::generation_context& ctx) {
    std::vector<domain::variance_calibration_input> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_variance_calibration_input(ctx));
    return r;
}

}

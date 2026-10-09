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
#include "ores.refdata.api/generators/cross_currency_basis_convention_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::cross_currency_basis_convention
generate_synthetic_cross_currency_basis_convention(utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::cross_currency_basis_convention r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.id = std::string("EUR-USD-XCCY-BASIS") + "-" + std::to_string(idx);
    r.party_id = ctx.generate_uuid();
    r.settlement_days = 2;
    r.settlement_calendar = std::string("TARGET");
    r.roll_convention = std::string("ModifiedFollowing");
    r.flat_index = std::string("EUR-EURIBOR-6M");
    r.spread_index = std::string("USD-LIBOR-3M");
    r.eom = std::nullopt;
    r.is_resettable = std::nullopt;
    r.flat_index_is_resettable = std::nullopt;
    r.flat_tenor = std::string("6M");
    r.spread_tenor = std::string("3M");
    r.spread_payment_lag = std::nullopt;
    r.flat_payment_lag = std::nullopt;
    r.spread_include_spread = std::nullopt;
    r.spread_lookback = std::string("0D");
    r.spread_fixing_days = std::nullopt;
    r.spread_rate_cutoff = std::nullopt;
    r.spread_is_averaged = std::nullopt;
    r.spread_observation_shift = std::nullopt;
    r.flat_include_spread = std::nullopt;
    r.flat_lookback = std::string("0D");
    r.flat_fixing_days = std::nullopt;
    r.flat_rate_cutoff = std::nullopt;
    r.flat_is_averaged = std::nullopt;
    r.flat_observation_shift = std::nullopt;
    r.oresmd_uri = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::cross_currency_basis_convention>
generate_synthetic_cross_currency_basis_conventions(std::size_t n,
                                                    utility::generation::generation_context& ctx) {
    std::vector<domain::cross_currency_basis_convention> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_cross_currency_basis_convention(ctx));
    return r;
}

}

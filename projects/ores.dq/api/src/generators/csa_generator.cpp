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
#include "ores.dq.api/generators/csa_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::dq::generators {

using ores::utility::generation::generation_keys;

domain::csa generate_synthetic_csa(utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::csa r;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.netting_set_code = std::string("NS-0001") + "-" + std::to_string(idx);
    r.is_active = true;
    r.bilateral = std::string("Bilateral");
    r.csa_currency = std::string("EUR");
    r.index_name = std::string("EUR-ESTER");
    r.threshold_pay = std::nullopt;
    r.threshold_receive = std::nullopt;
    r.minimum_transfer_amount_pay = std::nullopt;
    r.minimum_transfer_amount_receive = std::nullopt;
    r.independent_amount_held = std::nullopt;
    r.independent_amount_type = std::nullopt;
    r.call_frequency = std::string("1D");
    r.post_frequency = std::string("1D");
    r.margin_period_of_risk = std::string("2W");
    r.collateral_compounding_spread_receive = std::nullopt;
    r.collateral_compounding_spread_pay = std::nullopt;
    r.apply_initial_margin = std::nullopt;
    r.initial_margin_type = std::nullopt;
    r.calculate_im_amount = std::nullopt;
    r.calculate_vm_amount = std::nullopt;
    r.non_exempt_im_regulations = std::nullopt;
    r.eligible_currencies = std::string("EUR,USD");
    return r;
}

std::vector<domain::csa> generate_synthetic_csas(std::size_t n,
                                                 utility::generation::generation_context& ctx) {
    std::vector<domain::csa> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_csa(ctx));
    return r;
}

}

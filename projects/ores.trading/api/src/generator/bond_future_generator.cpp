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
#include "ores.trading.api/generator/bond_future_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::trading::generator {

using ores::utility::generation::generation_keys;

domain::bond_future generate_synthetic_bond_future(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::bond_future r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.instrument_id = ctx.generate_uuid();
    r.contract_name = std::string("US 10YR T-NOTE");
    r.contract_notional = 100000.0;
    r.long_short = std::string("Long");
    r.currency = std::string("USD");
    r.contract_month = std::string("2029-09");
    r.deliverable_grade = std::string("T 2.75 11/15/2042");
    r.fair_price = 98.5;
    r.settlement = std::string("Cash");
    r.settlement_dirty = false;
    r.root_date = std::string("2029-03-01");
    r.expiry_basis = std::string("Futures");
    r.settlement_basis = std::string("Futures");
    r.expiry_lag = 1;
    r.settlement_lag = 2;
    r.last_trading_date = std::string("2029-09-27");
    r.last_delivery_date = std::string("2029-09-28");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::bond_future>
generate_synthetic_bond_futures(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::bond_future> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_bond_future(ctx));
    return r;
}

}

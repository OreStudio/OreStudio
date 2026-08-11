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
#include "ores.trading.api/generators/fx_barrier_option_instrument_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::trading::generators {

using ores::utility::generation::generation_keys;

domain::fx_barrier_option_instrument
generate_synthetic_fx_barrier_option_instrument(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::fx_barrier_option_instrument r;
    r.identity.version = 0;
    r.identity.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.identity.workspace_id = utility::uuid::live_workspace_id();
    r.identity.instrument_id = ctx.generate_uuid();
    r.identity.trade_type_code = std::string("FxBarrierOption");
    r.identity.party_id = ctx.generate_uuid();
    r.bought_currency = std::string("EUR");
    r.bought_amount = 1000000.0;
    r.sold_currency = std::string("USD");
    r.sold_amount = 1100000.0;
    r.option_type = std::string("Call");
    r.expiry_date = std::string("2033-02-20");
    r.barrier_type = std::string("UpAndIn");
    r.lower_barrier = 1.2;
    r.audit.modified_by = modified_by;
    r.audit.performed_by = modified_by;
    r.audit.change_reason_code = "system.test";
    r.audit.change_commentary = "Synthetic test data";
    r.audit.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::fx_barrier_option_instrument>
generate_synthetic_fx_barrier_option_instruments(std::size_t n,
                                                 utility::generation::generation_context& ctx) {
    std::vector<domain::fx_barrier_option_instrument> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_fx_barrier_option_instrument(ctx));
    return r;
}

}

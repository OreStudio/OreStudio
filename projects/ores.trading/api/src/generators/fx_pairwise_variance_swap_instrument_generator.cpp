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
#include "ores.trading.api/generators/fx_pairwise_variance_swap_instrument_generator.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::trading::generators {

using ores::utility::generation::generation_keys;

domain::fx_pairwise_variance_swap_instrument
generate_synthetic_fx_pairwise_variance_swap_instrument(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::fx_pairwise_variance_swap_instrument r;
    r.identity.version = 0;
    r.identity.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.identity.trade_id = ctx.generate_uuid();
    r.identity.trade_type_code = std::string("FxPairwiseVarianceSwap");
    r.identity.party_id = ctx.generate_uuid();
    r.identity.trade_activity_id = ctx.generate_uuid();
    r.first_underlying = std::string("FX-ECB-EUR-USD");
    r.second_underlying = std::string("FX-ECB-GBP-USD");
    r.first_strike = ores::utility::decimal::decimal::from_string("0.2").value();
    r.second_strike = ores::utility::decimal::decimal::from_string("0.25").value();
    r.first_notional = ores::utility::decimal::decimal::from_string("1000").value();
    r.second_notional = ores::utility::decimal::decimal::from_string("1000").value();
    r.basket_notional = ores::utility::decimal::decimal::from_string("1000").value();
    r.basket_strike = ores::utility::decimal::decimal::from_string("0.22").value();
    r.currency = std::string("USD");
    r.settlement_date = std::chrono::year_month_day{std::chrono::year{2027} / 1 / 20};
    r.long_short = std::string("Long");
    r.payoff_limit = ores::utility::decimal::decimal::from_string(
                         std::to_string(faker::number::decimal<double>(1.0, 1000.0)))
                         .value();
    r.cap = ores::utility::decimal::decimal::from_string(
                std::to_string(faker::number::decimal<double>(1.0, 1000.0)))
                .value();
    r.floor = ores::utility::decimal::decimal::from_string(
                  std::to_string(faker::number::decimal<double>(1.0, 1000.0)))
                  .value();
    r.audit.modified_by = modified_by;
    r.audit.performed_by = modified_by;
    r.audit.change_reason_code = "system.test";
    r.audit.change_commentary = "Synthetic test data";
    r.audit.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::fx_pairwise_variance_swap_instrument>
generate_synthetic_fx_pairwise_variance_swap_instruments(
    std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::fx_pairwise_variance_swap_instrument> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_fx_pairwise_variance_swap_instrument(ctx));
    return r;
}

}

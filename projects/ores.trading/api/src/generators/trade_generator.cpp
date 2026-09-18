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
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::trading::generators {

using ores::utility::generation::generation_keys;

domain::trade generate_synthetic_trade(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::trade r;
    r.identity.version = 0;
    r.identity.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.identity.workspace_id = utility::uuid::live_workspace_id();
    r.identity.id = ctx.generate_uuid();
    r.identity.party_id = ctx.generate_uuid();
    r.identity.external_id = std::string();
    r.parties.book_id = ctx.generate_uuid();
    r.parties.portfolio_id = ctx.generate_uuid();
    r.parties.successor_trade_id = std::nullopt;
    r.classification.trade_type = std::string("Swap");
    r.parties.counterparty_id = std::nullopt;
    r.classification.product_type = domain::product_type::swap;
    r.classification.instrument_id = std::nullopt;
    r.classification.asset_class = std::nullopt;
    r.classification.netting_set_id = std::string("NS-001");
    r.classification.activity_type_code = std::string("New");
    r.classification.status_id = ctx.generate_uuid();
    r.lifecycle.trade_date = std::string("2025-01-15");
    r.lifecycle.execution_timestamp = std::string("2025-01-15 10:00:00");
    r.lifecycle.effective_date = std::string("2025-01-16");
    r.lifecycle.termination_date = std::string("2026-01-15");
    r.audit.modified_by = modified_by;
    r.audit.performed_by = modified_by;
    r.audit.change_reason_code = "system.test";
    r.audit.change_commentary = "Synthetic test data";
    r.audit.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::trade> generate_synthetic_trades(std::size_t n,
                                                     utility::generation::generation_context& ctx) {
    std::vector<domain::trade> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_trade(ctx));
    return r;
}

}

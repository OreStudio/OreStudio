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
#include "ores.trading.api/generator/bond_issue_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::trading::generator {

using ores::utility::generation::generation_keys;

domain::bond_issue generate_synthetic_bond_issue(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::bond_issue r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.workspace_id = utility::uuid::live_workspace_id();
    r.issue_id = ctx.generate_uuid();
    r.security_id = std::string("US9128283M42");
    r.issuer = std::string("US Treasury");
    r.currency = std::string("USD");
    r.face_value = 1000.0;
    r.coupon_rate = 0.0425;
    r.coupon_frequency_code = std::string("SemiAnnual");
    r.day_count_code = std::string("30/360");
    r.issue_date = std::string("2024-01-15");
    r.maturity_date = std::string("2034-01-15");
    r.settlement_days = 2;
    r.description = std::string(faker::lorem::sentence());
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::bond_issue>
generate_synthetic_bond_issues(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::bond_issue> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_bond_issue(ctx));
    return r;
}

}

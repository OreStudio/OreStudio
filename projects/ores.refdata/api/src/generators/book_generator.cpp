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
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::book generate_synthetic_book(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::book r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.workspace_id = utility::uuid::live_workspace_id();
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.party_id = ctx.generate_uuid();
    r.name = std::string("BOOK_") + std::to_string(faker::number::integer(1, 999)) + "-" +
             std::to_string(idx);
    r.description = std::string(faker::lorem::sentence());
    r.parent_portfolio_id = ctx.generate_uuid();
    r.owner_unit_id = std::nullopt;
    r.ledger_ccy = std::string("USD");
    r.gl_account_ref = std::string("GL-10150-TEST");
    r.cost_center = std::string("CC-001");
    r.book_status = std::string("Active");
    r.regulatory_book_type = std::string("Trading");
    r.is_sweepable = false;
    r.rates_centre_code = std::string("GBLO");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::book> generate_synthetic_books(std::size_t n,
                                                   utility::generation::generation_context& ctx) {
    std::vector<domain::book> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_book(ctx));
    return r;
}

}

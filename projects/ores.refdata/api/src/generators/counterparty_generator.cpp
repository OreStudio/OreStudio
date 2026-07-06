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
#include "ores.refdata.api/generators/counterparty_generator.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;
namespace change_reason_codes = ores::dq::domain::change_reason_constants::codes;

domain::counterparty generate_synthetic_counterparty(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::counterparty r;
    r.version = 1;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.short_code = std::string(faker::string::alpha(6)) + "-" + std::to_string(idx);
    r.full_name = std::string(faker::company::companyName());
    r.transliterated_name = std::nullopt;
    r.party_type = std::string("Bank");
    r.parent_counterparty_id = std::nullopt;
    r.business_center_code = std::string("WRLD");
    r.status = std::string("Active");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = change_reason_codes::synthetic_new;
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::counterparty>
generate_synthetic_counterparties(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::counterparty> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_counterparty(ctx));
    return r;
}

}

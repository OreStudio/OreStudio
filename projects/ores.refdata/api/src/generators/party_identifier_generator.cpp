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
#include "ores.refdata.api/generators/party_identifier_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <array>
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <string_view>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::party_identifier
generate_synthetic_party_identifier(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::party_identifier r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.party_id = ctx.generate_uuid();
    // Rotate through valid schemes: a party may have at most one identifier
    // per scheme (see ores_refdata_party_identifiers_insert_fn), so callers
    // that generate several identifiers for the same party need distinct
    // schemes, not just distinct ids.
    static constexpr std::array<std::string_view, 10> valid_schemes{"LEI",
                                                                     "BIC",
                                                                     "MIC",
                                                                     "NATIONAL_ID",
                                                                     "CEDB",
                                                                     "NATURAL_PERSON",
                                                                     "ACER",
                                                                     "DTCC_PARTICIPANT_ID",
                                                                     "MPID",
                                                                     "INTERNAL"};
    r.id_scheme = std::string(valid_schemes[idx % valid_schemes.size()]);
    r.id_value = std::string(faker::string::alphanumeric(20)) + "-" + std::to_string(idx);
    r.description = std::string("Test identifier");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::party_identifier>
generate_synthetic_party_identifiers(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::party_identifier> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_party_identifier(ctx));
    return r;
}

}

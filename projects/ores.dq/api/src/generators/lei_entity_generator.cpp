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
#include "ores.dq.api/generators/lei_entity_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::dq::generators {

using ores::utility::generation::generation_keys;

domain::lei_entity generate_synthetic_lei_entity(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::lei_entity r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.lei = std::string(faker::string::alphanumeric(20)) + "-" + std::to_string(idx);
    r.entity_legal_name = std::string(faker::company::companyName());
    r.entity_entity_category = std::string(faker::word::noun());
    r.entity_entity_sub_category = std::string(faker::word::noun());
    r.entity_entity_status = std::string(faker::word::noun());
    r.entity_legal_form_entity_legal_form_code = std::string(faker::string::alphanumeric(4));
    r.entity_legal_form_other_legal_form = std::string(faker::word::noun());
    r.entity_legal_jurisdiction = std::string(faker::location::countryCode());
    r.entity_legal_address_first_address_line = std::string(faker::location::streetAddress());
    r.entity_legal_address_city = std::string(faker::location::city());
    r.entity_legal_address_region = std::string(faker::location::state());
    r.entity_legal_address_country = std::string(faker::location::countryCode());
    r.entity_legal_address_postal_code = std::string(faker::location::zipCode());
    r.entity_headquarters_address_first_address_line =
        std::string(faker::location::streetAddress());
    r.entity_headquarters_address_city = std::string(faker::location::city());
    r.entity_headquarters_address_region = std::string(faker::location::state());
    r.entity_headquarters_address_country = std::string(faker::location::countryCode());
    r.entity_headquarters_address_postal_code = std::string(faker::location::zipCode());
    r.entity_entity_creation_date = std::chrono::system_clock::now();
    r.registration_initial_registration_date = std::chrono::system_clock::now();
    r.registration_last_update_date = std::chrono::system_clock::now();
    r.registration_next_renewal_date = std::chrono::system_clock::now();
    r.registration_registration_status = std::string(faker::word::noun());
    r.entity_transliterated_name_1 = std::string(faker::company::companyName());
    r.entity_transliterated_name_1_type = std::string(faker::word::noun());
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::lei_entity>
generate_synthetic_lei_entities(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::lei_entity> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_lei_entity(ctx));
    return r;
}

}

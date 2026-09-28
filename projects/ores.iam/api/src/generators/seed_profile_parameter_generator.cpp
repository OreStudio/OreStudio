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
#include "ores.iam.api/generators/seed_profile_parameter_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::seed_profile_parameter
generate_synthetic_seed_profile_parameter(utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::seed_profile_parameter r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.seed_profile_id = ctx.generate_uuid();
    r.name = std::string(faker::word::noun()) + "-" + std::to_string(idx);
    r.label = faker::word::noun();
    r.data_type = std::string("string");
    r.choices_json = std::string("[]");
    r.default_value = std::string("50");
    r.is_required = faker::datatype::boolean();
    r.description = std::string(faker::lorem::sentence());
    r.display_order = faker::number::integer(0, 100);
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::seed_profile_parameter>
generate_synthetic_seed_profile_parameters(std::size_t n,
                                           utility::generation::generation_context& ctx) {
    std::vector<domain::seed_profile_parameter> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_seed_profile_parameter(ctx));
    return r;
}

}

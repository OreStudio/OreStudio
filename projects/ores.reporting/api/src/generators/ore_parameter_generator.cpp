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
#include "ores.reporting.api/generators/ore_parameter_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::reporting::generators {

using ores::utility::generation::generation_keys;

domain::ore_parameter
generate_synthetic_ore_parameter(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::ore_parameter r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.scope = std::string(faker::word::noun());
    r.subtype = std::string(faker::word::noun());
    r.name = std::string(faker::word::noun());
    r.schema_name = std::string(faker::word::noun());
    r.xpath = std::string(faker::word::noun());
    r.ore_parameter_value_domain_code = std::string(faker::word::noun());
    r.label = std::string("Parameter");
    r.help = std::string("A flag.");
    r.description = std::string("What the parameter decides.");
    r.default_text = std::string("No.");
    r.format = std::string("A flag.");
    r.introduced_in = std::string("1.8.13.0");
    r.removed_in = std::string("1.9.0.0");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::ore_parameter>
generate_synthetic_ore_parameters(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::ore_parameter> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_ore_parameter(ctx));
    return r;
}

}

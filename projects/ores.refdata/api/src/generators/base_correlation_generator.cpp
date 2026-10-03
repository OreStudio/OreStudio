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
#include "ores.refdata.api/generators/base_correlation_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::base_correlation
generate_synthetic_base_correlation(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::base_correlation r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.curve_definition_id = ctx.generate_uuid();
    r.terms = std::string("5Y");
    r.detachment_points = std::string("0.03");
    r.settlement_days = 0.0;
    r.calendar = std::string("TARGET");
    r.business_day_convention = std::string("Following");
    r.day_counter = std::string("A365");
    r.extrapolate = std::nullopt;
    r.quote_name = std::nullopt;
    r.start_date = std::nullopt;
    r.rule = std::nullopt;
    r.adjust_for_losses = std::nullopt;
    r.index_term = std::nullopt;
    r.index_spread = std::nullopt;
    r.currency = std::nullopt;
    r.calibrate_constituents_to_index_spread = std::nullopt;
    r.use_assumed_recovery = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::base_correlation>
generate_synthetic_base_correlations(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::base_correlation> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_base_correlation(ctx));
    return r;
}

}

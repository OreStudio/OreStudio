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
#include "ores.synthetic.api/generators/variance_calibration_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::synthetic::generators {

using ores::utility::generation::generation_keys;

domain::variance_calibration
generate_synthetic_variance_calibration(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::variance_calibration r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.party_id = ctx.generate_uuid();
    r.base_currency_code = std::string("EUR");
    r.quote_currency_code = std::string("USD");
    r.input_id = ctx.generate_uuid();
    r.method = std::string("gjr_garch_qmle");
    r.method_version = 1;
    r.alpha = std::optional<double>(0.05);
    r.gamma = std::optional<double>(0.02);
    r.beta = std::optional<double>(0.9);
    r.persistence = std::optional<double>(0.96);
    r.long_run_variance = std::optional<double>(0.00004);
    r.starting_variance_ratio = std::optional<double>(1.0);
    r.log_likelihood = std::nullopt;
    r.returns_used = faker::number::integer(0, 1300);
    r.starts = faker::number::integer(0, 8);
    r.starts_agreed = faker::datatype::boolean();
    r.state = std::string("proposed");
    r.state_reason = std::string("");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::variance_calibration>
generate_synthetic_variance_calibrations(std::size_t n,
                                         utility::generation::generation_context& ctx) {
    std::vector<domain::variance_calibration> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_variance_calibration(ctx));
    return r;
}

}

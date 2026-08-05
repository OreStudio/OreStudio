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
#include "ores.refdata.api/generators/ir_curve_bootstrap_config_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::ir_curve_bootstrap_config
generate_synthetic_ir_curve_bootstrap_config(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::ir_curve_bootstrap_config r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.output_series_id = ctx.generate_uuid();
    r.party_id = ctx.generate_uuid();
    r.source_series_id = ctx.generate_uuid();
    r.curve_family_role = std::string("FUNDING");
    r.discount_curve_config_id = boost::uuids::nil_uuid();
    r.interpolation_method = std::string("LOG_LINEAR_DISCOUNT");
    r.day_count_convention = std::string("ACT/365");
    r.split_tenor_code = std::string("1Y");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::ir_curve_bootstrap_config>
generate_synthetic_ir_curve_bootstrap_configs(std::size_t n,
                                              utility::generation::generation_context& ctx) {
    std::vector<domain::ir_curve_bootstrap_config> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_ir_curve_bootstrap_config(ctx));
    return r;
}

}

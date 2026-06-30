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
#include "ores.synthetic.api/generators/gmm_component_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>

namespace ores::synthetic::generators {

using ores::utility::generation::generation_keys;

domain::gmm_component
generate_synthetic_gmm_component(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::gmm_component r;
    r.version = 1;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.party_id = ctx.generate_uuid();
    r.fx_spot_config_id = ctx.generate_uuid();
    r.component_index = faker::number::integer(0, 4);
    r.description = std::string(faker::word::adjective());
    r.mean = faker::number::decimal(-0.001, 0.001);
    r.stdev = faker::number::decimal(0.0, 0.01);
    r.weight = faker::number::decimal(0.0, 1.0);
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::gmm_component>
generate_synthetic_gmm_components(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::gmm_component> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_gmm_component(ctx));
    return r;
}

}

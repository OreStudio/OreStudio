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
#include "ores.dq.api/generators/synthetic_fx_spot_config_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::dq::generators {

using ores::utility::generation::generation_keys;

domain::synthetic_fx_spot_config
generate_synthetic_synthetic_fx_spot_config(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::synthetic_fx_spot_config r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.name = std::string(faker::word::noun()) + " FX Config";
    r.description = std::string(faker::lorem::sentence());
    r.enabled = faker::datatype::boolean();
    r.auto_start = faker::datatype::boolean();
    r.base_currency_code = std::string(faker::finance::currencyCode());
    r.quote_currency_code = std::string(faker::finance::currencyCode());
    r.gmm_initial_price = faker::number::decimal(0.5, 2.0);
    r.ticks_per_hour = faker::number::integer(1, 60);
    r.process_type = std::string(faker::word::noun());
    r.price_source = std::string("vintage");
    r.vintage_source = std::string("ore.reference");
    r.vintage_date = std::string("2016-02-05");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::synthetic_fx_spot_config>
generate_synthetic_synthetic_fx_spot_configs(std::size_t n,
                                             utility::generation::generation_context& ctx) {
    std::vector<domain::synthetic_fx_spot_config> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_synthetic_fx_spot_config(ctx));
    return r;
}

}

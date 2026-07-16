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
#include "ores.synthetic.api/generators/fx_spot_generation_config_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::synthetic::generators {

using ores::utility::generation::generation_keys;

domain::fx_spot_generation_config
generate_synthetic_fx_spot_generation_config(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::fx_spot_generation_config r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.party_id = ctx.generate_uuid();
    r.config_id = ctx.generate_uuid();
    r.base_currency_code = std::string("EUR") + "-" + std::to_string(idx);
    r.quote_currency_code = std::string("USD") + "-" + std::to_string(idx);
    r.source_name = std::string("synthetic.") + std::string(faker::finance::currencyCode());
    r.ore_key = std::string("FX/RATE/EUR/USD");
    r.price_source = std::string("vintage");
    r.gmm_initial_price = 0.0;
    r.ticks_per_hour = faker::number::integer(1, 3600);
    r.process_type = std::string("geometric");
    r.enabled = faker::datatype::boolean();
    r.vintage_source = std::string("ore.reference");
    r.vintage_date = std::string("2016-02-05");
    r.folder_id = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::fx_spot_generation_config>
generate_synthetic_fx_spot_generation_configs(std::size_t n,
                                              utility::generation::generation_context& ctx) {
    std::vector<domain::fx_spot_generation_config> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_fx_spot_generation_config(ctx));
    return r;
}

}

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
#include "ores.synthetic.api/generators/fx_vol_surface_feed_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::synthetic::generators {

using ores::utility::generation::generation_keys;

domain::fx_vol_surface_feed
generate_synthetic_fx_vol_surface_feed(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::fx_vol_surface_feed r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.party_id = ctx.generate_uuid();
    r.config_id = ctx.generate_uuid();
    r.base_currency_code = // no_generator_suffix: the column is validated against the currency
                           // table, and the parent config keeps each row unique.
        std::string("EUR");
    r.quote_currency_code = // no_generator_suffix: the column is validated against the currency
                            // table, and the parent config keeps each row unique.
        std::string("USD");
    r.source_name = std::string("synthetic.fx_vol.eurusd");
    r.ticks_per_hour = 12;
    r.enabled = true;
    r.auto_start = false;
    r.folder_id = std::nullopt;
    r.asset_class_code = std::string("fx");
    r.arrival_mode = std::string("periodic");
    r.branching_ratio = 0.0;
    r.decay = 1.0;
    r.sensitivity = 1.0;
    r.skew_reversion = 3.0;
    r.skew_volatility = 0.6;
    r.curvature_reversion = 3.0;
    r.curvature_volatility = 0.4;
    r.spot_variance_correlation = 0.3;
    r.spot_skew_correlation = -0.4;
    r.segment_correlation = 0.9;
    r.start_skew = -0.1;
    r.start_curvature = 0.8;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::fx_vol_surface_feed>
generate_synthetic_fx_vol_surface_feeds(std::size_t n,
                                        utility::generation::generation_context& ctx) {
    std::vector<domain::fx_vol_surface_feed> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_fx_vol_surface_feed(ctx));
    return r;
}

}

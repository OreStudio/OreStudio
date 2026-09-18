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
#include "ores.marketdata.api/generators/market_series_asset_class_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.

namespace ores::marketdata::generators {

using ores::utility::generation::generation_keys;

domain::market_series_asset_class
generate_synthetic_market_series_asset_class(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(generation_keys::modified_by, "system");
    const auto tenant_id = ctx.env().get_or(generation_keys::tenant_id, "system");

    domain::market_series_asset_class r;
    r.version = 0;
    r.tenant_id = tenant_id;
    r.market_series_id = ctx.generate_uuid();
    r.asset_class_code = std::string("interest_rates");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::market_series_asset_class>
generate_synthetic_market_series_asset_classes(std::size_t n,
                                               utility::generation::generation_context& ctx) {
    std::vector<domain::market_series_asset_class> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_market_series_asset_class(ctx));
    return r;
}

}

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
#include "ores.refdata.api/generators/fx_convention_generator.hpp"

#include <atomic>
#include <string>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::fx_convention generate_synthetic_fx_convention(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");
    const auto tid_str = ctx.env().get_or(
        std::string(generation_keys::tenant_id), std::string("system"));

    domain::fx_convention r;
    r.version = 1;
    r.tenant_id = utility::uuid::tenant_id::from_string(tid_str)
        .value_or(utility::uuid::tenant_id::system());
    r.workspace_id = ctx.generate_uuid();
    r.id = std::string("EUR-USD-FX-CONVENTIONS") + "-"
        + std::to_string(counter.fetch_add(1, std::memory_order_relaxed));
    r.spot_days = 2;
    r.source_currency = std::string("EUR");
    r.target_currency = std::string("USD");
    r.points_factor = 10000.0;
    r.advance_calendar = std::nullopt;
    r.spot_relative = std::nullopt;
    r.end_of_month = std::nullopt;
    r.convention = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::fx_convention>
generate_synthetic_fx_conventions(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::fx_convention> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_fx_convention(ctx));
    return r;
}

}

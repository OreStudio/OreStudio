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
#include "ores.refdata.api/generators/currency_pair_convention_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::currency_pair_convention
generate_synthetic_currency_pair_convention(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::currency_pair_convention r;
    r.version = 1;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.pair_code = std::string(faker::finance::currencyCode()) + "/" +
                  std::string(faker::finance::currencyCode()) + "-" + std::to_string(idx);
    r.pip_factor = 0.0001;
    r.tick_size = 0.1;
    r.decimal_places = 4;
    r.advance_calendar = std::string("TARGET");
    r.business_day_convention = std::string("Following");
    r.spot_relative = true;
    r.end_of_month = false;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::currency_pair_convention>
generate_synthetic_currency_pair_conventions(std::size_t n,
                                             utility::generation::generation_context& ctx) {
    std::vector<domain::currency_pair_convention> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_currency_pair_convention(ctx));
    return r;
}

}

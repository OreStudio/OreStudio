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
#include "ores.refdata.api/generators/deposit_convention_generator.hpp"

#include <atomic>
#include <string>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::deposit_convention generate_synthetic_deposit_convention(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    domain::deposit_convention r;
    r.version = 1;
    r.id = std::string("USD-LIBOR-CONVENTIONS") + "-"
        + std::to_string(counter.fetch_add(1, std::memory_order_relaxed));
    r.index_based = true;
    r.index = std::string("USD-LIBOR");
    r.calendar = std::nullopt;
    r.convention = std::nullopt;
    r.end_of_month = std::nullopt;
    r.day_count_fraction = std::nullopt;
    r.settlement_days = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::deposit_convention>
generate_synthetic_deposit_conventions(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::deposit_convention> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_deposit_convention(ctx));
    return r;
}

}

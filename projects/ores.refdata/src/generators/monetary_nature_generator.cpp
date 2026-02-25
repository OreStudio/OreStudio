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
#include "ores.refdata/generators/monetary_nature_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::monetary_nature generate_synthetic_monetary_nature(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    static const std::vector<std::string> codes = {
        "fiat", "commodity", "synthetic", "supranational"
    };
    const auto idx = counter++ % codes.size();

    domain::monetary_nature r;
    r.version = 1;
    r.code = codes[idx];
    r.name = std::string(faker::word::adjective()) + " Asset Class";
    r.description = std::string(faker::lorem::sentence());
    r.display_order = faker::number::integer(1, 100);
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::monetary_nature>
generate_synthetic_monetary_natures(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::monetary_nature> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_monetary_nature(ctx));
    return r;
}

}

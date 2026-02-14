/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/generators/change_reason_category_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::dq::generators {

using ores::utility::generation::generation_keys;

domain::change_reason_category generate_synthetic_change_reason_category(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    domain::change_reason_category r;
    r.version = 1;
    r.code = std::string(faker::word::noun()) + "_" + std::string(faker::word::noun());
    r.description = std::string(faker::lorem::sentence());
    r.modified_by = modified_by;
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::change_reason_category>
generate_synthetic_change_reason_categories(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::change_reason_category> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_change_reason_category(ctx));
    return r;
}

}

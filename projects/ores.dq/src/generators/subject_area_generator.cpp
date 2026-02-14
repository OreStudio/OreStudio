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
#include "ores.dq/generators/subject_area_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::dq::generators {

using ores::utility::generation::generation_keys;

domain::subject_area generate_synthetic_subject_area(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");

    domain::subject_area r;
    r.version = 1;
    r.name = std::string(faker::word::noun());
    r.domain_name = std::string(faker::word::noun());
    r.description = std::string(faker::lorem::sentence());
    r.modified_by = modified_by;
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

domain::subject_area generate_synthetic_subject_area(
    const std::string& domain_name,
    utility::generation::generation_context& ctx) {
    auto r = generate_synthetic_subject_area(ctx);
    r.domain_name = domain_name;
    return r;
}

std::vector<domain::subject_area>
generate_synthetic_subject_areas(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::subject_area> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_subject_area(ctx));
    return r;
}

}

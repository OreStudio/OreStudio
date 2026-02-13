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
#include "ores.utility/faker/datetime.hpp"

namespace ores::dq::generators {

domain::subject_area generate_synthetic_subject_area() {
    domain::subject_area r;
    r.version = 1;
    r.name = std::string(faker::word::noun());
    r.domain_name = std::string(faker::word::noun());
    r.description = std::string(faker::lorem::sentence());
    r.modified_by = std::string(faker::internet::username());
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

domain::subject_area generate_synthetic_subject_area(const std::string& domain_name) {
    auto r = generate_synthetic_subject_area();
    r.domain_name = domain_name;
    return r;
}

std::vector<domain::subject_area>
generate_synthetic_subject_areas(std::size_t n) {
    std::vector<domain::subject_area> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_subject_area());
    return r;
}

}

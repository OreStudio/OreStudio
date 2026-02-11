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
#include "ores.dq/generators/coding_scheme_generator.hpp"

#include <array>
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/datetime.hpp"

namespace ores::dq::generators {

domain::coding_scheme generate_synthetic_coding_scheme() {
    static constexpr std::array<const char*, 3> authority_types = {
        "official", "industry", "internal"
    };
    static std::atomic<int> counter{0};
    const auto idx = counter++;

    domain::coding_scheme r;
    r.version = 1;
    r.code = std::string(faker::word::noun()) + "_" + std::string(faker::word::noun())
        + "_" + std::to_string(idx + 1);
    r.name = std::string(faker::word::adjective()) + " " + std::string(faker::word::noun())
        + " " + std::to_string(idx + 1);
    r.authority_type = std::string(authority_types[idx % authority_types.size()]);
    r.subject_area_name = std::string("General");
    r.domain_name = std::string("Reference Data");
    if (faker::datatype::boolean()) {
        r.uri = "https://example.org/schemes/" + r.code;
    }
    r.description = std::string(faker::lorem::sentence());
    r.recorded_by = std::string(faker::internet::username());
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::coding_scheme>
generate_synthetic_coding_schemes(std::size_t n) {
    std::vector<domain::coding_scheme> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_coding_scheme());
    return r;
}

}

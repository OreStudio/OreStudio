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
#include "ores.dq/generators/methodology_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/datetime.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::dq::generators {

using ores::utility::uuid::uuid_v7_generator;

domain::methodology generate_synthetic_methodology() {
    static uuid_v7_generator uuid_gen;

    domain::methodology r;
    r.version = 1;
    r.id = uuid_gen();
    r.name = std::string(faker::word::adjective()) + " " + std::string(faker::word::noun());
    r.description = std::string(faker::lorem::sentence());
    if (faker::datatype::boolean()) {
        r.logic_reference = "https://example.org/logic/" + std::string(faker::word::noun());
    }
    if (faker::datatype::boolean()) {
        r.implementation_details = std::string(faker::lorem::paragraph());
    }
    r.modified_by = std::string(faker::internet::username());
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::methodology>
generate_synthetic_methodologies(std::size_t n) {
    std::vector<domain::methodology> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_methodology());
    return r;
}

}

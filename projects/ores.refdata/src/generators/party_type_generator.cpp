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
#include "ores.refdata/generators/party_type_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/datetime.hpp"

namespace ores::refdata::generators {

domain::party_type generate_synthetic_party_type() {
    domain::party_type r;
    r.version = 1;
    r.code = std::string(faker::word::noun()) + "_type";
    r.name = std::string(faker::word::adjective()) + " Party";
    r.description = std::string(faker::lorem::sentence());
    r.display_order = faker::number::integer(1, 100);
    r.recorded_by = std::string(faker::internet::username());
    r.performed_by = std::string(faker::internet::username());
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::party_type>
generate_synthetic_party_types(std::size_t n) {
    std::vector<domain::party_type> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_party_type());
    return r;
}

}

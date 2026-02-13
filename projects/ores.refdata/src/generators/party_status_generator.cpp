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
#include "ores.refdata/generators/party_status_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/datetime.hpp"

namespace ores::refdata::generators {

domain::party_status generate_synthetic_party_status() {
    static std::atomic<int> counter{0};

    domain::party_status r;
    r.version = 1;
    r.code = std::string(faker::word::noun()) + "_status_" + std::to_string(++counter);
    r.name = std::string(faker::word::adjective()) + " Status";
    r.description = std::string(faker::lorem::sentence());
    r.display_order = faker::number::integer(1, 100);
    r.modified_by = std::string(faker::internet::username());
    r.performed_by = std::string(faker::internet::username());
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::party_status>
generate_synthetic_party_statuses(std::size_t n) {
    std::vector<domain::party_status> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_party_status());
    return r;
}

}

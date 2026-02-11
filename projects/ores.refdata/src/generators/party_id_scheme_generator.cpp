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
#include "ores.refdata/generators/party_id_scheme_generator.hpp"

#include <array>
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/datetime.hpp"

namespace ores::refdata::generators {

domain::party_id_scheme generate_synthetic_party_id_scheme() {
    static constexpr std::array<const char*, 10> coding_schemes = {
        "LEI", "BIC", "MIC", "NATIONAL_ID", "CEDB",
        "NATURAL_PERSON", "ACER", "DTCC_PARTICIPANT_ID", "MPID", "INTERNAL"
    };
    static std::atomic<int> counter{0};
    const auto idx = counter++;

    domain::party_id_scheme r;
    r.version = 1;
    r.code = std::string(faker::word::noun()) + "_scheme_" + std::to_string(idx + 1);
    r.name = std::string(faker::word::adjective()) + " Scheme";
    r.description = std::string(faker::lorem::sentence());
    r.coding_scheme_code = coding_schemes[idx % coding_schemes.size()];
    r.display_order = faker::number::integer(1, 100);
    r.recorded_by = std::string(faker::internet::username());
    r.performed_by = std::string(faker::internet::username());
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::party_id_scheme>
generate_synthetic_party_id_schemes(std::size_t n) {
    std::vector<domain::party_id_scheme> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_party_id_scheme());
    return r;
}

}

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
#include "ores.refdata/generators/party_contact_information_generator.hpp"

#include <array>
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/datetime.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::refdata::generators {

using ores::utility::uuid::uuid_v7_generator;

domain::party_contact_information generate_synthetic_party_contact_information() {
    static uuid_v7_generator uuid_gen;
    static constexpr std::array<const char*, 4> contact_types = {
        "Legal", "Operations", "Settlement", "Billing"
    };
    static std::atomic<int> counter{0};
    const auto idx = counter++;

    domain::party_contact_information r;
    r.version = 1;
    r.tenant_id = "system";
    r.id = uuid_gen();
    r.party_id = uuid_gen();
    r.contact_type = std::string(contact_types[idx % contact_types.size()]);
    r.street_line_1 = std::string("123 Test Street");
    r.street_line_2 = std::string("Suite 100");
    r.city = std::string("London");
    r.state = std::string("");
    r.country_code = std::string("");
    r.postal_code = std::string("EC2V 8AS");
    r.phone = std::string("+44 20 7000 0000");
    r.email = std::string("contact@example.com");
    r.web_page = std::string("https://example.com");
    r.modified_by = std::string(faker::internet::username());
    r.performed_by = std::string(faker::internet::username());
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::party_contact_information>
generate_synthetic_party_contact_informations(std::size_t n) {
    std::vector<domain::party_contact_information> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_party_contact_information());
    return r;
}

}

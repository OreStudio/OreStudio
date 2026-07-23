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
#include "ores.iam.api/generators/account_contact_information_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::account_contact_information
generate_synthetic_account_contact_information(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::account_contact_information r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.account_id = ctx.generate_uuid();
    r.full_name = std::string("Test User");
    r.street_line_1 = std::string("123 Test Street");
    r.street_line_2 = std::string("Suite 100");
    r.city = std::string("London");
    r.state = std::string("");
    r.country_code = // Left blank: validated against the tenant-scoped countries table, which
                     // isolated test tenants don't seed; empty skips validation.
        std::string("");
    r.postal_code = std::string("EC2V 8AS");
    r.phone = std::string("+44 20 7000 0000");
    r.email = std::string("contact@example.com");
    r.web_page = std::string("https://example.com");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::account_contact_information>
generate_synthetic_account_contact_informations(std::size_t n,
                                                utility::generation::generation_context& ctx) {
    std::vector<domain::account_contact_information> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_account_contact_information(ctx));
    return r;
}

}

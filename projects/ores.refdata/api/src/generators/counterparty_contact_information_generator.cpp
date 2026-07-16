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
#include "ores.refdata.api/generators/counterparty_contact_information_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::counterparty_contact_information
generate_synthetic_counterparty_contact_information(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::counterparty_contact_information r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.counterparty_id = ctx.generate_uuid();
    r.contact_type = // no_generator_suffix: validated enum, a "-<idx>" suffix would be invalid.
                     // Rotate so a batch of several contact rows for one counterparty gets
                     // distinct types (max one contact row per type per counterparty).
        [idx] {
            static constexpr const char* types[] = {"Legal", "Operations", "Settlement", "Billing"};
            return std::string(types[idx % 4]);
        }();
    r.street_line_1 = std::string("456 Test Avenue");
    r.street_line_2 = std::string("Floor 10");
    r.city = std::string("New York");
    r.state = std::string("NY");
    r.country_code = // Left blank: validated against the tenant-scoped countries table, which
                     // isolated test tenants don't seed; empty skips validation.
        std::string("");
    r.postal_code = std::string("10001");
    r.phone = std::string("+1 212 555 0100");
    r.email = std::string("info@example.com");
    r.web_page = std::string("https://example.com");
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::counterparty_contact_information>
generate_synthetic_counterparty_contact_informations(std::size_t n,
                                                     utility::generation::generation_context& ctx) {
    std::vector<domain::counterparty_contact_information> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_counterparty_contact_information(ctx));
    return r;
}

}

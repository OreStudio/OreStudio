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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_generator.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::account generate_synthetic_account(utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::account r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.username = std::string(faker::internet::username(std::string(faker::person::firstName()),
                                                       std::string(faker::person::lastName()))) +
                 "-" + std::to_string(idx);
    r.account_type = std::string("user");
    r.full_name =
        std::string(faker::person::firstName()) + " " + std::string(faker::person::lastName());
    r.password_hash = ctx.alphanumeric(64);
    r.password_salt = ctx.alphanumeric(32);
    r.totp_secret = ctx.alphanumeric(32);
    r.email = std::string(faker::internet::email(std::string(faker::person::firstName()),
                                                 std::string(faker::person::lastName()))) +
              "-" + std::to_string(idx);
    r.job_title = faker::person::jobTitle();
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::account>
generate_synthetic_accounts(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::account> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_account(ctx));
    return r;
}

}

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
#include "ores.iam/generators/account_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/faker/totp.hpp"

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::account generate_synthetic_account(
    utility::generation::generation_context& ctx) {
    domain::account r;
    r.version = 1;
    const auto tid = ctx.env().get_or(
        std::string(generation_keys::tenant_id), "system");
    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);
    r.tenant_id = parsed_tid.has_value() ? parsed_tid.value()
        : utility::uuid::tenant_id::system();
    r.modified_by = ctx.env().get_or(std::string(generation_keys::modified_by),
        "system");
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";

    r.id = ctx.generate_uuid();

    auto first = std::string(faker::person::firstName());
    auto last  = std::string(faker::person::lastName());
    r.username = faker::internet::username(first, last);
    r.email = faker::internet::email(first, last);

    r.password_hash = ctx.alphanumeric(64);
    r.password_salt = ctx.alphanumeric(32);

    using utility::faker::totp;
    r.totp_secret = totp::totp_secret();

    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::account>
generate_synthetic_accounts(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::account> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_account(ctx));
    return r;
}

}

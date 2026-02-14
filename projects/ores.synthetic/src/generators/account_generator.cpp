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
#include "ores.synthetic/generators/account_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::synthetic::generators {

using ores::utility::generation::generation_keys;

iam::domain::account generate_synthetic_account(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");

    iam::domain::account r;
    r.version = 1;
    r.id = ctx.generate_uuid();
    r.modified_by = modified_by;
    r.change_reason_code = "SYNTHETIC";
    r.change_commentary = "Synthetic test data";
    r.username = std::string(faker::internet::username());
    r.password_hash = ctx.alphanumeric(64);
    r.password_salt = ctx.alphanumeric(32);
    r.totp_secret = ctx.alphanumeric(32);
    r.email = std::string(faker::internet::email());
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<iam::domain::account>
generate_synthetic_accounts(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<iam::domain::account> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_account(ctx));
    return r;
}

}

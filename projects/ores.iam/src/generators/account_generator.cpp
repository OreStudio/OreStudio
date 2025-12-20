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
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.utility/faker/totp.hpp"
#include "ores.utility/faker/datetime.hpp"

namespace ores::iam::generators {

domain::account generate_synthetic_account() {
    domain::account r;
    r.version = 1;
    r.recorded_by = faker::internet::username();

    boost::uuids::string_generator gen;
    r.id = gen(faker::string::uuidV4());

    auto first = std::string(faker::person::firstName());
    auto last  = std::string(faker::person::lastName());
    r.username = faker::internet::username(first, last);
    r.email = faker::internet::email(first, last);

    r.password_hash = faker::crypto::sha256();
    r.password_salt = faker::crypto::sha256();

    using utility::faker::totp;
    r.totp_secret = totp::totp_secret();

    r.is_admin = false;
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::account>
generate_synthetic_accounts(std::size_t n) {
    std::vector<domain::account> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_account());
    return r;
}

}

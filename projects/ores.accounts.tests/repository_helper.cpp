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
#include "ores.accounts.tests/repository_helper.hpp"

#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "faker-cxx/faker.h" // IWYU pragma: keep.

namespace ores::accounts::tests {

using accounts::domain::account;

accounts::domain::account repository_helper::
create_test_account(const std::string& username, bool is_admin) {
    account acc;
    acc.version = 1;
    acc.modified_by = faker::internet::username();
    acc.id = boost::uuids::random_generator()();
    acc.username = username;
    acc.password_hash = faker::crypto::sha256();
    acc.password_salt = faker::crypto::sha256();
    acc.totp_secret = "TOTP_SECRET_" + username;
    acc.email = username + "@test.com";
    acc.is_admin = is_admin;
    return acc;
}

void repository_helper::cleanup_database() {
    truncate_table("oresdb.accounts");
}

}

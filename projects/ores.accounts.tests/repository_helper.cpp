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
#include "faker-cxx/faker.h"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.utility/repository/context_factory.hpp"

namespace ores::accounts::tests {

using namespace ores::utility::log;
using accounts::domain::account;
using utility::repository::context;
using utility::repository::context_factory;

repository_helper::repository_helper() : context_(make_context()) {}

context repository_helper::make_context() {
    context_factory::configuration db_cfg{
        .user = "ores",
        .password = "ahV6aehuij6eingohsiajaiT0",
        .host = "localhost",
        .database = "oresdb",
        .port = 5432,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    try {
        context ctx = context_factory::make_context(db_cfg);
        BOOST_LOG_SEV(lg(), info) << "Database context created successfully";
        return ctx;

        // Ensure table exists
        accounts::repository::account_repository repo;
        const auto sql = repo.sql();
        BOOST_LOG_SEV(lg(), debug) << "Table SQL: " << sql;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to create database context: "
                                   << e.what();
        throw;
    }
}

accounts::domain::account repository_helper::
create_test_account(const std::string& username, bool is_admin) {
    account acc;
    acc.version = 1;
    acc.modified_by = faker::internet::username();
    acc.id = boost::uuids::random_generator()();
    acc.username = faker::internet::username();
    acc.password_hash = faker::crypto::sha256();
    acc.password_salt = faker::crypto::sha256();
    acc.totp_secret = "TOTP_SECRET_" + username;
    acc.email = faker::internet::email();
    acc.is_admin = is_admin;
    return acc;
}

}

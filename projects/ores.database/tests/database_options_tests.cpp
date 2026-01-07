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
#include "ores.database/domain/database_options.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.database.tests");
const std::string tags("[database]");

}

using namespace ores::database;
using namespace ores::logging;

TEST_CASE("database_options_to_credentials", tags) {
    auto lg(make_logger(test_suite));

    database_options opts;
    opts.host = "localhost";
    opts.port = 5432;
    opts.database = "test_db";
    opts.user = "test_user";
    opts.password = "test_pass";

    auto creds = to_credentials(opts);

    REQUIRE(creds.host == "localhost");
    REQUIRE(creds.port == 5432);
    REQUIRE(creds.dbname == "test_db");
    REQUIRE(creds.user == "test_user");
    REQUIRE(creds.password == "test_pass");

    BOOST_LOG_SEV(lg, info) << "Database options to credentials test passed";
}

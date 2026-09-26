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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.service/service/exit_codes.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[exit_codes]");

}

using ores::service::service::exit_code;

// Every service launcher returns these values as the process exit status, and
// the deployment reads the number rather than the enumerator. Renumbering an
// entry compiles and passes every other test, so the numbers are pinned here.
TEST_CASE("exit codes keep the values the service launchers report", tags) {
    REQUIRE(static_cast<int>(exit_code::ok) == 0);
    REQUIRE(static_cast<int>(exit_code::general_error) == 1);
    REQUIRE(static_cast<int>(exit_code::config_error) == 2);
    REQUIRE(static_cast<int>(exit_code::db_connection_failed) == 3);
    REQUIRE(static_cast<int>(exit_code::nats_connection_failed) == 4);
    REQUIRE(static_cast<int>(exit_code::startup_timeout) == 5);
    REQUIRE(static_cast<int>(exit_code::auth_error) == 6);
}

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
#include "ores.marketdata.service/config/options.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include <string>

using ores::marketdata::service::config::options;

TEST_CASE("printed options name each member in declaration order and no password", "[config]") {
    options o;
    o.database.password = "db-secret";
    o.tick_writer_user = "tick";
    o.tick_writer_password = "tick-secret";
    o.http_base_url = "http://localhost:1";

    std::ostringstream s;
    s << o;
    const auto text = s.str();

    CHECK(text.find("secret") == std::string::npos);
    CHECK(text.find("\"logging\"") == std::string::npos);

    const auto nats = text.find("\"nats\":{");
    const auto database = text.find("\"database\":{");
    const auto url = text.find("\"http_base_url\":\"http://localhost:1\"");
    const auto user = text.find("\"tick_writer_user\":\"tick\"");
    REQUIRE(nats != std::string::npos);
    REQUIRE(database != std::string::npos);
    REQUIRE(url != std::string::npos);
    REQUIRE(user != std::string::npos);
    CHECK(nats < database);
    CHECK(database < url);
    CHECK(url < user);
}

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
#include <catch2/catch_test_macros.hpp>

// Simple proof-of-concept tests without logging for now
// Once we fully migrate to Catch2, we'll update the logging infrastructure

TEST_CASE("basic_arithmetic", "[catch2_poc]") {
    SECTION("addition") {
        REQUIRE(2 + 2 == 4);
    }

    SECTION("subtraction") {
        REQUIRE(5 - 3 == 2);
    }

    SECTION("multiplication") {
        REQUIRE(3 * 4 == 12);
    }
}

TEST_CASE("string_operations", "[catch2_poc]") {
    std::string hello = "Hello";
    std::string world = "World";

    REQUIRE(hello.size() == 5);
    REQUIRE(world.size() == 5);

    std::string combined = hello + " " + world;
    REQUIRE(combined == "Hello World");
}

TEST_CASE("boolean_logic", "[catch2_poc]") {
    bool t = true;
    bool f = false;

    CHECK(t == true);
    CHECK(f == false);
    CHECK((t && f) == false);
    CHECK((t || f) == true);
}

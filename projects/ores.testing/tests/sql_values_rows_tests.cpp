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
#include "ores.testing/sql_values_rows.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string tags("[ores.testing.sql_values_rows]");
const std::string script("probe.sql");

using ores::testing::sql_values_rows;

}

TEST_CASE("sql_values_rows reads a row into its column texts", tags) {
    const auto rows = sql_values_rows("\nvalues\n(1, 'a', 'b');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"1", "a", "b"});
}

TEST_CASE("sql_values_rows unquotes a column", tags) {
    const auto rows = sql_values_rows("\nvalues\n('FX', 'RATE');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "RATE"});
}

TEST_CASE("sql_values_rows keeps a comma inside a quoted column", tags) {
    const auto rows = sql_values_rows("\nvalues\n('a,b', 'c');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"a,b", "c"});
}

TEST_CASE("sql_values_rows unescapes a doubled quote", tags) {
    const auto rows = sql_values_rows("\nvalues\n('it''s', 'x');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"it's", "x"});
}

TEST_CASE("sql_values_rows keeps a comma inside nested parentheses", tags) {
    const auto rows = sql_values_rows("\nvalues\n(ores_fn(1, 2), 'x');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"ores_fn(1, 2)", "x"});
}

TEST_CASE("sql_values_rows reads a row split across lines", tags) {
    const auto rows = sql_values_rows("\nvalues\n('FX',\n     'RATE',\n     'SPOT');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "RATE", "SPOT"});
}

TEST_CASE("sql_values_rows trims the whitespace the opening parenthesis exposes", tags) {
    const auto rows = sql_values_rows("\nvalues\n   (   'FX'  ,  'RATE'  );", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "RATE"});
}

TEST_CASE("sql_values_rows keeps the null keyword as text", tags) {
    const auto rows = sql_values_rows("\nvalues\n('FX', null);", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "null"});
}

TEST_CASE("sql_values_rows skips a comment between the rows", tags) {
    const auto rows = sql_values_rows("\nvalues\n(1, 'a'),\n-- a note\n(2, 'b');", script);

    REQUIRE(rows.size() == 2);
    CHECK(rows[0] == std::vector<std::string>{"1", "a"});
    CHECK(rows[1] == std::vector<std::string>{"2", "b"});
}

TEST_CASE("sql_values_rows skips a comment inside a row", tags) {
    const auto rows = sql_values_rows("\nvalues\n(1, -- why\n     'a');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"1", "a"});
}

TEST_CASE("sql_values_rows keeps a comment marker inside a quoted column", tags) {
    const auto rows = sql_values_rows("\nvalues\n('a -- b');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"a -- b"});
}

TEST_CASE("sql_values_rows reads one column from a one-column row", tags) {
    const auto rows = sql_values_rows("\nvalues\n('FX');", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX"});
}

TEST_CASE("sql_values_rows stops at the tail of the statement", tags) {
    const auto rows = sql_values_rows("\nvalues\n(1, 'a')\non conflict do nothing;", script);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"1", "a"});
}

TEST_CASE("sql_values_rows refuses a script with no values list", tags) {
    CHECK_THROWS_AS(sql_values_rows("select 1;", script), std::invalid_argument);
}

TEST_CASE("sql_values_rows refuses a values list with no rows", tags) {
    CHECK_THROWS_AS(sql_values_rows("\nvalues\n;", script), std::invalid_argument);
}

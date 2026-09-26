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
#include "ores.logging/make_logger.hpp"
#include "ores.testing/sql_values_rows.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string test_suite("ores.testing.tests");
const std::string tags("[testing]");
const std::string script("probe.sql");

using ores::testing::sql_values_rows;

}

TEST_CASE("reads_one_row_into_column_texts", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n(1, 'a', 'b');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "Columns: " << rows.front().size();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"1", "a", "b"});
}

TEST_CASE("unquotes_a_column", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n('FX', 'RATE');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "First column: " << rows.front().front();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "RATE"});
}

TEST_CASE("keeps_a_comma_inside_a_quoted_column", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n('a,b', 'c');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "First column: " << rows.front().front();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"a,b", "c"});
}

TEST_CASE("unescapes_a_doubled_quote", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n('it''s', 'x');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "First column: " << rows.front().front();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"it's", "x"});
}

TEST_CASE("keeps_a_comma_inside_nested_parentheses", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n(ores_fn(1, 2), 'x');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "First column: " << rows.front().front();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"ores_fn(1, 2)", "x"});
}

TEST_CASE("reads_a_row_split_across_lines", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n('FX',\n     'RATE',\n     'SPOT');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "Columns: " << rows.front().size();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "RATE", "SPOT"});
}

TEST_CASE("trims_the_whitespace_the_opening_parenthesis_exposes", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n   (   'FX'  ,  'RATE'  );", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "First column: " << rows.front().front();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "RATE"});
}

TEST_CASE("trims_a_vertical_tab_and_a_form_feed", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n(\v'FX', 'RATE'\f);", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "First column: " << rows.front().front();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "RATE"});
}

TEST_CASE("keeps_the_null_keyword_as_text", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n('FX', null);", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "Second column: " << rows.front().back();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX", "null"});
}

TEST_CASE("skips_a_comment_between_rows", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n(1, 'a'),\n-- a note\n(2, 'b');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "Rows: " << rows.size();

    REQUIRE(rows.size() == 2);
    CHECK(rows[0] == std::vector<std::string>{"1", "a"});
    CHECK(rows[1] == std::vector<std::string>{"2", "b"});
}

TEST_CASE("skips_a_comment_inside_a_row", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n(1, -- why\n     'a');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "Second column: " << rows.front().back();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"1", "a"});
}

TEST_CASE("keeps_a_comment_marker_inside_a_quoted_column", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n('a -- b');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "First column: " << rows.front().front();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"a -- b"});
}

TEST_CASE("reads_one_column_from_a_one_column_row", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n('FX');", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "First column: " << rows.front().front();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"FX"});
}

TEST_CASE("stops_at_the_tail_of_the_statement", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto rows = sql_values_rows("\nvalues\n(1, 'a')\non conflict do nothing;", script);
    BOOST_LOG_SEV(lg, ores::logging::info) << "Rows: " << rows.size();

    REQUIRE(rows.size() == 1);
    CHECK(rows[0] == std::vector<std::string>{"1", "a"});
}

TEST_CASE("refuses_a_script_with_no_values_list", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    BOOST_LOG_SEV(lg, ores::logging::info) << "Script without a values list: " << script;
    CHECK_THROWS_AS(sql_values_rows("select 1;", script), std::invalid_argument);
}

TEST_CASE("refuses_a_values_list_with_no_rows", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    BOOST_LOG_SEV(lg, ores::logging::info) << "Script with an empty values list: " << script;
    CHECK_THROWS_AS(sql_values_rows("\nvalues\n;", script), std::invalid_argument);
}

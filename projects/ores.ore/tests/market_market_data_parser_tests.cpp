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
#include "ores.ore/market/market_data_parser.hpp"

#include <chrono>
#include <sstream>
#include <stdexcept>
#include <string>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[ore][market][market_data_parser]");

std::chrono::year_month_day ymd(int y, unsigned m, unsigned d) {
    return std::chrono::year_month_day{
        std::chrono::year{y}, std::chrono::month{m}, std::chrono::day{d}};
}

} // namespace

using ores::ore::market::parse_market_data;
using ores::ore::market::parse_fixings;

// =============================================================================
// parse_market_data — date formats
// =============================================================================

TEST_CASE("parse_market_data_accepts_yyyymmdd_date", tags) {
    std::istringstream in("20160205 ZERO/RATE/EUR/BANK_EUR_BORROW/A365/2Y 0.0024\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].date  == ymd(2016, 2, 5));
    CHECK(result[0].key   == "ZERO/RATE/EUR/BANK_EUR_BORROW/A365/2Y");
    CHECK(result[0].value == "0.0024");
}

TEST_CASE("parse_market_data_accepts_yyyy_mm_dd_date", tags) {
    std::istringstream in("2022-01-31 BASIS_SWAP/BASIS_SPREAD/3M/1D/USD/10Y 0.0027750000\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].date  == ymd(2022, 1, 31));
    CHECK(result[0].key   == "BASIS_SWAP/BASIS_SPREAD/3M/1D/USD/10Y");
    CHECK(result[0].value == "0.0027750000");
}

// =============================================================================
// parse_market_data — value precision
// =============================================================================

TEST_CASE("parse_market_data_preserves_trailing_zeros_in_value", tags) {
    std::istringstream in("20160205 FX/RATE/EUR/CHF 0.0074600000\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].value == "0.0074600000");
}

TEST_CASE("parse_market_data_preserves_integer_value", tags) {
    std::istringstream in("20160205 EQUITY/PRICE/SP5/USD 2023.81\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].value == "2023.81");
}

TEST_CASE("parse_market_data_preserves_negative_value", tags) {
    std::istringstream in("20160205 ZERO/RATE/EUR/BANK_EUR_BORROW/A365/3M -0.006119\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].value == "-0.006119");
}

// =============================================================================
// parse_market_data — whitespace and comments
// =============================================================================

TEST_CASE("parse_market_data_skips_blank_lines", tags) {
    std::istringstream in(
        "20160205 FX/RATE/EUR/CHF 1.0947\n"
        "\n"
        "   \n"
        "20160205 FX/RATE/EUR/USD 1.0857\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 2);
    CHECK(result[0].key == "FX/RATE/EUR/CHF");
    CHECK(result[1].key == "FX/RATE/EUR/USD");
}

TEST_CASE("parse_market_data_skips_comment_lines", tags) {
    std::istringstream in(
        "# This is a comment\n"
        "20160205 FX/RATE/EUR/CHF 1.0947\n"
        "# Another comment\n"
        "20160205 FX/RATE/EUR/USD 1.0857\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 2);
    CHECK(result[0].key == "FX/RATE/EUR/CHF");
    CHECK(result[1].key == "FX/RATE/EUR/USD");
}

TEST_CASE("parse_market_data_comment_only_file_returns_empty", tags) {
    std::istringstream in("# just a comment\n# another comment\n");
    const auto result = parse_market_data(in);
    CHECK(result.empty());
}

TEST_CASE("parse_market_data_empty_stream_returns_empty", tags) {
    std::istringstream in("");
    const auto result = parse_market_data(in);
    CHECK(result.empty());
}

TEST_CASE("parse_market_data_accepts_tab_separated_line", tags) {
    std::istringstream in("20160205\tFX/RATE/EUR/CHF\t1.0947\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].key   == "FX/RATE/EUR/CHF");
    CHECK(result[0].value == "1.0947");
}

// =============================================================================
// parse_market_data — error handling
// =============================================================================

TEST_CASE("parse_market_data_throws_on_one_token_line", tags) {
    std::istringstream in("20160205\n");
    CHECK_THROWS_AS(parse_market_data(in), std::invalid_argument);
}

TEST_CASE("parse_market_data_throws_on_two_token_line", tags) {
    std::istringstream in("20160205 FX/RATE/EUR/CHF\n");
    CHECK_THROWS_AS(parse_market_data(in), std::invalid_argument);
}

TEST_CASE("parse_market_data_error_message_contains_line_number", tags) {
    std::istringstream in("20160205 FX/RATE/EUR/CHF\n");
    try {
        parse_market_data(in);
        FAIL("expected exception");
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("1") != std::string::npos);
    }
}

TEST_CASE("parse_market_data_throws_on_unrecognised_date_format", tags) {
    std::istringstream in("2016/02/05 FX/RATE/EUR/CHF 1.0947\n");
    CHECK_THROWS_AS(parse_market_data(in), std::invalid_argument);
}

// =============================================================================
// parse_market_data — multiple entries
// =============================================================================

TEST_CASE("parse_market_data_parses_multiple_entries_in_order", tags) {
    std::istringstream in(
        "20160205 ZERO/RATE/EUR/BANK_EUR_BORROW/A365/2Y 0.0024\n"
        "20160205 FX/RATE/EUR/CHF 1.0947\n"
        "20160205 EQUITY/PRICE/SP5/USD 2023.81\n");
    const auto result = parse_market_data(in);

    REQUIRE(result.size() == 3);
    CHECK(result[0].key == "ZERO/RATE/EUR/BANK_EUR_BORROW/A365/2Y");
    CHECK(result[1].key == "FX/RATE/EUR/CHF");
    CHECK(result[2].key == "EQUITY/PRICE/SP5/USD");
}

// =============================================================================
// parse_fixings — date formats
// =============================================================================

TEST_CASE("parse_fixings_accepts_yyyy_mm_dd_date", tags) {
    std::istringstream in("2021-09-30 EUR-EONIA 0.0074600000\n");
    const auto result = parse_fixings(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].date       == ymd(2021, 9, 30));
    CHECK(result[0].index_name == "EUR-EONIA");
    CHECK(result[0].value      == "0.0074600000");
}

TEST_CASE("parse_fixings_accepts_yyyymmdd_date", tags) {
    std::istringstream in("20160205 EUR-EONIA -0.003560\n");
    const auto result = parse_fixings(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].date       == ymd(2016, 2, 5));
    CHECK(result[0].index_name == "EUR-EONIA");
    CHECK(result[0].value      == "-0.003560");
}

// =============================================================================
// parse_fixings — whitespace, comments, errors
// =============================================================================

TEST_CASE("parse_fixings_skips_blank_and_comment_lines", tags) {
    std::istringstream in(
        "# fixings file\n"
        "\n"
        "2016-01-28 EQ-SP5 2244.2\n"
        "2016-01-29 EQ-SP5 2210\n");
    const auto result = parse_fixings(in);

    REQUIRE(result.size() == 2);
    CHECK(result[0].index_name == "EQ-SP5");
    CHECK(result[0].value      == "2244.2");
    CHECK(result[1].value      == "2210");
}

TEST_CASE("parse_fixings_preserves_trailing_zeros_in_value", tags) {
    std::istringstream in("2021-09-30 EUR-EONIA 0.0074600000\n");
    const auto result = parse_fixings(in);

    REQUIRE(result.size() == 1);
    CHECK(result[0].value == "0.0074600000");
}

TEST_CASE("parse_fixings_throws_on_fewer_than_three_tokens", tags) {
    std::istringstream in("2021-09-30 EUR-EONIA\n");
    CHECK_THROWS_AS(parse_fixings(in), std::invalid_argument);
}

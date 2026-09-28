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
#include "ores.platform/numeric/floating_point.hpp"
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <charconv>
#include <string>

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[numeric][floating_point]");

using ores::platform::numeric::parse_double;

}

using namespace ores::logging;

TEST_CASE("a_complete_number_parses_to_its_value", tags) {
    auto lg(make_logger(test_suite));

    CHECK(parse_double("1.5") == 1.5);
    CHECK(parse_double("-2.5e3") == -2500.0);
    CHECK(parse_double("0") == 0.0);
    CHECK(parse_double("6.02E-2") == 0.0602);
}

TEST_CASE("anything_but_the_number_is_refused", tags) {
    auto lg(make_logger(test_suite));

    // The contract is from_chars', not strtod's: the whole input is the
    // number, so nothing may surround it.
    CHECK(parse_double("") == std::nullopt);
    CHECK(parse_double(" 1.5") == std::nullopt);
    CHECK(parse_double("1.5 ") == std::nullopt);
    CHECK(parse_double("\t1.5") == std::nullopt);
    CHECK(parse_double("1.5\n") == std::nullopt);
    CHECK(parse_double("1.5x") == std::nullopt);
    CHECK(parse_double("abc") == std::nullopt);
    CHECK(parse_double("-") == std::nullopt);
    CHECK(parse_double("1.5.2") == std::nullopt);
}

TEST_CASE("a_comma_is_not_a_decimal_point_whatever_the_locale_is", tags) {
    auto lg(make_logger(test_suite));

    // The parser must not follow the program's locale: a value written with
    // a comma is not a number here, in any locale.
    CHECK(parse_double("1,5") == std::nullopt);
}

TEST_CASE("a_value_outside_the_range_of_a_double_is_refused", tags) {
    auto lg(make_logger(test_suite));

    CHECK(parse_double("1e400") == std::nullopt);
    CHECK(parse_double("-1e400") == std::nullopt);
}

TEST_CASE("every_formatted_double_parses_back_to_itself", tags) {
    auto lg(make_logger(test_suite));

    // The estate formats doubles with the floating-point to_chars, which
    // libc++ does implement, and reads them back with this parser. The pair
    // has to round trip.
    constexpr std::array values{
        0.0, 1.0, -1.0, 0.1, 1.0 / 3.0, 123456.789, -6.02e-2, 1e300, 4.9e-324};
    for (const auto value : values) {
        std::array<char, 64> buffer{};
        const auto written = std::to_chars(buffer.data(), buffer.data() + buffer.size(), value);
        REQUIRE(written.ec == std::errc{});
        const std::string text(buffer.data(), written.ptr);
        const auto parsed = parse_double(text);
        BOOST_LOG_SEV(lg, info) << "Round tripped " << text;
        REQUIRE(parsed.has_value());
        CHECK(*parsed == value);
    }
}

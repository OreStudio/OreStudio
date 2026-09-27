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
#include "ores.utility/decimal/decimal.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <limits>
#include <sstream>

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[decimal]");

}

using ores::utility::decimal::decimal;
using namespace ores::logging;

namespace {

decimal parsed(const std::string& text) {
    return decimal::from_string(text).value();
}

}

TEST_CASE("decimal_round_trips_exact_decimal_text", tags) {
    auto lg(make_logger(test_suite));

    CHECK(parsed("0.1").to_string() == "0.1");
    CHECK(parsed("0.10").to_string() == "0.1");
    CHECK(parsed("0.0425").to_string() == "0.0425");
    CHECK(parsed("-1.5").to_string() == "-1.5");
    CHECK(parsed("1000").to_string() == "1000");
    CHECK(parsed("0").to_string() == "0");
}

TEST_CASE("decimal_renders_a_small_magnitude_without_an_exponent", tags) {
    auto lg(make_logger(test_suite));

    CHECK(parsed("1e-10").to_string() == "0.0000000001");
    CHECK(parsed("-1e-10").to_string() == "-0.0000000001");
    CHECK(parsed("1.5e-3").to_string() == "0.0015");
    CHECK(parsed("1e3").to_string() == "1000");
}

TEST_CASE("decimal_holds_the_value_a_double_cannot", tags) {
    auto lg(make_logger(test_suite));

    const auto wide = parsed("12345678901234567890.1234567890");
    CHECK(wide.to_string() == "12345678901234567890.123456789");
    CHECK(wide.scale() == 9);
    CHECK(wide > parsed("12345678901234567168"));

    const auto from_binary = decimal::from_double(12345678901234567890.1234567890);
    REQUIRE(from_binary.has_value());
    BOOST_LOG_SEV(lg, info) << "The double names: " << from_binary->to_string();
    CHECK(*from_binary != wide);
}

TEST_CASE("decimal_default_constructs_to_zero", tags) {
    auto lg(make_logger(test_suite));

    const decimal sut;
    CHECK(sut.is_zero());
    CHECK(sut.to_string() == "0");
    CHECK(sut == parsed("0"));
    CHECK(sut == parsed("0.000"));
}

TEST_CASE("decimal_orders_by_its_value", tags) {
    auto lg(make_logger(test_suite));

    CHECK(parsed("0.1") < parsed("0.2"));
    CHECK(parsed("-0.2") < parsed("-0.1"));
    CHECK(parsed("-1e-10") < parsed("0"));
    CHECK(parsed("1e-10") > parsed("0"));
    CHECK(parsed("0.1") <=> parsed("0.10") == std::strong_ordering::equal);
}

TEST_CASE("decimal_scale_follows_the_value", tags) {
    auto lg(make_logger(test_suite));

    CHECK(parsed("1000").scale() == 0);
    CHECK(parsed("0.0425").scale() == 4);
    CHECK(parsed("1e-10").scale() == 10);
    CHECK(parsed("0.1000").scale() == 1);
}

TEST_CASE("decimal_refuses_text_that_is_not_a_decimal", tags) {
    auto lg(make_logger(test_suite));

    for (const auto* text : {"", " ", "abc", "nan", "inf", "-inf", "1.2.3", "1e", "0x10",
                             "1,5", "--1", "1 2", "."}) {
        const auto result = decimal::from_string(text);
        BOOST_LOG_SEV(lg, info) << "Refused [" << text << "]";
        CHECK_FALSE(result.has_value());
    }
}

TEST_CASE("decimal_refuses_a_text_wider_than_it_holds", tags) {
    auto lg(make_logger(test_suite));

    constexpr auto held = std::numeric_limits<boost::multiprecision::cpp_dec_float_50>::digits10;
    static_assert(held == 50);

    std::string fits(held, '9');
    std::string too_wide(held + 1, '9');

    CHECK(decimal::from_string(fits).has_value());

    const auto result = decimal::from_string(too_wide);
    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("significant digits") != std::string::npos);
}

TEST_CASE("decimal_from_double_refuses_a_non_finite_value", tags) {
    auto lg(make_logger(test_suite));

    CHECK_FALSE(decimal::from_double(std::numeric_limits<double>::infinity()).has_value());
    CHECK_FALSE(decimal::from_double(-std::numeric_limits<double>::infinity()).has_value());
    CHECK_FALSE(decimal::from_double(std::numeric_limits<double>::quiet_NaN()).has_value());
}

TEST_CASE("decimal_reaches_a_double_only_through_the_named_boundary", tags) {
    auto lg(make_logger(test_suite));

    CHECK(parsed("775").to_double() == 775.0);
    CHECK(parsed("-0.5").to_double() == -0.5);
    CHECK(decimal::from_double(parsed("0.0425").to_double()).value().to_string() == "0.0425");

    const auto wide = parsed("12345678901234567890.123456789");
    BOOST_LOG_SEV(lg, info) << "Through a double: " << decimal::from_double(wide.to_double())->to_string();
    CHECK(decimal::from_double(wide.to_double()).value() != wide);
}

TEST_CASE("decimal_streams_its_text", tags) {
    auto lg(make_logger(test_suite));

    std::ostringstream ss;
    ss << parsed("0.0425");
    CHECK(ss.str() == "0.0425");
}

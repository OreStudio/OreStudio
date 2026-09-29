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
#include "ores.utility/rfl/reflectors.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <string>

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[rfl][decimal]");

struct holds_amount {
    std::string label;
    ores::utility::decimal::decimal amount;
};

struct maybe_holds_amount {
    std::optional<ores::utility::decimal::decimal> amount;
};

}

using ores::utility::decimal::decimal;
using namespace ores::logging;

TEST_CASE("decimal_reflects_as_its_exact_text", tags) {
    auto lg(make_logger(test_suite));

    const auto value = decimal::from_string("0.1").value();
    CHECK(rfl::Reflector<decimal>::from(value) == "0.1");
    CHECK(rfl::Reflector<decimal>::to("0.1") == value);
    CHECK(rfl::Reflector<decimal>::to("1e-10").to_string() == "0.0000000001");
}

TEST_CASE("decimal_travels_as_a_json_string_not_a_number", tags) {
    auto lg(make_logger(test_suite));

    holds_amount sut{.label = "notional", .amount = decimal::from_string("0.1").value()};
    const auto written = rfl::json::write(sut);
    BOOST_LOG_SEV(lg, info) << "Written: " << written;
    CHECK(written == R"({"label":"notional","amount":"0.1"})");

    const auto read = rfl::json::read<holds_amount>(written);
    REQUIRE(read.has_value());
    CHECK(read->amount == sut.amount);
}

TEST_CASE("decimal_json_read_keeps_a_value_a_double_cannot", tags) {
    auto lg(make_logger(test_suite));

    const auto text = R"({"label":"notional","amount":"12345678901234567890.123456789"})";
    const auto read = rfl::json::read<holds_amount>(text);
    REQUIRE(read.has_value());
    CHECK(read->amount.to_string() == "12345678901234567890.123456789");
}

TEST_CASE("decimal_travels_the_msgpack_wire_exactly", tags) {
    auto lg(make_logger(test_suite));

    const auto sut =
        holds_amount{.label = "notional",
                     .amount = decimal::from_string("12345678901234567890.123456789").value()};

    const auto packed = rfl::msgpack::write(sut);
    const auto read = rfl::msgpack::read<holds_amount>(packed);
    REQUIRE(read.has_value());
    CHECK(read->amount.to_string() == "12345678901234567890.123456789");
}

TEST_CASE("decimal_json_read_refuses_text_that_is_not_a_decimal", tags) {
    auto lg(make_logger(test_suite));

    CHECK_FALSE(
        rfl::json::read<holds_amount>(R"({"label":"notional","amount":"nan"})").has_value());
    CHECK_FALSE(rfl::json::read<holds_amount>(R"({"label":"notional","amount":""})").has_value());
}

TEST_CASE("optional_decimal_travels_as_an_omitted_field_when_absent", tags) {
    auto lg(make_logger(test_suite));

    const auto absent = rfl::json::write(maybe_holds_amount{});
    CHECK(absent == "{}");

    const auto back = rfl::json::read<maybe_holds_amount>(absent);
    REQUIRE(back.has_value());
    CHECK_FALSE(back->amount.has_value());

    const auto present =
        rfl::json::write(maybe_holds_amount{.amount = decimal::from_string("1e-10").value()});
    CHECK(present == R"({"amount":"0.0000000001"})");

    const auto read = rfl::json::read<maybe_holds_amount>(present);
    REQUIRE(read.has_value());
    REQUIRE(read->amount.has_value());
    CHECK(read->amount->to_string() == "0.0000000001");
}

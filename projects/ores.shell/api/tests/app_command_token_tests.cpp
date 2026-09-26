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
#include "ores.platform/time/datetime.hpp"
#include "ores.shell/app/command_token.hpp"
#include <boost/lexical_cast.hpp>
#include <chrono>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <optional>
#include <string>

namespace {

const std::string_view test_suite("ores.shell.tests");
const std::string tags("[app]");

}

using ores::shell::app::from_token;
using namespace ores::logging;

TEST_CASE("from_token_takes_a_string_token_verbatim", tags) {
    auto lg(make_logger(test_suite));

    CHECK(from_token<std::string>("42") == "42");
    CHECK(from_token<std::string>("EUR") == "EUR");
    CHECK(from_token<std::string>("") == "");
}

TEST_CASE("from_token_converts_a_typed_token", tags) {
    auto lg(make_logger(test_suite));

    CHECK(from_token<int>("7") == 7);
    CHECK(from_token<std::int64_t>("-9000") == -9000);
    CHECK(from_token<double>("1100000.5") == 1100000.5);
}

TEST_CASE("from_token_parses_a_uuid_token", tags) {
    auto lg(make_logger(test_suite));

    const auto text = "01234567-89ab-cdef-0123-456789abcdef";
    CHECK(boost::lexical_cast<std::string>(from_token<boost::uuids::uuid>(text)) == text);
}

TEST_CASE("from_token_reads_a_bool_token", tags) {
    auto lg(make_logger(test_suite));

    CHECK(from_token<bool>("true"));
    CHECK(from_token<bool>("1"));
    CHECK_FALSE(from_token<bool>("false"));
}

TEST_CASE("from_token_leaves_an_optional_unset_on_the_absent_token", tags) {
    auto lg(make_logger(test_suite));

    CHECK_FALSE(from_token<std::optional<int>>("-").has_value());
    CHECK_FALSE(from_token<std::optional<std::string>>("-").has_value());
    CHECK_FALSE(from_token<std::optional<boost::uuids::uuid>>("-").has_value());
}

TEST_CASE("from_token_fills_an_optional_from_a_present_token", tags) {
    auto lg(make_logger(test_suite));

    REQUIRE(from_token<std::optional<int>>("7").has_value());
    CHECK(*from_token<std::optional<int>>("7") == 7);
    REQUIRE(from_token<std::optional<std::string>>("Cash").has_value());
    CHECK(*from_token<std::optional<std::string>>("Cash") == "Cash");
}

TEST_CASE("from_token_rejects_a_malformed_typed_token", tags) {
    auto lg(make_logger(test_suite));

    CHECK_THROWS_AS(from_token<int>("not-a-number"), boost::bad_lexical_cast);
    CHECK_THROWS_AS(from_token<boost::uuids::uuid>("not-a-uuid"), boost::bad_lexical_cast);
}

TEST_CASE("from_token_converts_an_instant_token", tags) {
    auto lg(make_logger(test_suite));

    // The storage form is the token a caller types, so a token parses back to
    // the instant it names. The second pair is what stops this passing when
    // from_token returns a constant.
    const auto parsed = from_token<std::chrono::system_clock::time_point>("2026-09-26T14:30:00Z");
    CHECK(ores::platform::time::datetime::to_iso8601_utc(parsed) == "2026-09-26T14:30:00Z");

    const auto other = from_token<std::chrono::system_clock::time_point>("2020-01-01T00:00:00Z");
    CHECK(ores::platform::time::datetime::to_iso8601_utc(other) == "2020-01-01T00:00:00Z");
    CHECK(parsed != other);
}

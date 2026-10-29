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
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>

namespace wire_json = ores::utility::codec::json;

namespace {

/**
 * @brief Encodes and decodes a value through reflect-cpp, which is what the
 * wire carries today and therefore what the new codec has to agree with to the
 * byte.
 */
template <typename T>
T through_rfl_json(const T& v) {
    return rfl::json::read<T>(rfl::json::write(v)).value();
}

};

TEST_CASE("a uuid's JSON is the string reflect-cpp writes", "[codec][json]") {
    const auto id = boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");

    CHECK(wire_json::write(id) == rfl::json::write(id));
    CHECK(wire_json::write(id) == "\"3f2504e0-4f89-11d3-9a0c-0305e82c3301\"");

    CHECK(wire_json::read<boost::uuids::uuid>(rfl::json::write(id)) == id);
    CHECK(through_rfl_json(wire_json::read<boost::uuids::uuid>(wire_json::write(id))) == id);
}

TEST_CASE("a nil uuid keeps the all-zero spelling", "[codec][json]") {
    const boost::uuids::uuid nil{};

    CHECK(wire_json::write(nil) == rfl::json::write(nil));
    CHECK(wire_json::write(nil) == "\"00000000-0000-0000-0000-000000000000\"");
    CHECK(wire_json::read<boost::uuids::uuid>(wire_json::write(nil)) == nil);
}

TEST_CASE("an absent uuid is the format's null", "[codec][json]") {
    const std::optional<boost::uuids::uuid> absent;
    const auto id =
        std::optional(boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"));

    CHECK(wire_json::write(absent) == rfl::json::write(absent));
    CHECK(wire_json::write(absent) == "null");
    CHECK(wire_json::write(id) == rfl::json::write(id));
    CHECK(wire_json::read<std::optional<boost::uuids::uuid>>(wire_json::write(absent)) == absent);
    CHECK(wire_json::read<std::optional<boost::uuids::uuid>>(wire_json::write(id)) == id);
}

TEST_CASE("an array of uuids is an array of the same strings", "[codec][json]") {
    const std::vector<boost::uuids::uuid> ids{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        boost::uuids::string_generator{}("6ba7b810-9dad-11d1-80b4-00c04fd430c8")};

    CHECK(wire_json::write(ids) == rfl::json::write(ids));
    CHECK(wire_json::read<std::vector<boost::uuids::uuid>>(wire_json::write(ids)) == ids);
}

TEST_CASE("a tenant id, a date and a decimal are the strings reflect-cpp writes", "[codec][json]") {
    const auto tenant =
        *ores::utility::uuid::tenant_id::from_string("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
    const auto date = std::chrono::year{2025} / std::chrono::January / 15;
    const auto absent_date = std::chrono::year_month_day{};
    const auto amount = *ores::utility::decimal::decimal::from_string("-1234.567890123456");

    CHECK(wire_json::write(tenant) == rfl::json::write(tenant));
    CHECK(wire_json::write(date) == rfl::json::write(date));
    CHECK(wire_json::write(absent_date) == rfl::json::write(absent_date));
    CHECK(wire_json::write(amount) == rfl::json::write(amount));

    CHECK(wire_json::read<ores::utility::uuid::tenant_id>(wire_json::write(tenant)) == tenant);
    CHECK(wire_json::read<std::chrono::year_month_day>(wire_json::write(date)) == date);
    CHECK(wire_json::read<std::chrono::year_month_day>(wire_json::write(absent_date)) ==
          absent_date);
    CHECK(wire_json::read<ores::utility::decimal::decimal>(wire_json::write(amount)) == amount);
}

TEST_CASE("a string is its own JSON form", "[codec][json]") {
    CHECK(wire_json::write(std::string("zürich's \"quote\"")) ==
          rfl::json::write(std::string("zürich's \"quote\"")));
    CHECK(wire_json::read<std::string>(wire_json::write(std::string("zürich's \"quote\""))) ==
          "zürich's \"quote\"");
}

TEST_CASE("a value of another shape is refused with what it should have been", "[codec][json]") {
    CHECK_THROWS_AS(wire_json::read<boost::uuids::uuid>("4"), std::invalid_argument);
    CHECK_THROWS_AS(wire_json::read<std::optional<boost::uuids::uuid>>("\"not a uuid\""),
                    std::invalid_argument);
}

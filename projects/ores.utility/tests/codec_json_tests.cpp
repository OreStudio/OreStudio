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

namespace wire = ores::utility::codec::json;

namespace {

/**
 * @brief Encodes and decodes a value through reflect-cpp, which is what the
 * wire carries today and therefore what the new codec has to agree with to the
 * byte.
 */
template <typename T>
T through_rfl(const T& v) {
    return rfl::json::read<T>(rfl::json::write(v)).value();
}

};

TEST_CASE("a uuid's JSON is the string reflect-cpp writes", "[codec][json]") {
    const auto id = boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");

    CHECK(wire::write(id) == rfl::json::write(id));
    CHECK(wire::write(id) == "\"3f2504e0-4f89-11d3-9a0c-0305e82c3301\"");

    CHECK(wire::read<boost::uuids::uuid>(rfl::json::write(id)) == id);
    CHECK(through_rfl(wire::read<boost::uuids::uuid>(wire::write(id))) == id);
}

TEST_CASE("a nil uuid keeps the all-zero spelling", "[codec][json]") {
    const boost::uuids::uuid nil{};

    CHECK(wire::write(nil) == rfl::json::write(nil));
    CHECK(wire::write(nil) == "\"00000000-0000-0000-0000-000000000000\"");
    CHECK(wire::read<boost::uuids::uuid>(wire::write(nil)) == nil);
}

TEST_CASE("an absent uuid is the format's null", "[codec][json]") {
    const std::optional<boost::uuids::uuid> absent;
    const auto id =
        std::optional(boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"));

    CHECK(wire::write(absent) == rfl::json::write(absent));
    CHECK(wire::write(absent) == "null");
    CHECK(wire::write(id) == rfl::json::write(id));
    CHECK(wire::read<std::optional<boost::uuids::uuid>>(wire::write(absent)) == absent);
    CHECK(wire::read<std::optional<boost::uuids::uuid>>(wire::write(id)) == id);
}

TEST_CASE("an array of uuids is an array of the same strings", "[codec][json]") {
    const std::vector<boost::uuids::uuid> ids{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        boost::uuids::string_generator{}("6ba7b810-9dad-11d1-80b4-00c04fd430c8")};

    CHECK(wire::write(ids) == rfl::json::write(ids));
    CHECK(wire::read<std::vector<boost::uuids::uuid>>(wire::write(ids)) == ids);
}

TEST_CASE("a string is its own JSON form", "[codec][json]") {
    CHECK(wire::write(std::string("zürich's \"quote\"")) ==
          rfl::json::write(std::string("zürich's \"quote\"")));
    CHECK(wire::read<std::string>(wire::write(std::string("zürich's \"quote\""))) ==
          "zürich's \"quote\"");
}

TEST_CASE("a value of another shape is refused with what it should have been", "[codec][json]") {
    CHECK_THROWS_AS(wire::read<boost::uuids::uuid>("4"), std::invalid_argument);
    CHECK_THROWS_AS(wire::read<std::optional<boost::uuids::uuid>>("\"not a uuid\""),
                    std::invalid_argument);
}

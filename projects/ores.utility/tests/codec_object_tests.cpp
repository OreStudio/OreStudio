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
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>

namespace {

/**
 * @brief The shape a generated type states: its members, in order, named as
 * the wire names them.
 */
struct probe {
    boost::uuids::uuid id;
    std::string label;
    std::optional<std::chrono::year_month_day> when;
};

inline const auto probe_fields =
    std::make_tuple(ores::utility::codec::field{"id", &probe::id},
                    ores::utility::codec::field{"label", &probe::label},
                    ores::utility::codec::field{"when", &probe::when});

}

TEST_CASE("an object's JSON is the one reflect-cpp writes", "[codec][object]") {
    const auto v = probe{boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
                         "alpha",
                         std::chrono::year{2025} / std::chrono::January / 15};

    const auto mine = ores::utility::codec::object::write_json(v, probe_fields);
    const auto theirs =
        R"({"id":"3f2504e0-4f89-11d3-9a0c-0305e82c3301","label":"alpha","when":"2025-01-15"})";
    CHECK(mine == theirs);
    const auto back = ores::utility::codec::object::read_json(mine, probe_fields);
    CHECK(back.id == v.id);
    CHECK(back.label == v.label);
    CHECK(back.when == v.when);
}

TEST_CASE("an object with an absent member writes the format's null", "[codec][object]") {
    const auto v = probe{boost::uuids::string_generator{}("00000000-0000-0000-0000-000000000000"),
                         "beta",
                         std::nullopt};

    const auto mine = ores::utility::codec::object::write_json(v, probe_fields);
    CHECK(mine == R"({"id":"00000000-0000-0000-0000-000000000000","label":"beta","when":null})");
    CHECK_FALSE(ores::utility::codec::object::read_json(mine, probe_fields).when.has_value());
}

TEST_CASE("an object's MessagePack round-trips", "[codec][object]") {
    const auto v = probe{boost::uuids::string_generator{}("6ba7b810-9dad-11d1-80b4-00c04fd430c8"),
                         "gamma",
                         std::chrono::year{2024} / std::chrono::December / 31};

    const auto bytes = ores::utility::codec::object::write_msgpack(v, probe_fields);
    const auto back = ores::utility::codec::object::read_msgpack(
        std::span(reinterpret_cast<const std::byte*>(bytes.data()), bytes.size()), probe_fields);
    CHECK(back.id == v.id);
    CHECK(back.label == v.label);
    CHECK(back.when == v.when);
}

TEST_CASE("a member the object does not carry is refused", "[codec][object]") {
    CHECK_THROWS_AS(ores::utility::codec::object::read_json(R"({"id":"x"})", probe_fields),
                    std::invalid_argument);
}

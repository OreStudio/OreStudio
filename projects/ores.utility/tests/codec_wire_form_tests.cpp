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
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

/**
 * @brief The probe types live in a named namespace with external linkage,
 * because reflect-cpp cannot reflect a type it cannot name in another
 * translation unit, and a unity build shares one anonymous namespace across
 * every file it joins.
 */
namespace wire_form_probe {

using ores::utility::codec::field;

/**
 * @brief A type that states its own shape, the way a generated one does.
 */
struct inner {
    int count = 0;
    std::string label;
};

/**
 * @brief States the shape. The argument names the type; the shape does not
 * depend on a value, so it carries no name and draws no unused-parameter
 * warning.
 */
inline constexpr auto wire_fields(const inner&) {
    return std::tuple{field{"count", &inner::count}, field{"label", &inner::label}};
}

/**
 * @brief A type whose members are the shapes a message actually holds: another
 * type with its own shape, a list of them, and a value that may be absent.
 */
struct outer {
    std::string name;
    inner one;
    std::vector<inner> many;
    std::optional<inner> maybe;
    std::optional<std::string> note;
};

inline constexpr auto wire_fields(const outer&) {
    return std::tuple{field{"name", &outer::name},
                      field{"one", &outer::one},
                      field{"many", &outer::many},
                      field{"maybe", &outer::maybe},
                      field{"note", &outer::note}};
}

}

namespace {

/**
 * @brief The bytes reflect-cpp writes for @p v.
 */
template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::string_view text_of(const std::string& s) {
    return {s.data(), s.size()};
}

}

TEST_CASE("a type that states its shape is an object, as reflect-cpp writes it",
          "[codec][object]") {
    const auto v = wire_form_probe::outer{
        "alpha", wire_form_probe::inner{7, "beta"}, {}, std::nullopt, std::nullopt};

    CHECK(ores::utility::codec::json::write(v) == rfl::json::write(v));
    CHECK(ores::utility::codec::msgpack::write(v) == their_msgpack(v));
}

TEST_CASE("a member that is itself a type with a shape recurses", "[codec][object]") {
    using wire_form_probe::inner;
    using wire_form_probe::outer;

    const auto v = outer{
        "alpha", inner{7, "beta"}, {inner{1, "one"}, inner{2, "two"}}, inner{3, "three"}, "a note"};

    const auto json = ores::utility::codec::json::write(v);
    CHECK(json == rfl::json::write(v));
    CHECK(json.find("\"one\":{\"count\":7,\"label\":\"beta\"}") != std::string::npos);
    CHECK(json.find("\"many\":[{\"count\":1,\"label\":\"one\"},") != std::string::npos);
    CHECK(json.find("\"maybe\":{\"count\":3,\"label\":\"three\"}") != std::string::npos);

    const auto back = ores::utility::codec::json::read<outer>(json);
    CHECK(back.name == v.name);
    CHECK(back.one.label == v.one.label);
    CHECK(back.many.size() == v.many.size());
    CHECK(back.maybe.has_value());
    CHECK(back.maybe->label == v.maybe->label);

    CHECK(ores::utility::codec::msgpack::write(v) == their_msgpack(v));
    const auto back_msgpack = ores::utility::codec::msgpack::read<outer>(
        text_of(ores::utility::codec::msgpack::write(v)));
    CHECK(back_msgpack.one.count == v.one.count);
    CHECK(back_msgpack.many.size() == v.many.size());
    CHECK(back_msgpack.note == v.note);
}

TEST_CASE("a nested member that is absent is omitted from JSON and nil in MessagePack",
          "[codec][object]") {
    using wire_form_probe::inner;
    using wire_form_probe::outer;

    const auto absent = outer{"alpha", inner{}, {}, std::nullopt, std::nullopt};
    const auto present = outer{"alpha", inner{}, {}, inner{9, "nine"}, std::nullopt};

    CHECK(ores::utility::codec::json::write(absent) == rfl::json::write(absent));
    CHECK(ores::utility::codec::json::write(present) == rfl::json::write(present));
    CHECK(ores::utility::codec::msgpack::write(absent) == their_msgpack(absent));
    CHECK(ores::utility::codec::msgpack::write(present) == their_msgpack(present));
}

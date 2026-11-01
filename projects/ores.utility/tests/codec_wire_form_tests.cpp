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
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <filesystem>
#include <map>
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

/**
 * @brief A type whose members map names to values, as an operation message's
 * version table does.
 */
struct tally {
    std::map<std::string, int> counts;
    std::map<std::string, inner> parts;
};

inline constexpr auto wire_fields(const tally&) {
    return std::tuple{field{"counts", &tally::counts}, field{"parts", &tally::parts}};
}

/**
 * @brief A type with a duration, as a service's flush interval is.
 */
struct timed {
    std::chrono::seconds interval{5};
    std::chrono::milliseconds pause{250};
};

inline constexpr auto wire_fields(const timed&) {
    return std::tuple{field{"interval", &timed::interval}, field{"pause", &timed::pause}};
}

/**
 * @brief A type with a path, as a service's working directory is.
 */
struct located {
    std::filesystem::path dir;
};

inline constexpr auto wire_fields(const located&) {
    return std::tuple{field{"dir", &located::dir}};
}

/**
 * @brief A type whose shape leaves a member out, as a hand-written shape does
 * when a member is added to the struct and not to the shape.
 */
struct forgetful {
    std::string user;
    std::string password;
    int port = 0;
};

inline constexpr auto wire_fields(const forgetful&) {
    return std::tuple{field{"user", &forgetful::user}, field{"port", &forgetful::port}};
}

}

namespace {

/**
 * @brief A type in an anonymous namespace whose member names another, as a
 * translation unit's private reply types do.
 */
struct local_part {
    std::string name;
};

struct local_reply {
    std::optional<std::vector<local_part>> parts;
    int count = 0;
};

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
    CHECK(json.contains("\"one\":{\"count\":7,\"label\":\"beta\"}"));
    CHECK(json.contains("\"many\":[{\"count\":1,\"label\":\"one\"},"));
    CHECK(json.contains("\"maybe\":{\"count\":3,\"label\":\"three\"}"));

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

TEST_CASE("a map from names to values is written as reflect-cpp writes it and reads back",
          "[codec][object]") {
    using wire_form_probe::inner;
    using wire_form_probe::tally;

    const auto full = tally{{{"b", 2}, {"a", 1}}, {{"x", inner{9, "nine"}}}};
    const auto empty = tally{};

    for (const auto& v : {full, empty}) {
        const auto json = ores::utility::codec::json::write(v);
        CHECK(json == rfl::json::write(v));
        CHECK(ores::utility::codec::msgpack::write(v) == their_msgpack(v));

        const auto back = ores::utility::codec::json::read<tally>(json);
        CHECK(back.counts == v.counts);
        CHECK(back.parts.size() == v.parts.size());

        const auto back_msgpack = ores::utility::codec::msgpack::read<tally>(
            text_of(ores::utility::codec::msgpack::write(v)));
        CHECK(back_msgpack.counts == v.counts);
    }
    CHECK(ores::utility::codec::json::write(full).contains("\"counts\":{\"a\":1,\"b\":2}"));
}

TEST_CASE("a duration is written as reflect-cpp writes it and reads back", "[codec][object]") {
    using wire_form_probe::timed;

    const auto v = timed{};
    const auto json = ores::utility::codec::json::write(v);
    CHECK(json == rfl::json::write(v));
    CHECK(json.contains("\"interval\":{\"count\":5,\"unit\":\"seconds\"}"));
    CHECK(ores::utility::codec::msgpack::write(v) == their_msgpack(v));

    const auto back = ores::utility::codec::json::read<timed>(json);
    CHECK(back.interval == v.interval);
    CHECK(back.pause == v.pause);
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

TEST_CASE("a shape that leaves a member out is told apart from one that names them all",
          "[codec][object]") {
    using namespace wire_form_probe;
    using ores::utility::codec::member_count;
    using ores::utility::codec::names_every_member;

    STATIC_REQUIRE(member_count<inner> == 2);
    STATIC_REQUIRE(member_count<outer> == 5);
    STATIC_REQUIRE(member_count<timed> == 2);
    STATIC_REQUIRE(names_every_member<outer>());
    STATIC_REQUIRE(names_every_member<tally>());
    STATIC_REQUIRE_FALSE(names_every_member<forgetful>());
    STATIC_REQUIRE(names_every_member<forgetful>(1));
}

TEST_CASE("a path is written as reflect-cpp writes it and reads back", "[codec][object]") {
    using wire_form_probe::located;

    const auto v = located{"/var/ores/work dir"};
    const auto json = ores::utility::codec::json::write(v);

    CHECK(json == rfl::json::write(v));
    CHECK(ores::utility::codec::msgpack::write(v) == their_msgpack(v));
    CHECK(ores::utility::codec::json::read<located>(json).dir == v.dir);
}

TEST_CASE("a type in an anonymous namespace is counted like any other", "[codec][object]") {
    STATIC_REQUIRE(ores::utility::codec::member_count<local_reply> == 2);
}

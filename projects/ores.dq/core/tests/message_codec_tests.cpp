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
#include "ores.dq.api/domain/catalog_codec.hpp"
#include "ores.dq.api/generators/catalog_generator.hpp"
#include "ores.dq.api/messaging/catalog_protocol.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <string>

namespace {

using ores::utility::generation::generation_context;

/**
 * @brief The bytes reflect-cpp writes for @p v.
 */
template <typename T>
std::string their_message_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> message_bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief Asserts that one message's codec writes what reflect-cpp writes, in
 * both formats, and reads those bytes back to the same bytes.
 *
 * One line per message, the same shape the domain types' case uses: the list of
 * lines below is the list of messages this component proved.
 */
template <typename T>
void same_message_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;

    const auto json = codec::write_json(v);
    CHECK(json == rfl::json::write(v));
    // A message's reader is proved by writing back what it read: the messages
    // have no value equality, and what survives a round trip is the wire's own
    // resolution rather than the value the encoding started from.
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_message_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(message_bytes_of(msgpack))) == msgpack);

    // The protocol header states the specialisation, so a translation unit that
    // only saw the messages resolves the seam to the generated code.
    CHECK(codec::is_generated);
}

/**
 * @brief A catalog the response below carries, so the case proves a message
 * that holds a domain type recurses into that type's codec.
 */
ores::dq::domain::catalog a_message_catalog() {
    generation_context ctx(20261030);
    return ores::dq::generators::generate_synthetic_catalog(ctx);
}

}

TEST_CASE("every generated message codec writes what reflect-cpp writes", "[codec][message]") {
    using namespace ores::dq::messaging;

    same_message_bytes_as_reflect_cpp(catalog_key{"ISO Standards"});

    same_message_bytes_as_reflect_cpp(catalog_write{"ISO Standards", "ISO datasets", std::nullopt});

    same_message_bytes_as_reflect_cpp(
        catalog_change{catalog_write{"ISO Standards", "ISO datasets", "Standards Team"},
                       ores::utility::domain::precondition{
                           ores::utility::domain::precondition_kind::any, std::nullopt}});

    same_message_bytes_as_reflect_cpp(
        catalog_removal{catalog_key{"ISO Standards"}, ores::utility::domain::removal_precondition});

    same_message_bytes_as_reflect_cpp(
        catalog_lookup{catalog_key{"ISO Standards"}, a_message_catalog()});

    same_message_bytes_as_reflect_cpp(catalogs_filter{std::vector<std::string>{"ISO Standards"}});

    same_message_bytes_as_reflect_cpp(catalog_event{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        catalog_key{"ISO Standards"},
        "written",
        7,
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456)),
        std::nullopt});

    same_message_bytes_as_reflect_cpp(catalog_versions_filter{std::nullopt, 1, std::nullopt});

    same_message_bytes_as_reflect_cpp(
        list_catalogs_request{0,
                              100,
                              ores::utility::domain::order{"name", false},
                              catalogs_filter{std::nullopt},
                              std::nullopt});

    same_message_bytes_as_reflect_cpp(list_catalogs_response{
        ores::utility::domain::result{ores::utility::domain::outcome::ok, "", "", {}},
        {a_message_catalog()},
        1});

    same_message_bytes_as_reflect_cpp(get_catalog_response{
        ores::utility::domain::result{
            ores::utility::domain::outcome::denied, "denied", "not allowed", {}},
        std::nullopt});
}

TEST_CASE("the wire codec sends a dq message through its generated codec",
          "[codec][message][wire_codec]") {
    using namespace ores::dq::messaging;

    const auto v = catalog_event{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        catalog_key{"ISO Standards"},
        "written",
        7,
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456)),
        std::nullopt};

    using codec = ores::utility::codec::wire_traits<catalog_event>;

    for (const auto format : {ores::nats::wire_format::json, ores::nats::wire_format::msgpack}) {
        const ores::nats::wire_codec nats_codec(format);
        const auto bytes = nats_codec.encode(v);
        const std::string_view sent(reinterpret_cast<const char*>(bytes.data()), bytes.size());

        const auto generated = format == ores::nats::wire_format::json ? codec::write_json(v) :
                                                                         codec::write_msgpack(v);
        CHECK(sent == generated);

        const auto read = nats_codec.decode<catalog_event>(bytes);
        REQUIRE(read);
        CHECK(codec::write_json(*read) == codec::write_json(v));
    }
}

TEST_CASE("a message and the domain type it holds agree about the domain type",
          "[codec][message]") {
    using namespace ores::dq::messaging;

    const auto catalog = a_message_catalog();
    const auto response = list_catalogs_response{
        ores::utility::domain::result{ores::utility::domain::outcome::ok, "", "", {}},
        {catalog},
        1};

    const auto json =
        ores::utility::codec::wire_traits<list_catalogs_response>::write_json(response);

    // The message carries the domain type's own object, written by that type's
    // codec, so the two cannot drift.
    const auto catalog_json =
        ores::utility::codec::wire_traits<ores::dq::domain::catalog>::write_json(catalog);
    CHECK(json.contains(catalog_json));

    const auto back = ores::utility::codec::wire_traits<list_catalogs_response>::read_json(json);
    REQUIRE(back.catalogs.size() == 1);
    CHECK(back.catalogs.front().name == catalog.name);
    CHECK(back.total == response.total);
    CHECK(back.result.outcome == ores::utility::domain::outcome::ok);
}

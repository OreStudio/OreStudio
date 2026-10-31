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
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.refdata.api/domain/convention_identity.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/domain/currency_codec.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_traits.hpp"
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

/**
 * @brief The bytes reflect-cpp writes for @p v.
 */
template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> message_bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief Asserts that one type's codec writes what reflect-cpp writes, in both
 * formats, and reads those bytes back to the same bytes.
 *
 * The generated codec is the one the trait resolves to, so this compares the
 * bytes this component now sends against the bytes it sent before the move.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;

    const auto json = codec::write_json(v);
    CHECK(json == rfl::json::write(v));
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(message_bytes_of(msgpack))) == msgpack);

    // The specialisation is the generated one, not the reflector-backed
    // primary template: a translation unit that includes the codec header
    // resolves the seam to generated code.
    CHECK(codec::is_generated);
}

ores::refdata::domain::currency a_currency() {
    ores::refdata::domain::currency c;
    c.version = 7;
    c.iso_code = "GBP";
    c.name = "Pound Sterling";
    c.numeric_code = "826";
    c.symbol = "£";
    c.fraction_symbol = "p";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;
    c.format = "###,###,###.##";
    c.monetary_nature = "Fiat";
    c.market_tier = "G10";
    c.spot_days = 2;
    c.day_basis = "ACT/365";
    c.base_precedence = 1;
    c.modified_by = "admin";
    c.performed_by = "admin";
    c.change_reason_code = "system.test";
    c.change_commentary = "Synthetic test data";
    c.recorded_at =
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456));
    return c;
}

ores::refdata::messaging::currency_write a_currency_write() {
    const auto c = a_currency();
    return ores::refdata::messaging::currency_write{c.iso_code,
                                                    c.name,
                                                    c.numeric_code,
                                                    c.symbol,
                                                    c.fraction_symbol,
                                                    c.fractions_per_unit,
                                                    c.rounding_type,
                                                    c.rounding_precision,
                                                    c.format,
                                                    c.monetary_nature,
                                                    c.market_tier,
                                                    c.ore_currency_type,
                                                    c.image_id,
                                                    c.spot_days,
                                                    c.day_basis,
                                                    c.base_precedence};
}

}

TEST_CASE("a refdata row codec writes what reflect-cpp writes", "[codec][refdata]") {
    same_bytes_as_reflect_cpp(a_currency());
}

TEST_CASE("every generated refdata message codec writes what reflect-cpp writes",
          "[codec][refdata][message]") {
    using namespace ores::refdata::messaging;

    same_bytes_as_reflect_cpp(currency_key{"GBP"});

    // One write with both optionals stated and one with neither, because the
    // two formats disagree about an absent value: JSON omits the member and
    // MessagePack writes nil.
    same_bytes_as_reflect_cpp(a_currency_write());

    auto bare = a_currency_write();
    bare.ore_currency_type = "Fiat";
    bare.image_id = boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
    same_bytes_as_reflect_cpp(bare);

    same_bytes_as_reflect_cpp(currency_removal{currency_key{"GBP"}});

    same_bytes_as_reflect_cpp(currency_event{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        currency_key{"GBP"},
        "written",
        7,
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456)),
        std::nullopt});

    // A response holding the row proves the message recurses into the row's
    // codec rather than into a shape of its own.
    same_bytes_as_reflect_cpp(get_currency_response{
        ores::utility::domain::result{ores::utility::domain::outcome::ok, "", "", {}},
        a_currency()});

    same_bytes_as_reflect_cpp(list_currencies_response{
        ores::utility::domain::result{ores::utility::domain::outcome::ok, "", "", {}},
        {a_currency()},
        1});

    same_bytes_as_reflect_cpp(
        currencies_filter{std::vector<std::string>{"GBP", "EUR"}, std::nullopt});

    same_bytes_as_reflect_cpp(
        list_currencies_request{0,
                                100,
                                ores::utility::domain::order{"iso_code", false},
                                currencies_filter{std::nullopt, std::nullopt},
                                std::nullopt});
}

TEST_CASE("a refdata field group states its shape, so its holder recurses", "[codec][refdata]") {
    auto group = ores::refdata::domain::convention_identity{};
    group.id = boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
    group.party_id = boost::uuids::string_generator{}("5c9d4a3b-2e1f-4a6b-8c7d-9e0f1a2b3c4d");
    group.kind_code = "OIS";

    CHECK(ores::utility::codec::json::write(group) == rfl::json::write(group));
    CHECK(ores::utility::codec::msgpack::write(group) == their_msgpack(group));
}

TEST_CASE("the wire codec sends a refdata message through its generated codec",
          "[codec][refdata][wire_codec]") {
    using namespace ores::refdata::messaging;

    const auto v = currency_event{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        currency_key{"GBP"},
        "written",
        7,
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456)),
        std::nullopt};

    using codec = ores::utility::codec::wire_traits<currency_event>;

    for (const auto format : {ores::nats::wire_format::json, ores::nats::wire_format::msgpack}) {
        const ores::nats::wire_codec nats_codec(format);
        const auto bytes = nats_codec.encode(v);
        const std::string_view sent(reinterpret_cast<const char*>(bytes.data()), bytes.size());

        const auto generated = format == ores::nats::wire_format::json ? codec::write_json(v) :
                                                                         codec::write_msgpack(v);
        CHECK(sent == generated);

        const auto read = nats_codec.decode<currency_event>(bytes);
        REQUIRE(read);
        CHECK(codec::write_json(*read) == codec::write_json(v));
    }
}

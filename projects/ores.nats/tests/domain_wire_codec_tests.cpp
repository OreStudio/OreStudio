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
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <string>
#include <vector>

namespace {

const std::string tags("[domain][wire_codec]");

struct sample {
    std::string name;
    int value = 0;
    std::vector<std::uint8_t> payload;
};

}

TEST_CASE("wire_codec round-trips a reflectable struct under json", tags) {
    const ores::nats::wire_codec codec(ores::nats::wire_format::json);

    const sample original{"alpha", 42, {std::uint8_t{1}, std::uint8_t{2}, std::uint8_t{3}}};
    const auto bytes = codec.encode(original);
    const auto decoded = codec.decode<sample>(bytes);

    REQUIRE(decoded.has_value());
    CHECK(decoded->name == original.name);
    CHECK(decoded->value == original.value);
    CHECK(decoded->payload == original.payload);
}

TEST_CASE("wire_codec round-trips a reflectable struct under msgpack", tags) {
    const ores::nats::wire_codec codec(ores::nats::wire_format::msgpack);

    const sample original{"beta", -7, {std::uint8_t{9}, std::uint8_t{8}, std::uint8_t{7}}};
    const auto bytes = codec.encode(original);
    const auto decoded = codec.decode<sample>(bytes);

    REQUIRE(decoded.has_value());
    CHECK(decoded->name == original.name);
    CHECK(decoded->value == original.value);
    CHECK(decoded->payload == original.payload);
}

TEST_CASE("wire_codec msgpack round-trips a byte-vector field natively, not as base64", tags) {
    // This is the property the whole story exists to exploit: msgpack's
    // native binary type carries std::vector<uint8_t> as raw bytes, not
    // a base64-inflated string -- so the msgpack encoding of a
    // byte-heavy payload should not be larger than its json equivalent.
    const ores::nats::wire_codec json_codec(ores::nats::wire_format::json);
    const ores::nats::wire_codec msgpack_codec(ores::nats::wire_format::msgpack);

    sample original;
    original.name = "gamma";
    original.value = 1;
    original.payload.assign(1000, std::uint8_t{0xAB});

    const auto json_bytes = json_codec.encode(original);
    const auto msgpack_bytes = msgpack_codec.encode(original);

    CHECK(msgpack_bytes.size() < json_bytes.size());

    const auto decoded = msgpack_codec.decode<sample>(msgpack_bytes);
    REQUIRE(decoded.has_value());
    CHECK(decoded->payload == original.payload);
}

TEST_CASE("wire_codec::format returns the format fixed at construction", tags) {
    CHECK(ores::nats::wire_codec(ores::nats::wire_format::json).format() ==
          ores::nats::wire_format::json);
    CHECK(ores::nats::wire_codec(ores::nats::wire_format::msgpack).format() ==
          ores::nats::wire_format::msgpack);
}

TEST_CASE("wire_codec decode surfaces a parse error for malformed json input", tags) {
    const ores::nats::wire_codec codec(ores::nats::wire_format::json);
    const std::string garbage = "not json";
    const std::span<const std::byte> bytes(reinterpret_cast<const std::byte*>(garbage.data()),
                                           garbage.size());

    const auto decoded = codec.decode<sample>(bytes);

    CHECK_FALSE(decoded.has_value());
}

TEST_CASE("wire_codec decode surfaces a parse error for malformed msgpack input", tags) {
    const ores::nats::wire_codec codec(ores::nats::wire_format::msgpack);
    const std::string garbage = "not msgpack";
    const std::span<const std::byte> bytes(reinterpret_cast<const std::byte*>(garbage.data()),
                                           garbage.size());

    const auto decoded = codec.decode<sample>(bytes);

    CHECK_FALSE(decoded.has_value());
}

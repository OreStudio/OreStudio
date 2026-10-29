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
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <string>

namespace {

/**
 * @brief A message with no generated codec, which is every message today.
 */
struct plain_message {
    std::string name;
    int count = 0;
};

/**
 * @brief A message whose codec stands in for a generated one: it writes the
 * same shape but states its own text for the name, which is what a specialisation
 * has to be able to do.
 */
struct generated_message {
    std::string name;
    int count = 0;
};

}

namespace ores::utility::codec {

template <>
struct wire_traits<generated_message> {
    static std::string write_json(const generated_message& v) {
        return "{\"name\":\"" + v.name + "\",\"count\":" + std::to_string(v.count) + "}";
    }

    static generated_message read_json(std::string_view text) {
        return rfl::json::read<generated_message>(std::string(text)).value();
    }

    static std::string write_msgpack(const generated_message& v) {
        const auto bytes = rfl::msgpack::write(v);
        return std::string(bytes.begin(), bytes.end());
    }

    static generated_message read_msgpack(std::span<const std::byte> data) {
        return rfl::msgpack::read<generated_message>(data).value();
    }
};

}

TEST_CASE("a message with no generated codec keeps reflect-cpp", "[wire_codec]") {
    const plain_message v{"alpha", 7};
    const ores::nats::wire_codec codec(ores::nats::wire_format::json);

    const auto bytes = codec.encode(v);
    const auto text = std::string(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    CHECK(text == rfl::json::write(v));
    CHECK(codec.decode<plain_message>(bytes).value().count == v.count);
}

TEST_CASE("a message with a generated codec is written by it", "[wire_codec]") {
    const generated_message v{"beta", 3};
    const ores::nats::wire_codec codec(ores::nats::wire_format::json);

    const auto bytes = codec.encode(v);
    const auto text = std::string(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    CHECK(text == "{\"name\":\"beta\",\"count\":3}");
    CHECK(codec.decode<generated_message>(bytes).value().name == v.name);
}

TEST_CASE("a MessagePack message with a generated codec is written by it", "[wire_codec]") {
    const generated_message v{"gamma", 9};
    const ores::nats::wire_codec codec(ores::nats::wire_format::msgpack);

    const auto bytes = codec.encode(v);
    CHECK(bytes == codec.encode(v));
    CHECK(codec.decode<generated_message>(bytes).value().count == v.count);
}

TEST_CASE("a message that cannot be read is an error, not a throw", "[wire_codec]") {
    const ores::nats::wire_codec codec(ores::nats::wire_format::json);
    const std::string not_json = "not json";
    const auto bytes = std::span(reinterpret_cast<const std::byte*>(not_json.data()),
                                 not_json.size());

    const auto read = codec.decode<plain_message>(bytes);
    CHECK_FALSE(read);
}

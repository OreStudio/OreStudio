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
#include "ores.utility/codec/msgpack_codec.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <rfl/msgpack.hpp>

namespace wire_msgpack = ores::utility::codec::msgpack;

namespace {

/**
 * @brief Encodes and decodes a value through reflect-cpp, which is what the
 * wire carries today and therefore what the new codec has to agree with to the
 * byte.
 */
template <typename T>
std::string rfl_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

template <typename T>
T through_rfl_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return rfl::msgpack::read<T>(bytes).value();
}

};

TEST_CASE("a uuid's MessagePack is the string reflect-cpp writes", "[codec][msgpack]") {
    const auto id = boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
    const auto theirs = rfl_msgpack(id);

    CHECK(wire_msgpack::write(id) == theirs);
    CHECK(wire_msgpack::read<boost::uuids::uuid>(theirs) == id);
    CHECK(through_rfl_msgpack(wire_msgpack::read<boost::uuids::uuid>(wire_msgpack::write(id))) ==
          id);
}

TEST_CASE("a nil uuid keeps the all-zero spelling", "[codec][msgpack]") {
    const boost::uuids::uuid nil{};
    const auto theirs = rfl_msgpack(nil);

    CHECK(wire_msgpack::write(nil) == theirs);
    CHECK(wire_msgpack::read<boost::uuids::uuid>(wire_msgpack::write(nil)) == nil);
}

TEST_CASE("an absent uuid is the format's nil", "[codec][msgpack]") {
    const std::optional<boost::uuids::uuid> absent;
    const auto id =
        std::optional(boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"));

    CHECK(wire_msgpack::read<std::optional<boost::uuids::uuid>>(wire_msgpack::write(absent)) ==
          absent);
    CHECK(wire_msgpack::read<std::optional<boost::uuids::uuid>>(wire_msgpack::write(id)) == id);
    CHECK(wire_msgpack::read<std::optional<boost::uuids::uuid>>(rfl_msgpack(absent)) == absent);
    CHECK(wire_msgpack::read<std::optional<boost::uuids::uuid>>(rfl_msgpack(id)) == id);
}

TEST_CASE("an array of uuids is an array of the same strings", "[codec][msgpack]") {
    const std::vector<boost::uuids::uuid> ids{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        boost::uuids::string_generator{}("6ba7b810-9dad-11d1-80b4-00c04fd430c8")};

    CHECK(wire_msgpack::read<std::vector<boost::uuids::uuid>>(wire_msgpack::write(ids)) == ids);
    CHECK(wire_msgpack::read<std::vector<boost::uuids::uuid>>(rfl_msgpack(ids)) == ids);
}

TEST_CASE("a tenant id, a date and a decimal are the strings reflect-cpp writes",
          "[codec][msgpack]") {
    const auto tenant =
        *ores::utility::uuid::tenant_id::from_string("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
    const auto date = std::chrono::year{2025} / std::chrono::January / 15;
    const auto absent_date = std::chrono::year_month_day{};
    const auto amount = *ores::utility::decimal::decimal::from_string("-1234.567890123456");

    CHECK(wire_msgpack::write(tenant) == rfl_msgpack(tenant));
    CHECK(wire_msgpack::write(date) == rfl_msgpack(date));
    CHECK(wire_msgpack::write(absent_date) == rfl_msgpack(absent_date));
    CHECK(wire_msgpack::write(amount) == rfl_msgpack(amount));

    CHECK(wire_msgpack::read<ores::utility::uuid::tenant_id>(wire_msgpack::write(tenant)) ==
          tenant);
    CHECK(wire_msgpack::read<std::chrono::year_month_day>(wire_msgpack::write(date)) == date);
    CHECK(wire_msgpack::read<std::chrono::year_month_day>(wire_msgpack::write(absent_date)) ==
          absent_date);
    CHECK(wire_msgpack::read<ores::utility::decimal::decimal>(wire_msgpack::write(amount)) ==
          amount);
}

TEST_CASE("an address, an instant and an error code are what reflect-cpp writes",
          "[codec][msgpack]") {
    const auto address = boost::asio::ip::make_address("192.168.1.100");
    const auto instant =
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456));
    const auto code = ores::utility::serialization::error_code::limit_exceeded;

    CHECK(wire_msgpack::write(address) == rfl_msgpack(address));
    CHECK(wire_msgpack::write(instant) == rfl_msgpack(instant));
    CHECK(wire_msgpack::write(code) == rfl_msgpack(code));

    CHECK(wire_msgpack::read<boost::asio::ip::address>(wire_msgpack::write(address)) == address);
    CHECK(wire_msgpack::read<std::chrono::system_clock::time_point>(wire_msgpack::write(instant)) ==
          instant);
    CHECK(wire_msgpack::read<ores::utility::serialization::error_code>(wire_msgpack::write(code)) ==
          code);
}

TEST_CASE("a string is its own MessagePack form", "[codec][msgpack]") {
    const auto text = std::string("zürich's \"quote\"");
    const auto theirs = rfl_msgpack(text);

    CHECK(wire_msgpack::write(text) == theirs);
    CHECK(wire_msgpack::read<std::string>(theirs) == text);
}

TEST_CASE("a buffer holding more than one value is refused", "[codec][msgpack]") {
    const auto id = boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
    const auto one = wire_msgpack::write(id);

    CHECK_THROWS_AS(wire_msgpack::read<boost::uuids::uuid>(one + one), std::invalid_argument);
}

TEST_CASE("a value of another shape is refused with what it should have been", "[codec][msgpack]") {
    const auto not_a_string = std::string(1, static_cast<char>(0xc0));

    CHECK_THROWS_AS(wire_msgpack::read<boost::uuids::uuid>(not_a_string), std::invalid_argument);
}

TEST_CASE("an integer is the frame reflect-cpp writes", "[codec][msgpack]") {
    const auto small = 7;
    const auto negative = -2;
    const auto wide = std::int64_t{4'294'967'296};

    CHECK(wire_msgpack::write(small) == rfl_msgpack(small));
    CHECK(wire_msgpack::write(negative) == rfl_msgpack(negative));
    CHECK(wire_msgpack::write(wide) == rfl_msgpack(wide));

    CHECK(wire_msgpack::read<int>(rfl_msgpack(negative)) == negative);
    CHECK(wire_msgpack::read<std::int64_t>(rfl_msgpack(wide)) == wide);
}

TEST_CASE("a truth value is the frame reflect-cpp writes", "[codec][msgpack]") {
    CHECK(wire_msgpack::write(true) == rfl_msgpack(true));
    CHECK(wire_msgpack::write(false) == rfl_msgpack(false));
    CHECK(wire_msgpack::read<bool>(rfl_msgpack(true)));
    CHECK_FALSE(wire_msgpack::read<bool>(rfl_msgpack(false)));
}

TEST_CASE("a number packed in a frame too wide for its type is refused", "[codec][msgpack]") {
    CHECK_THROWS_AS(wire_msgpack::read<std::uint8_t>(rfl_msgpack(300)), std::invalid_argument);
    CHECK_THROWS_AS(wire_msgpack::read<std::uint32_t>(rfl_msgpack(-1)), std::invalid_argument);
    CHECK_THROWS_AS(wire_msgpack::read<int>(rfl_msgpack(std::string("7"))), std::invalid_argument);
}

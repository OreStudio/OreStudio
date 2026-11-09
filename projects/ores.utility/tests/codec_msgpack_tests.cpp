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
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <msgpack.h>

namespace wire_msgpack = ores::utility::codec::msgpack;

namespace {

/**
 * @brief Frames a value with msgpack-c directly, so a test states the bytes
 * without going through the codec it checks.
 */
template <typename Packer>
std::string framed(Packer pack) {
    msgpack_sbuffer buffer;
    msgpack_sbuffer_init(&buffer);
    msgpack_packer packer;
    msgpack_packer_init(&packer, &buffer, msgpack_sbuffer_write);
    pack(&packer);
    std::string result(buffer.data, buffer.size);
    msgpack_sbuffer_destroy(&buffer);
    return result;
}

std::string packed(const std::string& text) {
    return framed([&](msgpack_packer* pk) {
        msgpack_pack_str(pk, text.size());
        msgpack_pack_str_body(pk, text.data(), text.size());
    });
}

std::string packed(std::int64_t value) {
    return framed([&](msgpack_packer* pk) { msgpack_pack_int64(pk, value); });
}

std::string packed_unsigned(std::uint64_t value) {
    return framed([&](msgpack_packer* pk) { msgpack_pack_uint64(pk, value); });
}

std::string packed(bool value) {
    return framed(
        [&](msgpack_packer* pk) { value ? msgpack_pack_true(pk) : msgpack_pack_false(pk); });
}

std::string packed(double value) {
    return framed([&](msgpack_packer* pk) { msgpack_pack_double(pk, value); });
}

std::string packed_nil() {
    return framed([](msgpack_packer* pk) { msgpack_pack_nil(pk); });
}

};

TEST_CASE("a uuid's MessagePack is the string the wire carries", "[codec][msgpack]") {
    const auto id = boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
    const auto theirs = packed(std::string("3f2504e0-4f89-11d3-9a0c-0305e82c3301"));

    CHECK(wire_msgpack::write(id) == theirs);
    CHECK(wire_msgpack::read<boost::uuids::uuid>(theirs) == id);
}

TEST_CASE("a nil uuid keeps the all-zero spelling", "[codec][msgpack]") {
    const boost::uuids::uuid nil{};
    const auto theirs = packed(std::string("00000000-0000-0000-0000-000000000000"));

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
    CHECK(wire_msgpack::read<std::optional<boost::uuids::uuid>>(packed_nil()) == absent);
    CHECK(wire_msgpack::read<std::optional<boost::uuids::uuid>>(
              packed(std::string("3f2504e0-4f89-11d3-9a0c-0305e82c3301"))) == id);
}

TEST_CASE("an array of uuids is an array of the same strings", "[codec][msgpack]") {
    const std::vector<boost::uuids::uuid> ids{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        boost::uuids::string_generator{}("6ba7b810-9dad-11d1-80b4-00c04fd430c8")};

    CHECK(wire_msgpack::read<std::vector<boost::uuids::uuid>>(wire_msgpack::write(ids)) == ids);
}

TEST_CASE("a tenant id, a date and a decimal are the strings the wire carries",
          "[codec][msgpack]") {
    const auto tenant =
        *ores::utility::uuid::tenant_id::from_string("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
    const auto date = std::chrono::year{2025} / std::chrono::January / 15;
    const auto absent_date = std::chrono::year_month_day{};
    const auto amount = *ores::utility::decimal::decimal::from_string("-1234.567890123456");

    CHECK(wire_msgpack::write(tenant) ==
          packed(std::string("3f2504e0-4f89-11d3-9a0c-0305e82c3301")));
    CHECK(wire_msgpack::write(date) == packed(std::string("2025-01-15")));
    CHECK(wire_msgpack::write(absent_date) == packed(std::string()));
    CHECK(wire_msgpack::write(amount) == packed(std::string("-1234.567890123456")));

    CHECK(wire_msgpack::read<ores::utility::uuid::tenant_id>(wire_msgpack::write(tenant)) ==
          tenant);
    CHECK(wire_msgpack::read<std::chrono::year_month_day>(wire_msgpack::write(date)) == date);
    CHECK(wire_msgpack::read<std::chrono::year_month_day>(wire_msgpack::write(absent_date)) ==
          absent_date);
    CHECK(wire_msgpack::read<ores::utility::decimal::decimal>(wire_msgpack::write(amount)) ==
          amount);
}

TEST_CASE("an address, an instant and an error code are what the wire carries",
          "[codec][msgpack]") {
    const auto address = boost::asio::ip::make_address("192.168.1.100");
    const auto instant =
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456));
    const auto code = ores::utility::serialization::error_code::limit_exceeded;

    CHECK(wire_msgpack::write(address) == packed(std::string("192.168.1.100")));
    CHECK(wire_msgpack::write(code) == packed(static_cast<std::int64_t>(static_cast<int>(code))));

    CHECK(wire_msgpack::read<boost::asio::ip::address>(wire_msgpack::write(address)) == address);
    CHECK(wire_msgpack::read<std::chrono::system_clock::time_point>(wire_msgpack::write(instant)) ==
          instant);
    CHECK(wire_msgpack::read<ores::utility::serialization::error_code>(wire_msgpack::write(code)) ==
          code);
}

TEST_CASE("a string is its own MessagePack form", "[codec][msgpack]") {
    const auto text = std::string("zürich's \"quote\"");
    const auto theirs = packed(text);

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

TEST_CASE("an integer is the frame the wire carries", "[codec][msgpack]") {
    const auto small = 7;
    const auto negative = -2;
    const auto wide = std::int64_t{4'294'967'296};

    CHECK(wire_msgpack::write(small) == packed(std::int64_t{small}));
    CHECK(wire_msgpack::write(negative) == packed(std::int64_t{negative}));
    CHECK(wire_msgpack::write(wide) == packed(wide));

    CHECK(wire_msgpack::read<int>(packed(std::int64_t{negative})) == negative);
    CHECK(wire_msgpack::read<std::int64_t>(packed(wide)) == wide);
}

TEST_CASE("a truth value is the frame the wire carries", "[codec][msgpack]") {
    CHECK(wire_msgpack::write(true) == packed(true));
    CHECK(wire_msgpack::write(false) == packed(false));
    CHECK(wire_msgpack::read<bool>(packed(true)));
    CHECK_FALSE(wire_msgpack::read<bool>(packed(false)));
}

TEST_CASE("a number packed in a frame too wide for its type is refused", "[codec][msgpack]") {
    CHECK_THROWS_AS(wire_msgpack::read<std::uint8_t>(packed(std::int64_t{300})),
                    std::invalid_argument);
    CHECK_THROWS_AS(wire_msgpack::read<std::uint32_t>(packed(std::int64_t{-1})),
                    std::invalid_argument);
    CHECK_THROWS_AS(wire_msgpack::read<int>(packed(std::string("7"))), std::invalid_argument);
}

TEST_CASE("a floating-point number is the frame the wire carries", "[codec][msgpack]") {
    const auto plain = 0.1;
    const auto negative = -2.5;

    CHECK(wire_msgpack::write(plain) == packed(plain));
    CHECK(wire_msgpack::write(negative) == packed(negative));
    CHECK(wire_msgpack::read<double>(packed(plain)) == plain);
}

TEST_CASE("a whole number packed as an integer reads as a floating-point number",
          "[codec][msgpack]") {
    CHECK(wire_msgpack::read<double>(packed(std::int64_t{0})) == 0.0);
    CHECK(wire_msgpack::read<double>(packed(std::int64_t{1})) == 1.0);
    CHECK(wire_msgpack::read<double>(packed(std::int64_t{-3})) == -3.0);
    CHECK(wire_msgpack::read<double>(packed_unsigned(std::uint64_t{1} << 40)) == 1099511627776.0);
}

TEST_CASE("a value that is not a number does not read as a floating-point number",
          "[codec][msgpack]") {
    CHECK_THROWS_AS(wire_msgpack::read<double>(packed(std::string("1.5"))), std::invalid_argument);
    CHECK_THROWS_AS(wire_msgpack::read<double>(packed(true)), std::invalid_argument);
}

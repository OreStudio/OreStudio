/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.utility/uuid/tenant_id.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[uuid][tenant_id]");

}

using ores::utility::uuid::tenant_id;
using ores::utility::uuid::nil_uuid_str;
using ores::utility::uuid::max_uuid_str;
using namespace ores::logging;

TEST_CASE("tenant_id_system_returns_max_uuid", tags) {
    auto lg(make_logger(test_suite));

    auto sut = tenant_id::system();

    BOOST_LOG_SEV(lg, info) << "System tenant UUID: " << sut.to_string();

    CHECK(sut.to_string() == max_uuid_str);
    CHECK(sut.is_system());
    CHECK_FALSE(sut.is_nil());
}

TEST_CASE("tenant_id_system_is_consistent", tags) {
    auto lg(make_logger(test_suite));

    auto sut1 = tenant_id::system();
    auto sut2 = tenant_id::system();

    BOOST_LOG_SEV(lg, info) << "System tenant 1: " << sut1.to_string();
    BOOST_LOG_SEV(lg, info) << "System tenant 2: " << sut2.to_string();

    CHECK(sut1 == sut2);
}

TEST_CASE("tenant_id_from_uuid_rejects_nil", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::uuid nil = boost::uuids::nil_uuid();
    auto result = tenant_id::from_uuid(nil);

    BOOST_LOG_SEV(lg, info) << "Nil UUID: " << nil;
    BOOST_LOG_SEV(lg, info) << "Result has error: " << !result.has_value();

    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("nil UUID") != std::string::npos);
}

TEST_CASE("tenant_id_from_uuid_accepts_valid_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator gen;
    auto uuid = gen();
    auto result = tenant_id::from_uuid(uuid);

    BOOST_LOG_SEV(lg, info) << "Random UUID: " << uuid;
    BOOST_LOG_SEV(lg, info) << "Result has value: " << result.has_value();

    REQUIRE(result.has_value());
    CHECK(result.value().to_uuid() == uuid);
    CHECK_FALSE(result.value().is_system());
    CHECK_FALSE(result.value().is_nil());
}

TEST_CASE("tenant_id_from_uuid_accepts_max_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator gen;
    auto max_uuid = gen(max_uuid_str);
    auto result = tenant_id::from_uuid(max_uuid);

    BOOST_LOG_SEV(lg, info) << "Max UUID: " << max_uuid;
    BOOST_LOG_SEV(lg, info) << "Result has value: " << result.has_value();

    REQUIRE(result.has_value());
    CHECK(result.value().is_system());
    CHECK(result.value() == tenant_id::system());
}

TEST_CASE("tenant_id_from_string_rejects_nil", tags) {
    auto lg(make_logger(test_suite));

    auto result = tenant_id::from_string(nil_uuid_str);

    BOOST_LOG_SEV(lg, info) << "Nil UUID string: " << nil_uuid_str;
    BOOST_LOG_SEV(lg, info) << "Result has error: " << !result.has_value();

    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("nil UUID") != std::string::npos);
}

TEST_CASE("tenant_id_from_string_accepts_valid_uuid", tags) {
    auto lg(make_logger(test_suite));

    const std::string valid_uuid = "550e8400-e29b-41d4-a716-446655440000";
    auto result = tenant_id::from_string(valid_uuid);

    BOOST_LOG_SEV(lg, info) << "Valid UUID string: " << valid_uuid;
    BOOST_LOG_SEV(lg, info) << "Result has value: " << result.has_value();

    REQUIRE(result.has_value());
    CHECK(result.value().to_string() == valid_uuid);
    CHECK_FALSE(result.value().is_system());
    CHECK_FALSE(result.value().is_nil());
}

TEST_CASE("tenant_id_from_string_accepts_max_uuid", tags) {
    auto lg(make_logger(test_suite));

    auto result = tenant_id::from_string(max_uuid_str);

    BOOST_LOG_SEV(lg, info) << "Max UUID string: " << max_uuid_str;
    BOOST_LOG_SEV(lg, info) << "Result has value: " << result.has_value();

    REQUIRE(result.has_value());
    CHECK(result.value().is_system());
    CHECK(result.value() == tenant_id::system());
}

TEST_CASE("tenant_id_from_string_rejects_invalid_format", tags) {
    auto lg(make_logger(test_suite));

    const std::string invalid = "not-a-uuid";
    auto result = tenant_id::from_string(invalid);

    BOOST_LOG_SEV(lg, info) << "Invalid string: " << invalid;
    BOOST_LOG_SEV(lg, info) << "Result has error: " << !result.has_value();

    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("Failed to parse") != std::string::npos);
}

TEST_CASE("tenant_id_from_string_handles_uppercase", tags) {
    auto lg(make_logger(test_suite));

    const std::string upper = "550E8400-E29B-41D4-A716-446655440000";
    auto result = tenant_id::from_string(upper);

    BOOST_LOG_SEV(lg, info) << "Uppercase UUID: " << upper;
    BOOST_LOG_SEV(lg, info) << "Result has value: " << result.has_value();

    REQUIRE(result.has_value());
    CHECK_FALSE(result.value().is_system());
}

TEST_CASE("tenant_id_equality_comparison", tags) {
    auto lg(make_logger(test_suite));

    const std::string uuid_str = "550e8400-e29b-41d4-a716-446655440000";
    auto result1 = tenant_id::from_string(uuid_str);
    auto result2 = tenant_id::from_string(uuid_str);

    REQUIRE(result1.has_value());
    REQUIRE(result2.has_value());

    BOOST_LOG_SEV(lg, info) << "Tenant 1: " << result1.value().to_string();
    BOOST_LOG_SEV(lg, info) << "Tenant 2: " << result2.value().to_string();

    CHECK(result1.value() == result2.value());
}

TEST_CASE("tenant_id_inequality_for_different_uuids", tags) {
    auto lg(make_logger(test_suite));

    const std::string uuid_str1 = "550e8400-e29b-41d4-a716-446655440000";
    const std::string uuid_str2 = "660e8400-e29b-41d4-a716-446655440000";
    auto result1 = tenant_id::from_string(uuid_str1);
    auto result2 = tenant_id::from_string(uuid_str2);

    REQUIRE(result1.has_value());
    REQUIRE(result2.has_value());

    BOOST_LOG_SEV(lg, info) << "Tenant 1: " << result1.value().to_string();
    BOOST_LOG_SEV(lg, info) << "Tenant 2: " << result2.value().to_string();

    CHECK_FALSE(result1.value() == result2.value());
}

TEST_CASE("tenant_id_to_uuid_returns_correct_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator gen;
    auto expected_uuid = gen();
    auto result = tenant_id::from_uuid(expected_uuid);

    REQUIRE(result.has_value());

    const auto& actual_uuid = result.value().to_uuid();

    BOOST_LOG_SEV(lg, info) << "Expected UUID: " << expected_uuid;
    BOOST_LOG_SEV(lg, info) << "Actual UUID: " << actual_uuid;

    CHECK(actual_uuid == expected_uuid);
}

TEST_CASE("tenant_id_constants_are_valid_uuids", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Nil UUID constant: " << nil_uuid_str;
    BOOST_LOG_SEV(lg, info) << "Max UUID constant: " << max_uuid_str;

    CHECK(std::string(nil_uuid_str).length() == 36);
    CHECK(std::string(max_uuid_str).length() == 36);

    // Verify the patterns
    CHECK(nil_uuid_str[8] == '-');
    CHECK(nil_uuid_str[13] == '-');
    CHECK(nil_uuid_str[18] == '-');
    CHECK(nil_uuid_str[23] == '-');

    CHECK(max_uuid_str[8] == '-');
    CHECK(max_uuid_str[13] == '-');
    CHECK(max_uuid_str[18] == '-');
    CHECK(max_uuid_str[23] == '-');
}

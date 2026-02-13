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
#include "ores.variability/messaging/feature_flags_protocol.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.variability/domain/feature_flags.hpp"

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[messaging]");

ores::variability::domain::feature_flags generate_feature_flag() {
    ores::variability::domain::feature_flags flag;
    flag.name = std::string(faker::word::noun()) + "_" +
        std::string(faker::word::verb());
    flag.version = faker::number::integer(1, 100);
    flag.enabled = faker::datatype::boolean();
    flag.description = std::string(faker::lorem::sentence());
    flag.modified_by = std::string(faker::internet::username());
    flag.recorded_at = std::chrono::system_clock::now();
    return flag;
}

}

using namespace ores::variability::messaging;
using ores::variability::domain::feature_flags;
using namespace ores::logging;

TEST_CASE("get_feature_flags_request_serialize_deserialize", tags) {
    get_feature_flags_request e;

    const auto serialized = e.serialize();
    const auto r = get_feature_flags_request::deserialize(serialized);

    REQUIRE(r.has_value());
}

TEST_CASE("get_feature_flags_request_empty_payload", tags) {
    auto lg(make_logger(test_suite));

    get_feature_flags_request rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    // Serialization should produce empty buffer (no fields in request)
    const auto serialized = rq.serialize();
    CHECK(serialized.empty());
}

TEST_CASE("get_feature_flags_response_empty", tags) {
    auto lg(make_logger(test_suite));

    get_feature_flags_response rp;
    BOOST_LOG_SEV(lg, info) << "Response with empty flags: " << rp;

    CHECK(rp.feature_flags.empty());
}

TEST_CASE("get_feature_flags_response_with_single_flag", tags) {
    auto lg(make_logger(test_suite));

    get_feature_flags_response rp;
    rp.feature_flags.push_back(generate_feature_flag());
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.feature_flags.size() == 1);
    CHECK(!rp.feature_flags[0].name.empty());
}

TEST_CASE("get_feature_flags_response_with_multiple_flags", tags) {
    auto lg(make_logger(test_suite));

    get_feature_flags_response rp;

    const auto expected_size = 5;
    rp.feature_flags.reserve(expected_size);
    for (int i = 0; i < expected_size; ++i) {
        rp.feature_flags.push_back(generate_feature_flag());
    }
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.feature_flags.size() == expected_size);
}

TEST_CASE("get_feature_flags_response_serialize_deserialize_empty", tags) {
    auto lg(make_logger(test_suite));

    get_feature_flags_response e;
    BOOST_LOG_SEV(lg, info) << "Expected (empty): " << e;

    const auto serialized = e.serialize();
    const auto r = get_feature_flags_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.feature_flags.empty());
}

TEST_CASE("get_feature_flags_response_serialize_deserialize_with_flags", tags) {
    auto lg(make_logger(test_suite));

    get_feature_flags_response e;

    const auto expected_size = 3;
    e.feature_flags.reserve(expected_size);
    for (int i = 0; i < expected_size; ++i) {
        feature_flags ff;
        ff.name = "flag_" + std::to_string(i);
        ff.version = i + 1;
        ff.enabled = (i % 2 == 0);
        ff.description = "Description for flag " + std::to_string(i);
        ff.modified_by = "tester" + std::to_string(i);
        ff.recorded_at = std::chrono::system_clock::now();
        e.feature_flags.push_back(ff);
    }
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_feature_flags_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.feature_flags.size() == e.feature_flags.size());
    for (size_t i = 0; i < e.feature_flags.size(); ++i) {
        CHECK(a.feature_flags[i].name == e.feature_flags[i].name);
        CHECK(a.feature_flags[i].version == e.feature_flags[i].version);
        CHECK(a.feature_flags[i].enabled == e.feature_flags[i].enabled);
        CHECK(a.feature_flags[i].description == e.feature_flags[i].description);
        // recorded_at is serialized as string, so check within 1 second tolerance
        const auto diff = std::chrono::abs(
            a.feature_flags[i].recorded_at - e.feature_flags[i].recorded_at);
        CHECK(diff < std::chrono::seconds(1));
    }
}

TEST_CASE("get_feature_flags_response_serialize_deserialize_with_faker", tags) {
    auto lg(make_logger(test_suite));

    get_feature_flags_response e;

    const auto expected_size = 5;
    e.feature_flags.reserve(expected_size);
    for (int i = 0; i < expected_size; ++i) {
        e.feature_flags.push_back(generate_feature_flag());
    }
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_feature_flags_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.feature_flags.size() == e.feature_flags.size());
    for (size_t i = 0; i < e.feature_flags.size(); ++i) {
        CHECK(a.feature_flags[i].name == e.feature_flags[i].name);
        CHECK(a.feature_flags[i].version == e.feature_flags[i].version);
        CHECK(a.feature_flags[i].enabled == e.feature_flags[i].enabled);
        CHECK(a.feature_flags[i].description == e.feature_flags[i].description);
        // recorded_at is serialized as string, so check within 1 second tolerance
        const auto diff = std::chrono::abs(
            a.feature_flags[i].recorded_at - e.feature_flags[i].recorded_at);
        CHECK(diff < std::chrono::seconds(1));
    }
}

TEST_CASE("create_multiple_random_feature_flag_responses", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        get_feature_flags_response rp;

        const int flag_count = faker::number::integer(1, 10);
        for (int j = 0; j < flag_count; ++j) {
            rp.feature_flags.push_back(generate_feature_flag());
        }
        BOOST_LOG_SEV(lg, info) << "Response " << i << " with "
            << rp.feature_flags.size() << " flags: " << rp;

        CHECK(rp.feature_flags.size() == static_cast<size_t>(flag_count));
        for (const auto& ff : rp.feature_flags) {
            CHECK(!ff.name.empty());
            CHECK(!ff.description.empty());
        }
    }
}

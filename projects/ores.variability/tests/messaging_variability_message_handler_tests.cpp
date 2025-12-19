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
#include "ores.variability/messaging/variability_message_handler.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.variability/domain/feature_flags.hpp"
#include "ores.variability/domain/feature_flags_json_io.hpp" // IWYU pragma: keep.
#include "ores.variability/repository/feature_flags_repository.hpp"

using namespace ores::variability::messaging;
using ores::testing::run_coroutine_test;

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string database_table("oresdb.feature_flags");
const std::string tags("[messaging][handler]");

ores::variability::domain::feature_flags generate_feature_flag() {
    ores::variability::domain::feature_flags flag;
    flag.name = std::string(faker::word::noun()) + "_" +
        std::string(faker::word::verb()) + "_" +
        faker::string::alphanumeric(4);
    flag.enabled = faker::datatype::boolean();
    flag.description = std::string(faker::lorem::sentence());
    flag.recorded_by = std::string(faker::internet::username());
    return flag;
}

std::vector<ores::variability::domain::feature_flags>
generate_feature_flags(int count) {
    std::vector<ores::variability::domain::feature_flags> flags;
    flags.reserve(count);
    for (int i = 0; i < count; ++i) {
        flags.push_back(generate_feature_flag());
    }
    return flags;
}

}

using namespace ores::utility::log;
using ores::comms::messaging::message_type;
using ores::comms::messaging::error_code;
using ores::testing::scoped_database_helper;
using ores::variability::repository::feature_flags_repository;

TEST_CASE("handle_list_feature_flags_request_empty", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    variability_message_handler sut(h.context());

    list_feature_flags_request rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::list_feature_flags_request,
            payload, "127.0.0.1:12345");

        REQUIRE(r.has_value());
        const auto response_result =
            list_feature_flags_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.feature_flags.empty());
    });
}

TEST_CASE("handle_list_feature_flags_request_with_flags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);

    // First populate the database with some flags
    feature_flags_repository repo(h.context());
    const int flag_count = 5;
    auto flags = generate_feature_flags(flag_count);
    BOOST_LOG_SEV(lg, info) << "Writing " << flags.size() << " feature flags";
    repo.write(flags);

    variability_message_handler sut(h.context());

    list_feature_flags_request rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::list_feature_flags_request,
            payload, "127.0.0.1:12345");

        REQUIRE(r.has_value());
        const auto response_result =
            list_feature_flags_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.feature_flags.size() == flag_count);
    });
}

TEST_CASE("handle_list_feature_flags_request_multiple_times", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);

    // Populate the database with some flags
    feature_flags_repository repo(h.context());
    auto flags = generate_feature_flags(3);
    repo.write(flags);

    variability_message_handler sut(h.context());

    boost::asio::io_context io_ctx;

    // Call multiple times to ensure consistency
    for (int i = 0; i < 3; ++i) {
        list_feature_flags_request rq;
        const auto payload = rq.serialize();

        run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
            auto r = co_await sut.handle_message(
                message_type::list_feature_flags_request,
                payload, "127.0.0.1:12345");

            REQUIRE(r.has_value());
            const auto response_result =
                list_feature_flags_response::deserialize(r.value());
            REQUIRE(response_result.has_value());
            const auto& rp = response_result.value();
            BOOST_LOG_SEV(lg, info) << "Response " << i << ": "
                << rp.feature_flags.size() << " flags";

            CHECK(rp.feature_flags.size() == 3);
        });
    }
}

TEST_CASE("handle_invalid_message_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    variability_message_handler sut(h.context());

    std::vector<std::byte> empty_payload;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        // Use an invalid message type within the variability range
        auto r = co_await sut.handle_message(
            static_cast<message_type>(0x3FFF),
            empty_payload, "127.0.0.1:12345");

        CHECK(!r.has_value());
        CHECK(r.error() == error_code::invalid_message_type);
    });
}

TEST_CASE("handle_list_feature_flags_request_verifies_content", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);

    // Create flags with specific known values
    feature_flags_repository repo(h.context());

    ores::variability::domain::feature_flags flag1;
    flag1.name = "test_feature_alpha";
    flag1.enabled = true;
    flag1.description = "Alpha feature for testing";
    flag1.recorded_by = "test_user";

    ores::variability::domain::feature_flags flag2;
    flag2.name = "test_feature_beta";
    flag2.enabled = false;
    flag2.description = "Beta feature for testing";
    flag2.recorded_by = "test_admin";

    repo.write({flag1, flag2});

    variability_message_handler sut(h.context());

    list_feature_flags_request rq;
    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::list_feature_flags_request,
            payload, "127.0.0.1:12345");

        REQUIRE(r.has_value());
        const auto response_result =
            list_feature_flags_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        REQUIRE(rp.feature_flags.size() == 2);

        // Find and verify specific flags
        bool found_alpha = false, found_beta = false;
        for (const auto& ff : rp.feature_flags) {
            if (ff.name == "test_feature_alpha") {
                found_alpha = true;
                CHECK(ff.enabled == true);
                CHECK(ff.description == "Alpha feature for testing");
            }
            if (ff.name == "test_feature_beta") {
                found_beta = true;
                CHECK(ff.enabled == false);
                CHECK(ff.description == "Beta feature for testing");
            }
        }

        CHECK(found_alpha);
        CHECK(found_beta);
    });
}

TEST_CASE("handle_list_feature_flags_request_with_many_flags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);

    // Create a larger number of flags
    feature_flags_repository repo(h.context());
    const int flag_count = 20;
    auto flags = generate_feature_flags(flag_count);
    BOOST_LOG_SEV(lg, info) << "Writing " << flags.size() << " feature flags";
    repo.write(flags);

    variability_message_handler sut(h.context());

    list_feature_flags_request rq;
    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::list_feature_flags_request,
            payload, "127.0.0.1:12345");

        REQUIRE(r.has_value());
        const auto response_result =
            list_feature_flags_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response with " << rp.feature_flags.size()
            << " flags";

        CHECK(rp.feature_flags.size() == flag_count);
    });
}

TEST_CASE("handle_list_feature_flags_request_from_different_endpoints", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);

    feature_flags_repository repo(h.context());
    auto flags = generate_feature_flags(3);
    repo.write(flags);

    variability_message_handler sut(h.context());

    list_feature_flags_request rq;
    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;

    // Test from different remote addresses
    const std::vector<std::string> endpoints = {
        "127.0.0.1:12345",
        "192.168.1.100:54321",
        "10.0.0.1:8080"
    };

    for (const auto& endpoint : endpoints) {
        run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
            auto r = co_await sut.handle_message(
                message_type::list_feature_flags_request,
                payload, endpoint);

            REQUIRE(r.has_value());
            const auto response_result =
                list_feature_flags_response::deserialize(r.value());
            REQUIRE(response_result.has_value());
            const auto& rp = response_result.value();
            BOOST_LOG_SEV(lg, info) << "Response from " << endpoint << ": "
                << rp.feature_flags.size() << " flags";

            CHECK(rp.feature_flags.size() == 3);
        });
    }
}

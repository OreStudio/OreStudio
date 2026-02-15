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
#include "ores.comms/net/pending_request_map.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.testing/run_coroutine_test.hpp"

namespace {

const std::string_view test_suite("ores.comms.tests");
const std::string tags("[net][pending_request_map]");

}

using namespace ores::logging;
using ores::comms::net::pending_request_map;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;
using ores::utility::serialization::error_code;
using ores::testing::run_coroutine_test;

TEST_CASE("test_register_request_creates_channel_and_increases_size", tags) {
    auto lg = make_logger(test_suite);

    boost::asio::io_context io_ctx;
    pending_request_map prm(io_ctx.get_executor());

    CHECK(prm.empty());
    CHECK(prm.size() == 0);

    auto channel = prm.register_request(1);
    REQUIRE(channel != nullptr);
    CHECK(prm.size() == 1);
    CHECK(!prm.empty());

    auto channel2 = prm.register_request(2);
    REQUIRE(channel2 != nullptr);
    CHECK(prm.size() == 2);

    BOOST_LOG_SEV(lg, debug) << "Register request verified";
}

TEST_CASE("test_complete_delivers_response", tags) {
    auto lg = make_logger(test_suite);

    boost::asio::io_context io_ctx;
    pending_request_map prm(io_ctx.get_executor());

    const std::uint32_t corr_id = 42;
    auto channel = prm.register_request(corr_id);
    REQUIRE(channel != nullptr);

    std::vector<std::byte> payload;
    frame response(message_type::get_currencies_response, 1, corr_id, payload);

    bool completed = prm.complete(corr_id, std::move(response));
    CHECK(completed);
    CHECK(prm.empty());

    // Verify the channel received the response
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await channel->get();
        REQUIRE(result.has_value());
        CHECK(result->correlation_id() == corr_id);
    });
}

TEST_CASE("test_complete_returns_false_for_unknown_correlation_id", tags) {
    auto lg = make_logger(test_suite);

    boost::asio::io_context io_ctx;
    pending_request_map prm(io_ctx.get_executor());

    std::vector<std::byte> payload;
    frame response(message_type::get_currencies_response, 1, payload);

    bool completed = prm.complete(999, std::move(response));
    CHECK(!completed);
}

TEST_CASE("test_fail_delivers_error", tags) {
    auto lg = make_logger(test_suite);

    boost::asio::io_context io_ctx;
    pending_request_map prm(io_ctx.get_executor());

    const std::uint32_t corr_id = 100;
    auto channel = prm.register_request(corr_id);

    bool failed = prm.fail(corr_id, error_code::network_error);
    CHECK(failed);
    CHECK(prm.empty());

    // Verify the channel received the error
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await channel->get();
        CHECK(!result.has_value());
        CHECK(result.error() == error_code::network_error);
    });
}

TEST_CASE("test_fail_returns_false_for_unknown_correlation_id", tags) {
    auto lg = make_logger(test_suite);

    boost::asio::io_context io_ctx;
    pending_request_map prm(io_ctx.get_executor());

    bool failed = prm.fail(999, error_code::network_error);
    CHECK(!failed);
}

TEST_CASE("test_fail_all_fails_all_pending_requests", tags) {
    auto lg = make_logger(test_suite);

    boost::asio::io_context io_ctx;
    pending_request_map prm(io_ctx.get_executor());

    auto ch1 = prm.register_request(1);
    auto ch2 = prm.register_request(2);
    auto ch3 = prm.register_request(3);

    CHECK(prm.size() == 3);

    prm.fail_all(error_code::network_error);
    CHECK(prm.empty());

    // Verify all channels received errors
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r1 = co_await ch1->get();
        CHECK(!r1.has_value());
        CHECK(r1.error() == error_code::network_error);

        auto r2 = co_await ch2->get();
        CHECK(!r2.has_value());

        auto r3 = co_await ch3->get();
        CHECK(!r3.has_value());
    });
}

TEST_CASE("test_remove_decreases_size", tags) {
    auto lg = make_logger(test_suite);

    boost::asio::io_context io_ctx;
    pending_request_map prm(io_ctx.get_executor());

    prm.register_request(1);
    prm.register_request(2);
    CHECK(prm.size() == 2);

    prm.remove(1);
    CHECK(prm.size() == 1);

    prm.remove(2);
    CHECK(prm.empty());
}

TEST_CASE("test_empty_returns_true_when_no_pending_requests", tags) {
    auto lg = make_logger(test_suite);

    boost::asio::io_context io_ctx;
    pending_request_map prm(io_ctx.get_executor());

    CHECK(prm.empty());

    prm.register_request(1);
    CHECK(!prm.empty());

    prm.remove(1);
    CHECK(prm.empty());
}

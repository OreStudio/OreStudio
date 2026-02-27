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
#include "ores.database/service/postgres_listener_service.hpp"

#include <catch2/catch_test_macros.hpp>
#include <sqlgen/postgres.hpp>
#include <chrono>
#include <future>
#include <thread>
#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string test_suite("ores.database.service.tests");
const std::string tags("[service][postgres_listener]");

/**
 * @brief Sends a NOTIFY on a channel using a separate connection.
 *
 * This simulates an external process sending a notification.
 */
void send_notify(const sqlgen::postgres::Credentials& credentials,
                 const std::string& channel_name,
                 const std::string& payload) {
    auto conn_result = sqlgen::postgres::connect(credentials);
    REQUIRE(conn_result);

    auto result = (*conn_result)->notify(channel_name, payload);
    REQUIRE(result);
}

}

using namespace ores::logging;
using namespace ores::database::service;
using ores::testing::database_helper;

TEST_CASE("postgres_listener_service_lifecycle", tags) {
    auto lg(make_logger(test_suite));

    auto callback = [&](const std::string&, const std::string&) {
        FAIL("Callback invoked during lifecycle test unexpectedly.");
    };

    database_helper h;

    postgres_listener_service listener(h.context(), callback);
    listener.start();

    // Give thread a moment to start and initialise
    std::this_thread::sleep_for(std::chrono::milliseconds(200));

    listener.stop();

    // Ensure the service can be started and stopped cleanly
    SUCCEED("Listener started and stopped without crashing.");
}

TEST_CASE("postgres_listener_service_notification_reception", tags) {
    auto lg(make_logger(test_suite));

    std::string channel_name = "test_channel_reception_" +
        std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
    std::string test_payload = R"({"entity":"ores.database.test_entity", "data":"hello"})";

    std::promise<std::pair<std::string, std::string>> promise;
    std::future<std::pair<std::string, std::string>> future = promise.get_future();
    bool promise_set = false;

    auto callback = [&](const std::string& channel, const std::string& payload) {
        if (!promise_set) {
            promise_set = true;
            promise.set_value({channel, payload});
        }
    };

    database_helper h;
    const auto& credentials = h.context().credentials();

    postgres_listener_service listener(h.context(), callback);
    listener.start();
    listener.subscribe(channel_name);

    // Wait for listener to be ready
    REQUIRE(listener.wait_until_ready());

    // Send a notification from a separate connection
    send_notify(credentials, channel_name, test_payload);

    // Wait for the notification with a timeout
    auto status = future.wait_for(std::chrono::seconds(10));
    REQUIRE(status == std::future_status::ready);

    auto [recv_channel, recv_payload] = future.get();
    REQUIRE(recv_channel == channel_name);
    REQUIRE(recv_payload == test_payload);

    listener.stop();
}

TEST_CASE("postgres_listener_service_no_notification_without_subscribe", tags) {
    auto lg(make_logger(test_suite));

    std::string channel_name = "test_channel_no_subscribe_" +
        std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
    std::string test_payload = R"({"entity":"test_no_subscribe"})";

    bool callback_invoked = false;
    auto callback = [&](const std::string&, const std::string&) {
        callback_invoked = true;
    };

    database_helper h;
    const auto& credentials = h.context().credentials();

    postgres_listener_service listener(h.context(), callback);
    listener.start();

    // Give listener thread a moment to start
    std::this_thread::sleep_for(std::chrono::milliseconds(200));

    // Send a notification without subscribing to the channel
    // PostgreSQL's NOTIFY only delivers to sessions that have LISTENed
    send_notify(credentials, channel_name, test_payload);

    // Wait for a short period to ensure no unexpected notification is received
    std::this_thread::sleep_for(std::chrono::seconds(2));

    REQUIRE_FALSE(callback_invoked);

    listener.stop();
}

TEST_CASE("postgres_listener_service_subscribe_before_start", tags) {
    auto lg(make_logger(test_suite));

    std::string channel_name = "test_channel_pre_subscribe_" +
        std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
    std::string test_payload = R"({"entity":"ores.database.pre_subscribe_entity"})";

    std::promise<std::pair<std::string, std::string>> promise;
    std::future<std::pair<std::string, std::string>> future = promise.get_future();
    bool promise_set = false;

    auto callback = [&](const std::string& channel, const std::string& payload) {
        if (!promise_set) {
            promise_set = true;
            promise.set_value({channel, payload});
        }
    };

    database_helper h;
    const auto& credentials = h.context().credentials();

    postgres_listener_service listener(h.context(), callback);

    // Subscribe BEFORE starting
    listener.subscribe(channel_name);

    listener.start();

    // Wait for listener to be ready
    REQUIRE(listener.wait_until_ready());

    // Send a notification from a separate connection
    send_notify(credentials, channel_name, test_payload);

    // Wait for the notification with a timeout
    auto status = future.wait_for(std::chrono::seconds(10));
    REQUIRE(status == std::future_status::ready);

    auto [recv_channel, recv_payload] = future.get();
    REQUIRE(recv_channel == channel_name);
    REQUIRE(recv_payload == test_payload);

    listener.stop();
}

TEST_CASE("postgres_listener_service_notify_method", tags) {
    auto lg(make_logger(test_suite));

    std::string channel_name = "test_channel_notify_method_" +
        std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
    std::string test_payload = R"({"entity":"ores.database.notify_method_entity"})";

    std::promise<std::pair<std::string, std::string>> promise;
    std::future<std::pair<std::string, std::string>> future = promise.get_future();
    bool promise_set = false;

    auto callback = [&](const std::string& channel, const std::string& payload) {
        if (!promise_set) {
            promise_set = true;
            promise.set_value({channel, payload});
        }
    };

    database_helper h;

    postgres_listener_service listener(h.context(), callback);
    listener.subscribe(channel_name);
    listener.start();

    // Wait for listener to be ready
    REQUIRE(listener.wait_until_ready());

    // Use the service's own notify method
    listener.notify(channel_name, test_payload);

    // Wait for the notification with a timeout
    auto status = future.wait_for(std::chrono::seconds(10));
    REQUIRE(status == std::future_status::ready);

    auto [recv_channel, recv_payload] = future.get();
    REQUIRE(recv_channel == channel_name);
    REQUIRE(recv_payload == test_payload);

    listener.stop();
}

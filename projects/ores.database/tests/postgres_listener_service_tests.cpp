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
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <functional>
#include <future>
#include <mutex>
#include <sqlgen/postgres.hpp>
#include <string>
#include <thread>
#include <vector>

namespace {

const std::string test_suite("ores.database.service.tests");
const std::string tags("[service][postgres_listener]");

/**
 * @brief Polls a predicate every 100ms until it returns true or the timeout
 * elapses.
 */
bool wait_for(const std::function<bool()>& predicate, std::chrono::milliseconds timeout) {
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    while (std::chrono::steady_clock::now() < deadline) {
        if (predicate())
            return true;
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
    return predicate();
}

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

    std::string channel_name =
        "test_channel_reception_" +
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

    std::string channel_name =
        "test_channel_no_subscribe_" +
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

    std::string channel_name =
        "test_channel_pre_subscribe_" +
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

    std::string channel_name =
        "test_channel_notify_method_" +
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

TEST_CASE("postgres_listener_service_connect_forces_utc_session", tags) {
    database_helper h;
    const auto& credentials = h.context().credentials();

    auto conn_result = connect_utc(credentials, "ores.database.test.session");
    REQUIRE(conn_result);

    // Behavioral assertion: in a UTC session the sentinel function equals the
    // naive literal's fold, so this DO block raises. In any non-UTC session
    // (the server default is Europe/London) the naive literal folds away and
    // the DO block succeeds -- which would fail the REQUIRE_FALSE below.
    auto result = (*conn_result)
                      ->execute("DO $$ BEGIN IF ores_utility_infinity_timestamp_fn() = "
                                "'9999-12-31 23:59:59'::timestamptz THEN RAISE EXCEPTION 'utc'; "
                                "END IF; END $$;");
    REQUIRE_FALSE(result);
}

TEST_CASE("postgres_listener_service_reconnect_surfaces_loss_window", tags) {
    std::string channel_name =
        "test_channel_reconnect_" +
        std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
    const std::string lost_payload = R"({"entity":"ores.database.lost"})";
    const std::string recovered_payload = R"({"entity":"ores.database.recovered"})";

    std::vector<std::string> received;
    std::mutex received_mutex;
    auto callback = [&](const std::string&, const std::string& payload) {
        std::lock_guard lock(received_mutex);
        received.push_back(payload);
    };

    database_helper h;
    const auto& credentials = h.context().credentials();

    postgres_listener_service listener(h.context(), callback);
    listener.subscribe(channel_name);
    listener.start();
    REQUIRE(listener.wait_until_ready());

    // The listener stamps each session with an application_name unique to
    // this instance (process pid plus an instance counter), so
    // pg_stat_activity identifies exactly this listener's backend: same
    // user and database, not our own pid, and this exact name. Sibling
    // listeners from concurrently-running suites carry their own names and
    // are never matched, and plain connections of this user never match
    // either. A role may terminate its own sessions, so the DML test user
    // can signal the listener backend.
    auto control = sqlgen::postgres::connect(credentials);
    REQUIRE(control);

    const std::string backend_filter =
        "usename = current_user AND datname = current_database() "
        "AND pid <> pg_backend_pid() "
        "AND application_name = '" + listener.application_name() + "'";

    const std::string expect_present =
        "DO $$ BEGIN IF NOT EXISTS (SELECT 1 FROM pg_stat_activity WHERE " + backend_filter +
        ") THEN RAISE EXCEPTION 'listener backend not found'; END IF; END $$;";
    const std::string terminate_listener =
        "DO $$ BEGIN PERFORM pg_terminate_backend(pid) FROM pg_stat_activity WHERE " +
        backend_filter + "; END $$;";
    const std::string expect_gone =
        "DO $$ BEGIN IF EXISTS (SELECT 1 FROM pg_stat_activity WHERE " + backend_filter +
        ") THEN RAISE EXCEPTION 'listener backend still present'; END IF; END $$;";

    // Precondition: the listener session is registered in pg_stat_activity.
    REQUIRE(wait_for([&] { return bool((*control)->execute(expect_present)); },
                     std::chrono::seconds(10)));

    // Kill the listener session. PostgreSQL delivers NOTIFY only to live
    // LISTEN sessions, so everything sent from here until the reconnect is
    // a lost window.
    auto terminated = (*control)->execute(terminate_listener);
    REQUIRE(terminated);

    // Wait until the session is gone before sending the in-flight NOTIFY,
    // so the loss is guaranteed rather than racy.
    REQUIRE(wait_for([&] { return bool((*control)->execute(expect_gone)); },
                     std::chrono::seconds(10)));

    // Sent while the listener is down: lost forever. The surfacing is the
    // counter and the error logs, not a recovery.
    send_notify(credentials, channel_name, lost_payload);

    // The listener detects the loss on its next poll and records the window.
    REQUIRE(wait_for([&] { return listener.connection_loss_count() >= 1; },
                     std::chrono::seconds(10)));

    // Wait until the listener session is back, then let the reissued LISTEN
    // land before sending the recovery notification.
    REQUIRE(wait_for([&] { return bool((*control)->execute(expect_present)); },
                     std::chrono::seconds(10)));
    std::this_thread::sleep_for(std::chrono::milliseconds(300));

    send_notify(credentials, channel_name, recovered_payload);

    // The recovered notification must arrive exactly once; the lost one
    // must never arrive.
    REQUIRE(wait_for([&] {
                         std::lock_guard lock(received_mutex);
                         return received.size() == 1;
                     },
                     std::chrono::seconds(10)));
    {
        std::lock_guard lock(received_mutex);
        REQUIRE(received.size() == 1);
        REQUIRE(received[0] == recovered_payload);
    }

    listener.stop();
}

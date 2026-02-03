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
#include "ores.telemetry/log/database_sink_backend.hpp"
#include "ores.telemetry/log/database_sink_utils.hpp"
#include "ores.telemetry/domain/resource.hpp"
#include "ores.telemetry/domain/telemetry_log_entry.hpp"
#include "ores.logging/make_logger.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include <vector>

namespace {

const std::string test_suite("ores.telemetry.database.tests");
const std::string tags("[database_sink_backend]");

}

using namespace ores::telemetry::log;
using namespace ores::telemetry::domain;
using namespace ores::logging;

TEST_CASE("database_sink_backend_constructs_with_defaults", tags) {
    auto lg(make_logger(test_suite));

    auto resource = std::make_shared<ores::telemetry::domain::resource>(
        ores::telemetry::domain::resource::from_environment("test-app", "1.0.0"));

    std::vector<telemetry_log_entry> captured_entries;
    auto handler = [&captured_entries](const telemetry_log_entry& entry) {
        captured_entries.push_back(entry);
    };

    // Should construct without throwing
    REQUIRE_NOTHROW(database_sink_backend(resource, handler));

    BOOST_LOG_SEV(lg, debug) << "Backend constructed successfully with defaults";
}

TEST_CASE("database_sink_backend_constructs_with_custom_source", tags) {
    auto lg(make_logger(test_suite));

    auto resource = std::make_shared<ores::telemetry::domain::resource>(
        ores::telemetry::domain::resource::from_environment("test-app", "1.0.0"));

    std::vector<telemetry_log_entry> captured_entries;
    auto handler = [&captured_entries](const telemetry_log_entry& entry) {
        captured_entries.push_back(entry);
    };

    // Should construct without throwing with custom source type and name
    REQUIRE_NOTHROW(database_sink_backend(resource, handler, "server", "my-service"));

    BOOST_LOG_SEV(lg, debug) << "Backend constructed successfully with custom source";
}

TEST_CASE("database_sink_backend_accepts_session_and_account_ids", tags) {
    auto lg(make_logger(test_suite));

    auto resource = std::make_shared<ores::telemetry::domain::resource>(
        ores::telemetry::domain::resource::from_environment("test-app", "1.0.0"));

    std::vector<telemetry_log_entry> captured_entries;
    auto handler = [&captured_entries](const telemetry_log_entry& entry) {
        captured_entries.push_back(entry);
    };

    database_sink_backend backend(resource, handler, "test", "session-test");

    boost::uuids::random_generator gen;
    auto session_id = gen();
    auto account_id = gen();

    // Should set IDs without throwing
    REQUIRE_NOTHROW(backend.set_session_id(session_id));
    REQUIRE_NOTHROW(backend.set_account_id(account_id));

    BOOST_LOG_SEV(lg, debug) << "Session and account IDs set successfully";
}

TEST_CASE("make_forwarding_handler_forwards_entries", tags) {
    auto lg(make_logger(test_suite));

    std::vector<telemetry_log_entry> received_entries;

    auto inner_handler = [&received_entries](const telemetry_log_entry& entry) {
        received_entries.push_back(entry);
    };

    auto forwarding_handler = make_forwarding_handler(inner_handler);

    // Create a test entry
    telemetry_log_entry entry;
    entry.timestamp = std::chrono::system_clock::now();
    entry.level = "info";
    entry.message = "Test forwarding";
    entry.component = "test";
    entry.source = telemetry_source::client;
    entry.source_name = "unit-test";

    // Forward the entry
    forwarding_handler(entry);

    REQUIRE(received_entries.size() == 1);
    REQUIRE(received_entries[0].message == "Test forwarding");
    REQUIRE(received_entries[0].level == "info");

    BOOST_LOG_SEV(lg, debug) << "Forwarding handler works correctly";
}

TEST_CASE("make_forwarding_handler_handles_exceptions", tags) {
    auto lg(make_logger(test_suite));

    auto throwing_handler = [](const telemetry_log_entry&) {
        throw std::runtime_error("Test exception");
    };

    auto forwarding_handler = make_forwarding_handler(throwing_handler);

    telemetry_log_entry entry;
    entry.timestamp = std::chrono::system_clock::now();
    entry.level = "info";
    entry.message = "Test exception handling";

    // Should not throw - exception should be caught and logged to stderr
    REQUIRE_NOTHROW(forwarding_handler(entry));

    BOOST_LOG_SEV(lg, debug) << "Exception handling in forwarding handler works";
}

// Note: lifecycle_manager integration test removed because creating a second
// lifecycle_manager conflicts with the test framework's logging setup.
// The lifecycle_manager functionality is tested via other integration tests.

TEST_CASE("telemetry_log_entry_has_correct_defaults", tags) {
    auto lg(make_logger(test_suite));

    telemetry_log_entry entry;

    // Check that optional fields are empty by default
    REQUIRE_FALSE(entry.session_id.has_value());
    REQUIRE_FALSE(entry.account_id.has_value());

    // Check default source
    REQUIRE(entry.source == telemetry_source::client);

    BOOST_LOG_SEV(lg, debug) << "Telemetry log entry defaults are correct";
}

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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.telemetry.core/domain/resource.hpp"
#include "ores.telemetry.core/log/database_sink_backend.hpp"
#include "ores.telemetry.core/log/database_sink_utils.hpp"
#include "ores.telemetry.core/messaging/logs_protocol.hpp"
#include <boost/log/attributes/attribute_value_impl.hpp>
#include <boost/log/attributes/attribute_value_set.hpp>
#include <boost/log/core.hpp>
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <iostream>
#include <sstream>
#include <vector>

namespace {

const std::string test_suite("ores.telemetry.database.tests");
const std::string tags("[database_sink_backend]");

/**
 * @brief Builds a record view carrying the given attribute values.
 *
 * Tests run with Boost.Log disabled by the test listener, so a record is only
 * obtainable while logging is enabled. The previous setting is restored
 * immediately after the record is locked.
 */
boost::log::record_view make_record(const std::string& message,
                                    ores::logging::boost_severity severity,
                                    const std::string& channel) {
    boost::log::attribute_value_set values;
    values.insert("Message", boost::log::attributes::make_attribute_value(message));
    values.insert("Severity", boost::log::attributes::make_attribute_value(severity));
    values.insert("Channel", boost::log::attributes::make_attribute_value(channel));

    auto& core = *boost::log::core::get();
    const bool was_enabled = core.get_logging_enabled();
    core.set_logging_enabled(true);
    auto rec = core.open_record(values);
    auto view = rec.lock();
    core.set_logging_enabled(was_enabled);
    return view;
}

std::shared_ptr<ores::telemetry::domain::resource> make_resource() {
    return std::make_shared<ores::telemetry::domain::resource>(
        ores::telemetry::domain::resource::from_environment("test-app", "1.0.0"));
}

}

using namespace ores::telemetry::log;
using namespace ores::telemetry::domain;
using namespace ores::telemetry::messaging;
using namespace ores::logging;

TEST_CASE("database_sink_backend_constructs_with_defaults", tags) {
    auto lg(make_logger(test_suite));

    auto resource = make_resource();

    std::vector<telemetry_log_entry> captured_entries;
    auto handler = [&captured_entries](const telemetry_log_entry& entry) {
        captured_entries.push_back(entry);
    };

    database_sink_backend backend(resource, handler);
    backend.consume(make_record("default source message", boost_severity::debug, "test.component"));

    REQUIRE(captured_entries.size() == 1);
    const auto& entry = captured_entries[0];
    REQUIRE(entry.message == "default source message");
    REQUIRE(entry.level == "debug");
    REQUIRE(entry.component == "test.component");
    REQUIRE(entry.source == telemetry_source::client);
    REQUIRE(entry.source_name == "unit-test");
    REQUIRE_FALSE(entry.session_id.has_value());
    REQUIRE_FALSE(entry.account_id.has_value());

    BOOST_LOG_SEV(lg, debug) << "Backend constructed successfully with defaults";
}

TEST_CASE("database_sink_backend_constructs_with_custom_source", tags) {
    auto lg(make_logger(test_suite));

    auto resource = make_resource();

    std::vector<telemetry_log_entry> captured_entries;
    auto handler = [&captured_entries](const telemetry_log_entry& entry) {
        captured_entries.push_back(entry);
    };

    database_sink_backend backend(resource, handler, "server", "my-service");
    backend.consume(make_record("custom source message", boost_severity::warn, "custom.component"));

    REQUIRE(captured_entries.size() == 1);
    const auto& entry = captured_entries[0];
    REQUIRE(entry.source == telemetry_source::server);
    REQUIRE(entry.source_name == "my-service");
    REQUIRE(entry.level == "warn");
    REQUIRE(entry.message == "custom source message");
    REQUIRE(entry.component == "custom.component");

    BOOST_LOG_SEV(lg, debug) << "Backend constructed successfully with custom source";
}

TEST_CASE("database_sink_backend_accepts_session_and_account_ids", tags) {
    auto lg(make_logger(test_suite));

    auto resource = make_resource();

    std::vector<telemetry_log_entry> captured_entries;
    auto handler = [&captured_entries](const telemetry_log_entry& entry) {
        captured_entries.push_back(entry);
    };

    database_sink_backend backend(resource, handler, "test", "session-test");

    const boost::uuids::string_generator string_gen;
    const auto session_id = string_gen("11111111-1111-1111-1111-111111111111");
    const auto account_id = string_gen("22222222-2222-2222-2222-222222222222");

    backend.set_session_id(session_id);
    backend.set_account_id(account_id);
    backend.consume(make_record("ids message", boost_severity::info, "ids.component"));

    REQUIRE(captured_entries.size() == 1);
    const auto& entry = captured_entries[0];
    REQUIRE(entry.message == "ids message");
    REQUIRE(entry.session_id.has_value());
    REQUIRE(entry.session_id.value() == session_id);
    REQUIRE(entry.account_id.has_value());
    REQUIRE(entry.account_id.value() == account_id);

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

    std::ostringstream captured_stderr;
    auto* const previous_streambuf = std::cerr.rdbuf(captured_stderr.rdbuf());
    try {
        forwarding_handler(entry);
    } catch (...) {
        std::cerr.rdbuf(previous_streambuf);
        FAIL("forwarding handler must not propagate exceptions");
    }
    std::cerr.rdbuf(previous_streambuf);

    REQUIRE(captured_stderr.str() ==
            "[Logging Sink Error] Failed to forward log entry: Test exception\n");

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

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
#include "ores.telemetry/messaging/telemetry_protocol.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.telemetry.tests");
const std::string tags("[messaging][telemetry]");

using namespace ores::telemetry::messaging;
using namespace ores::telemetry::domain;
using namespace ores::telemetry::log;

/**
 * @brief Helper to create a sample telemetry_log_entry.
 */
telemetry_log_entry make_sample_entry() {
    telemetry_log_entry entry;
    entry.id = boost::uuids::random_generator()();
    entry.timestamp = std::chrono::system_clock::now();
    entry.source = telemetry_source::client;
    entry.source_name = "ores.qt";
    entry.session_id = boost::uuids::random_generator()();
    entry.account_id = boost::uuids::random_generator()();
    entry.level = "info";
    entry.component = "ores.qt.main";
    entry.message = std::string(faker::lorem::sentence());
    entry.tag = "general";
    entry.recorded_at = std::chrono::system_clock::now();
    return entry;
}

/**
 * @brief Helper to create a sample telemetry_query.
 */
telemetry_query make_sample_query() {
    telemetry_query query;
    query.start_time = std::chrono::system_clock::now() - std::chrono::hours(24);
    query.end_time = std::chrono::system_clock::now();
    query.source = telemetry_source::client;
    query.source_name = "ores.qt";
    query.level = "info";
    query.limit = 100;
    query.offset = 0;
    return query;
}

/**
 * @brief Helper to create a sample telemetry_stats entry.
 */
telemetry_stats make_sample_stats() {
    telemetry_stats stats;
    stats.period_start = std::chrono::system_clock::now();
    stats.source = telemetry_source::client;
    stats.source_name = "ores.qt";
    stats.component = "ores.qt.main";
    stats.level = "info";
    stats.log_count = 42;
    stats.unique_sessions = 5;
    stats.unique_accounts = 3;
    return stats;
}

}

// =============================================================================
// submit_telemetry_response tests
// =============================================================================

TEST_CASE("submit_telemetry_response_success", tags) {
    auto lg(make_logger(test_suite));

    submit_telemetry_response rp;
    rp.success = true;
    rp.entries_accepted = 100;
    rp.message = "";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.entries_accepted == 100);
    CHECK(rp.message.empty());
}

TEST_CASE("submit_telemetry_response_partial_failure", tags) {
    auto lg(make_logger(test_suite));

    submit_telemetry_response rp;
    rp.success = true;
    rp.entries_accepted = 95;
    rp.message = "5 entries rejected due to invalid timestamps";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.entries_accepted == 95);
    CHECK(rp.message == "5 entries rejected due to invalid timestamps");
}

TEST_CASE("submit_telemetry_response_failure", tags) {
    auto lg(make_logger(test_suite));

    submit_telemetry_response rp;
    rp.success = false;
    rp.entries_accepted = 0;
    rp.message = "Database connection failed";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.entries_accepted == 0);
    CHECK(rp.message == "Database connection failed");
}

TEST_CASE("submit_telemetry_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    submit_telemetry_response e;
    e.success = true;
    e.entries_accepted = 42;
    e.message = "All entries accepted";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = submit_telemetry_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.entries_accepted == e.entries_accepted);
    CHECK(a.message == e.message);
}

// =============================================================================
// get_telemetry_logs_request tests
// =============================================================================

TEST_CASE("get_telemetry_logs_request_minimal_query", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_request rq;
    rq.query.start_time = std::chrono::system_clock::now() - std::chrono::hours(1);
    rq.query.end_time = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.query.limit == 1000); // default
    CHECK(rq.query.offset == 0);   // default
    CHECK_FALSE(rq.query.source.has_value());
    CHECK_FALSE(rq.query.source_name.has_value());
}

TEST_CASE("get_telemetry_logs_request_with_filters", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_request rq;
    rq.query = make_sample_query();
    rq.query.min_level = "warn";
    rq.query.component = "ores.comms";
    rq.query.tag = "security";
    rq.query.message_contains = "error";
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.query.source == telemetry_source::client);
    CHECK(rq.query.source_name == "ores.qt");
    CHECK(rq.query.min_level == "warn");
    CHECK(rq.query.component == "ores.comms");
    CHECK(rq.query.tag == "security");
    CHECK(rq.query.message_contains == "error");
}

TEST_CASE("get_telemetry_logs_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_request e;
    e.query = make_sample_query();
    e.query.session_id = boost::uuids::random_generator()();
    e.query.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_telemetry_logs_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    // Check time ranges match (to millisecond precision)
    auto expected_start = std::chrono::duration_cast<std::chrono::milliseconds>(
        e.query.start_time.time_since_epoch()).count();
    auto actual_start = std::chrono::duration_cast<std::chrono::milliseconds>(
        a.query.start_time.time_since_epoch()).count();
    CHECK(actual_start == expected_start);

    CHECK(a.query.source == e.query.source);
    CHECK(a.query.source_name == e.query.source_name);
    CHECK(a.query.session_id == e.query.session_id);
    CHECK(a.query.account_id == e.query.account_id);
    CHECK(a.query.level == e.query.level);
    CHECK(a.query.limit == e.query.limit);
    CHECK(a.query.offset == e.query.offset);
}

// =============================================================================
// get_telemetry_logs_response tests
// =============================================================================

TEST_CASE("get_telemetry_logs_response_empty", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_response rp;
    rp.success = true;
    rp.total_count = 0;
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.entries.empty());
    CHECK(rp.total_count == 0);
    CHECK(rp.message.empty());
}

TEST_CASE("get_telemetry_logs_response_with_entries", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_response rp;
    rp.success = true;
    rp.entries.push_back(make_sample_entry());
    rp.entries.push_back(make_sample_entry());
    rp.entries.push_back(make_sample_entry());
    rp.total_count = 100; // More than returned (pagination)
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.entries.size() == 3);
    CHECK(rp.total_count == 100);
}

TEST_CASE("get_telemetry_logs_response_failure", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_response rp;
    rp.success = false;
    rp.total_count = 0;
    rp.message = "Invalid time range: start_time > end_time";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.entries.empty());
    CHECK(rp.message == "Invalid time range: start_time > end_time");
}

TEST_CASE("get_telemetry_logs_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_response e;
    e.success = true;
    e.entries.push_back(make_sample_entry());
    e.entries.push_back(make_sample_entry());
    e.total_count = 50;
    e.message = "";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_telemetry_logs_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.entries.size() == e.entries.size());
    CHECK(a.total_count == e.total_count);
    CHECK(a.message == e.message);

    // Verify entry content
    for (size_t i = 0; i < e.entries.size(); ++i) {
        CHECK(a.entries[i].id == e.entries[i].id);
        CHECK(a.entries[i].source == e.entries[i].source);
        CHECK(a.entries[i].source_name == e.entries[i].source_name);
        CHECK(a.entries[i].level == e.entries[i].level);
        CHECK(a.entries[i].component == e.entries[i].component);
        CHECK(a.entries[i].message == e.entries[i].message);
    }
}

// =============================================================================
// get_telemetry_stats_request tests
// =============================================================================

TEST_CASE("get_telemetry_stats_request_hourly", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_stats_request rq;
    rq.query.start_time = std::chrono::system_clock::now() - std::chrono::hours(24);
    rq.query.end_time = std::chrono::system_clock::now();
    rq.query.granularity = stats_granularity::hourly;
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.query.granularity == stats_granularity::hourly);
    CHECK_FALSE(rq.query.source.has_value());
    CHECK_FALSE(rq.query.source_name.has_value());
}

TEST_CASE("get_telemetry_stats_request_daily_with_filters", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_stats_request rq;
    rq.query.start_time = std::chrono::system_clock::now() - std::chrono::hours(24 * 7);
    rq.query.end_time = std::chrono::system_clock::now();
    rq.query.granularity = stats_granularity::daily;
    rq.query.source = telemetry_source::client;
    rq.query.source_name = "ores.comms.shell";
    rq.query.level = "error";
    rq.query.component = "ores.comms";
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.query.granularity == stats_granularity::daily);
    CHECK(rq.query.source == telemetry_source::client);
    CHECK(rq.query.source_name == "ores.comms.shell");
    CHECK(rq.query.level == "error");
    CHECK(rq.query.component == "ores.comms");
}

TEST_CASE("get_telemetry_stats_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_stats_request e;
    e.query.start_time = std::chrono::system_clock::now() - std::chrono::hours(48);
    e.query.end_time = std::chrono::system_clock::now();
    e.query.granularity = stats_granularity::daily;
    e.query.source = telemetry_source::server;
    e.query.source_name = "ores.comms.service";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_telemetry_stats_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.query.granularity == e.query.granularity);
    CHECK(a.query.source == e.query.source);
    CHECK(a.query.source_name == e.query.source_name);
}

// =============================================================================
// get_telemetry_stats_response tests
// =============================================================================

TEST_CASE("get_telemetry_stats_response_empty", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_stats_response rp;
    rp.success = true;
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.stats.empty());
    CHECK(rp.message.empty());
}

TEST_CASE("get_telemetry_stats_response_with_stats", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_stats_response rp;
    rp.success = true;
    rp.stats.push_back(make_sample_stats());
    rp.stats.push_back(make_sample_stats());
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.stats.size() == 2);
}

TEST_CASE("get_telemetry_stats_response_failure", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_stats_response rp;
    rp.success = false;
    rp.message = "Continuous aggregate not yet refreshed";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.stats.empty());
    CHECK(rp.message == "Continuous aggregate not yet refreshed");
}

TEST_CASE("get_telemetry_stats_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_stats_response e;
    e.success = true;
    e.stats.push_back(make_sample_stats());
    e.stats.push_back(make_sample_stats());
    e.stats.push_back(make_sample_stats());
    e.message = "";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_telemetry_stats_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.stats.size() == e.stats.size());
    CHECK(a.message == e.message);

    // Verify stats content
    for (size_t i = 0; i < e.stats.size(); ++i) {
        CHECK(a.stats[i].source == e.stats[i].source);
        CHECK(a.stats[i].source_name == e.stats[i].source_name);
        CHECK(a.stats[i].component == e.stats[i].component);
        CHECK(a.stats[i].level == e.stats[i].level);
        CHECK(a.stats[i].log_count == e.stats[i].log_count);
        CHECK(a.stats[i].unique_sessions == e.stats[i].unique_sessions);
        CHECK(a.stats[i].unique_accounts == e.stats[i].unique_accounts);
    }
}

// =============================================================================
// Edge cases and stress tests
// =============================================================================

TEST_CASE("telemetry_logs_response_large_batch", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_response rp;
    rp.success = true;
    rp.total_count = 10000;

    const size_t batch_size = 100;
    for (size_t i = 0; i < batch_size; ++i) {
        rp.entries.push_back(make_sample_entry());
    }

    BOOST_LOG_SEV(lg, info) << "Response with " << rp.entries.size() << " entries";

    const auto serialized = rp.serialize();
    const auto r = get_telemetry_logs_response::deserialize(serialized);

    REQUIRE(r.has_value());
    CHECK(r->entries.size() == batch_size);
    CHECK(r->total_count == 10000);
}

TEST_CASE("telemetry_entry_with_long_message", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_response rp;
    rp.success = true;

    auto entry = make_sample_entry();
    // Create a long message (10KB)
    entry.message = std::string(10 * 1024, 'x');
    rp.entries.push_back(std::move(entry));
    rp.total_count = 1;

    const auto serialized = rp.serialize();
    const auto r = get_telemetry_logs_response::deserialize(serialized);

    REQUIRE(r.has_value());
    CHECK(r->entries.size() == 1);
    CHECK(r->entries[0].message.size() == 10 * 1024);
}

TEST_CASE("telemetry_entry_without_optional_fields", tags) {
    auto lg(make_logger(test_suite));

    get_telemetry_logs_response rp;
    rp.success = true;

    telemetry_log_entry entry;
    entry.id = boost::uuids::random_generator()();
    entry.timestamp = std::chrono::system_clock::now();
    entry.source = telemetry_source::server;
    entry.source_name = "ores.comms.service";
    // No session_id, no account_id (server logs)
    entry.level = "info";
    entry.component = "ores.comms.service.app";
    entry.message = "Server startup complete";
    entry.tag = "";
    entry.recorded_at = std::chrono::system_clock::now();

    rp.entries.push_back(std::move(entry));
    rp.total_count = 1;

    const auto serialized = rp.serialize();
    const auto r = get_telemetry_logs_response::deserialize(serialized);

    REQUIRE(r.has_value());
    CHECK(r->entries.size() == 1);
    CHECK_FALSE(r->entries[0].session_id.has_value());
    CHECK_FALSE(r->entries[0].account_id.has_value());
}

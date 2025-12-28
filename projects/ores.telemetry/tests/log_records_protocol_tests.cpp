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
#include "ores.telemetry/messaging/log_records_protocol.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.telemetry/domain/resource.hpp"
#include "ores.telemetry/generators/trace_id_generator.hpp"
#include "ores.telemetry/generators/span_id_generator.hpp"

namespace {

const std::string_view test_suite("ores.telemetry.tests");
const std::string tags("[messaging]");

}

using namespace ores::telemetry::messaging;
using namespace ores::telemetry::domain;
using namespace ores::telemetry::log;

TEST_CASE("submit_log_records_request_empty_batch", tags) {
    auto lg(make_logger(test_suite));

    submit_log_records_request rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.records.empty());

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    CHECK(r->records.empty());
}

TEST_CASE("submit_log_records_request_single_record", tags) {
    auto lg(make_logger(test_suite));

    submit_log_records_request rq;

    log_record rec;
    rec.timestamp = std::chrono::system_clock::now();
    rec.severity = severity_level::info;
    rec.body = "Test log message";
    rec.logger_name = "test.logger";
    rq.records.push_back(std::move(rec));

    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == 1);

    const auto& actual = r->records[0];
    CHECK(actual.severity == severity_level::info);
    CHECK(actual.body == "Test log message");
    CHECK(actual.logger_name == "test.logger");
}

TEST_CASE("submit_log_records_request_with_trace_and_span", tags) {
    auto lg(make_logger(test_suite));

    ores::telemetry::generators::trace_id_generator trace_gen;
    ores::telemetry::generators::span_id_generator span_gen;

    submit_log_records_request rq;

    log_record rec;
    rec.timestamp = std::chrono::system_clock::now();
    rec.severity = severity_level::debug;
    rec.body = "Traced operation";
    rec.logger_name = "trace.logger";
    rec.trace = trace_gen();
    rec.span = span_gen();
    rq.records.push_back(std::move(rec));

    BOOST_LOG_SEV(lg, info) << "Request: " << rq;
    BOOST_LOG_SEV(lg, info) << "Trace ID: " << rq.records[0].trace->to_hex();
    BOOST_LOG_SEV(lg, info) << "Span ID: " << rq.records[0].span->to_hex();

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == 1);

    const auto& actual = r->records[0];
    REQUIRE(actual.trace.has_value());
    REQUIRE(actual.span.has_value());
    CHECK(actual.trace->to_hex() == rq.records[0].trace->to_hex());
    CHECK(actual.span->to_hex() == rq.records[0].span->to_hex());
}

TEST_CASE("submit_log_records_request_with_resource", tags) {
    auto lg(make_logger(test_suite));

    auto res = std::make_shared<resource>(
        resource::from_environment("test-service", "1.0.0"));

    submit_log_records_request rq;

    log_record rec;
    rec.timestamp = std::chrono::system_clock::now();
    rec.severity = severity_level::warn;
    rec.body = "Warning message";
    rec.logger_name = "resource.logger";
    rec.source_resource = res;
    rq.records.push_back(std::move(rec));

    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == 1);

    // Note: resource is not fully reconstructed on deserialize,
    // only the service name is transferred
    const auto& actual = r->records[0];
    CHECK(actual.severity == severity_level::warn);
    CHECK(actual.body == "Warning message");
}

TEST_CASE("submit_log_records_request_multiple_records", tags) {
    auto lg(make_logger(test_suite));

    ores::telemetry::generators::trace_id_generator trace_gen;
    ores::telemetry::generators::span_id_generator span_gen;

    submit_log_records_request rq;

    const auto expected_size = 5;
    auto shared_trace = trace_gen();

    for (int i = 0; i < expected_size; ++i) {
        log_record rec;
        rec.timestamp = std::chrono::system_clock::now();
        rec.severity = static_cast<severity_level>(
            static_cast<int>(severity_level::trace) + i * 4);
        rec.body = std::string(faker::lorem::sentence());
        rec.logger_name = "test.logger." + std::to_string(i);
        rec.trace = shared_trace;
        rec.span = span_gen();
        rq.records.push_back(std::move(rec));
    }

    BOOST_LOG_SEV(lg, info) << "Request with " << rq.records.size() << " records";

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == expected_size);

    // All records should have the same trace ID
    for (size_t i = 0; i < r->records.size(); ++i) {
        const auto& actual = r->records[i];
        const auto& expected = rq.records[i];

        CHECK(actual.severity == expected.severity);
        CHECK(actual.body == expected.body);
        CHECK(actual.logger_name == expected.logger_name);

        REQUIRE(actual.trace.has_value());
        CHECK(actual.trace->to_hex() == shared_trace.to_hex());

        REQUIRE(actual.span.has_value());
        CHECK(actual.span->to_hex() == expected.span->to_hex());
    }
}

TEST_CASE("submit_log_records_request_all_severity_levels", tags) {
    auto lg(make_logger(test_suite));

    submit_log_records_request rq;

    const std::vector<severity_level> levels = {
        severity_level::trace,
        severity_level::debug,
        severity_level::info,
        severity_level::warn,
        severity_level::error,
        severity_level::fatal
    };

    for (auto level : levels) {
        log_record rec;
        rec.timestamp = std::chrono::system_clock::now();
        rec.severity = level;
        rec.body = "Message at severity " + std::to_string(static_cast<int>(level));
        rec.logger_name = "severity.test";
        rq.records.push_back(std::move(rec));
    }

    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == levels.size());

    for (size_t i = 0; i < levels.size(); ++i) {
        CHECK(r->records[i].severity == levels[i]);
    }
}

TEST_CASE("submit_log_records_request_with_special_characters", tags) {
    auto lg(make_logger(test_suite));

    submit_log_records_request rq;

    log_record rec;
    rec.timestamp = std::chrono::system_clock::now();
    rec.severity = severity_level::info;
    rec.body = "Message with special chars: \"quotes\", \\backslash, \ttab, \nnewline";
    rec.logger_name = "special.chars.logger";
    rq.records.push_back(std::move(rec));

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == 1);
    CHECK(r->records[0].body == rq.records[0].body);
}

TEST_CASE("submit_log_records_request_empty_strings", tags) {
    auto lg(make_logger(test_suite));

    submit_log_records_request rq;

    log_record rec;
    rec.timestamp = std::chrono::system_clock::now();
    rec.severity = severity_level::info;
    rec.body = "";
    rec.logger_name = "";
    rq.records.push_back(std::move(rec));

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == 1);
    CHECK(r->records[0].body.empty());
    CHECK(r->records[0].logger_name.empty());
}

TEST_CASE("submit_log_records_request_no_trace_or_span", tags) {
    auto lg(make_logger(test_suite));

    submit_log_records_request rq;

    log_record rec;
    rec.timestamp = std::chrono::system_clock::now();
    rec.severity = severity_level::error;
    rec.body = "Error without trace context";
    rec.logger_name = "untrace.logger";
    // trace and span are std::nullopt by default
    rq.records.push_back(std::move(rec));

    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == 1);
    CHECK_FALSE(r->records[0].trace.has_value());
    CHECK_FALSE(r->records[0].span.has_value());
}

TEST_CASE("submit_log_records_request_timestamp_precision", tags) {
    auto lg(make_logger(test_suite));

    submit_log_records_request rq;

    auto now = std::chrono::system_clock::now();
    log_record rec;
    rec.timestamp = now;
    rec.severity = severity_level::info;
    rec.body = "Timestamp test";
    rec.logger_name = "time.logger";
    rq.records.push_back(std::move(rec));

    // Serialize and deserialize
    const auto serialized = rq.serialize();
    const auto r = submit_log_records_request::deserialize(serialized);

    REQUIRE(r.has_value());
    REQUIRE(r->records.size() == 1);

    // Timestamps should match to millisecond precision
    auto expected_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()).count();
    auto actual_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        r->records[0].timestamp.time_since_epoch()).count();
    CHECK(actual_ms == expected_ms);
}

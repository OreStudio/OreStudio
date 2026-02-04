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
#include "ores.telemetry.database/repository/telemetry_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.telemetry/domain/telemetry_log_entry.hpp"

namespace {

const std::string_view test_suite("ores.telemetry.database.tests");
const std::string tags("[repository]");

/**
 * @brief Creates a test telemetry log entry.
 */
ores::telemetry::domain::telemetry_log_entry make_test_entry() {
    boost::uuids::random_generator gen;

    ores::telemetry::domain::telemetry_log_entry entry;
    entry.id = gen();
    entry.timestamp = std::chrono::system_clock::now();
    entry.source = ores::telemetry::domain::telemetry_source::client;
    entry.source_name = "test-application";
    entry.session_id = gen();
    entry.account_id = gen();
    entry.level = "info";
    entry.component = "test.component";
    entry.message = "Test log message from integration test";
    entry.tag = "integration-test";
    entry.recorded_at = std::chrono::system_clock::now();

    return entry;
}

}

using namespace ores::telemetry::domain;
using namespace ores::telemetry::database::repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("create_single_telemetry_entry", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo(h.context());

    auto entry = make_test_entry();
    BOOST_LOG_SEV(lg, debug) << "Entry ID: " << boost::uuids::to_string(entry.id);

    CHECK_NOTHROW(repo.create(entry));

    BOOST_LOG_SEV(lg, debug) << "Telemetry entry created successfully";
}

TEST_CASE("create_and_query_telemetry_entry", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo(h.context());

    auto entry = make_test_entry();
    entry.message = "Unique query test message";
    entry.tag = "query-test";
    BOOST_LOG_SEV(lg, debug) << "Creating entry with ID: "
                             << boost::uuids::to_string(entry.id);

    repo.create(entry);

    // Query for the entry
    telemetry_query q;
    q.start_time = entry.timestamp - std::chrono::hours(1);
    q.end_time = entry.timestamp + std::chrono::hours(1);
    q.tag = "query-test";
    q.limit = 100;

    auto results = repo.query(q);
    BOOST_LOG_SEV(lg, debug) << "Query returned " << results.size() << " entries";

    REQUIRE(results.size() >= 1);

    // Find our specific entry
    auto it = std::find_if(results.begin(), results.end(),
        [&entry](const telemetry_log_entry& e) {
            return e.id == entry.id;
        });

    REQUIRE(it != results.end());
    CHECK(it->message == "Unique query test message");
    CHECK(it->tag == "query-test");
    CHECK(it->source == telemetry_source::client);
    CHECK(it->source_name == "test-application");
}

TEST_CASE("create_batch_telemetry_entries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo(h.context());

    telemetry_batch batch;
    batch.source = telemetry_source::server;
    batch.source_name = "batch-test-service";

    // Add multiple entries
    for (int i = 0; i < 5; ++i) {
        auto entry = make_test_entry();
        entry.message = "Batch entry " + std::to_string(i);
        entry.tag = "batch-test";
        batch.entries.push_back(std::move(entry));
    }

    BOOST_LOG_SEV(lg, debug) << "Creating batch of " << batch.size() << " entries";

    auto count = repo.create_batch(batch);

    CHECK(count == 5);
    BOOST_LOG_SEV(lg, debug) << "Batch created successfully with " << count << " entries";
}

TEST_CASE("read_by_session", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo(h.context());

    boost::uuids::random_generator gen;
    auto session_id = gen();

    // Create entries with the same session
    for (int i = 0; i < 3; ++i) {
        auto entry = make_test_entry();
        entry.session_id = session_id;
        entry.message = "Session test message " + std::to_string(i);
        repo.create(entry);
    }

    BOOST_LOG_SEV(lg, debug) << "Reading entries for session: "
                             << boost::uuids::to_string(session_id);

    auto results = repo.read_by_session(session_id);
    BOOST_LOG_SEV(lg, debug) << "Read " << results.size() << " entries";

    CHECK(results.size() >= 3);

    // All results should have our session_id
    for (const auto& entry : results) {
        if (entry.session_id.has_value()) {
            CHECK(*entry.session_id == session_id);
        }
    }
}

TEST_CASE("count_telemetry_entries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo(h.context());

    auto entry = make_test_entry();
    entry.tag = "count-test";
    repo.create(entry);

    telemetry_query q;
    q.start_time = entry.timestamp - std::chrono::hours(1);
    q.end_time = entry.timestamp + std::chrono::hours(1);
    q.tag = "count-test";

    auto count = repo.count(q);
    BOOST_LOG_SEV(lg, debug) << "Count: " << count;

    CHECK(count >= 1);
}

TEST_CASE("get_telemetry_summary", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo(h.context());

    // Create entries at different levels
    auto info_entry = make_test_entry();
    info_entry.level = "info";
    repo.create(info_entry);

    auto error_entry = make_test_entry();
    error_entry.level = "error";
    repo.create(error_entry);

    auto summary = repo.get_summary(24);
    BOOST_LOG_SEV(lg, debug) << "Summary: total=" << summary.total_logs
                             << " errors=" << summary.error_count;

    CHECK(summary.total_logs >= 2);
}

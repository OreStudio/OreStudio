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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.telemetry.core/messaging/logs_protocol.hpp"
#include "ores.telemetry.database/repository/telemetry_repository.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <set>

namespace {

const std::string_view test_suite("ores.telemetry.database.tests");
const std::string tags("[repository]");

/**
 * @brief Creates a test telemetry log entry.
 */
ores::telemetry::messaging::telemetry_log_entry make_test_entry() {
    boost::uuids::random_generator gen;

    ores::telemetry::messaging::telemetry_log_entry entry;
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

/**
 * @brief Builds a query that selects one tag over a one-hour window.
 */
ores::telemetry::messaging::telemetry_query
make_tag_query(const std::string& tag, const std::chrono::system_clock::time_point& reference) {
    ores::telemetry::messaging::telemetry_query q;
    q.start_time = reference - std::chrono::hours(1);
    q.end_time = reference + std::chrono::hours(1);
    q.tag = tag;
    q.limit = 100;
    return q;
}

}

using namespace ores::telemetry::domain;
using namespace ores::telemetry::messaging;
using namespace ores::telemetry::database::repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("create_single_telemetry_entry", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo;

    auto entry = make_test_entry();
    BOOST_LOG_SEV(lg, debug) << "Entry ID: " << boost::uuids::to_string(entry.id);

    repo.create(h.context(), entry);

    const auto results =
        repo.query(h.context(), make_tag_query("integration-test", entry.timestamp));

    REQUIRE(results.size() == 1);
    CHECK(results[0].message == "Test log message from integration test");
    CHECK(results[0].component == "test.component");
    CHECK(results[0].source_name == "test-application");
    CHECK(results[0].source == telemetry_source::client);
    CHECK(results[0].level == "info");

    BOOST_LOG_SEV(lg, debug) << "Telemetry entry created and read back successfully";
}

TEST_CASE("create_and_query_telemetry_entry", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo;

    auto entry = make_test_entry();
    entry.message = "Unique query test message";
    entry.tag = "query-test";
    BOOST_LOG_SEV(lg, debug) << "Creating entry with ID: " << boost::uuids::to_string(entry.id);

    repo.create(h.context(), entry);

    // Query for the entry
    telemetry_query q;
    q.start_time = entry.timestamp - std::chrono::hours(1);
    q.end_time = entry.timestamp + std::chrono::hours(1);
    q.tag = "query-test";
    q.limit = 100;

    auto results = repo.query(h.context(), q);
    BOOST_LOG_SEV(lg, debug) << "Query returned " << results.size() << " entries";

    REQUIRE(results.size() >= 1);

    // Find our specific entry
    auto it = std::find_if(results.begin(), results.end(), [&entry](const telemetry_log_entry& e) {
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
    telemetry_repository repo;

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

    auto count = repo.create_batch(h.context(), batch);

    CHECK(count == 5);

    const auto results =
        repo.query(h.context(), make_tag_query("batch-test", std::chrono::system_clock::now()));

    REQUIRE(results.size() == 5);
    std::set<std::string> messages;
    for (const auto& entry : results) {
        CHECK(entry.tag == "batch-test");
        CHECK(entry.source == telemetry_source::server);
        CHECK(entry.source_name == "batch-test-service");
        messages.insert(entry.message);
    }
    CHECK(messages ==
          std::set<std::string>{
              "Batch entry 0", "Batch entry 1", "Batch entry 2", "Batch entry 3", "Batch entry 4"});

    BOOST_LOG_SEV(lg, debug) << "Batch created and read back successfully";
}

TEST_CASE("read_by_session", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo;

    boost::uuids::random_generator gen;
    auto session_id = gen();

    // Create entries with the same session
    for (int i = 0; i < 3; ++i) {
        auto entry = make_test_entry();
        entry.session_id = session_id;
        entry.tag = "session-test";
        entry.message = "Session test message " + std::to_string(i);
        repo.create(h.context(), entry);
    }

    BOOST_LOG_SEV(lg, debug) << "Reading entries for session: "
                             << boost::uuids::to_string(session_id);

    auto results = repo.read_by_session(h.context(), session_id);
    BOOST_LOG_SEV(lg, debug) << "Read " << results.size() << " entries";

    REQUIRE(results.size() == 3);

    std::set<std::string> messages;
    for (const auto& entry : results) {
        REQUIRE(entry.session_id.has_value());
        CHECK(*entry.session_id == session_id);
        messages.insert(entry.message);
    }
    CHECK(messages == std::set<std::string>{"Session test message 0",
                                            "Session test message 1",
                                            "Session test message 2"});
}

TEST_CASE("count_telemetry_entries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo;

    auto entry = make_test_entry();
    entry.tag = "count-test";

    const auto q = make_tag_query("count-test", entry.timestamp);
    const auto before = repo.count(h.context(), q);

    repo.create(h.context(), entry);

    const auto after = repo.count(h.context(), q);
    BOOST_LOG_SEV(lg, debug) << "Count before: " << before << ", after: " << after;

    CHECK(before == 0);
    CHECK(after == 1);
    CHECK(after == before + 1);
}

TEST_CASE("get_telemetry_summary", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo;

    const auto before = repo.get_summary(h.context(), 24);

    // Create entries at different levels
    auto info_entry = make_test_entry();
    info_entry.level = "info";
    info_entry.tag = "summary-test-info";
    repo.create(h.context(), info_entry);

    auto error_entry = make_test_entry();
    error_entry.level = "error";
    error_entry.tag = "summary-test-error";
    repo.create(h.context(), error_entry);

    const auto after = repo.get_summary(h.context(), 24);
    BOOST_LOG_SEV(lg, debug) << "Summary delta: total=" << (after.total_logs - before.total_logs)
                             << " errors=" << (after.error_count - before.error_count);

    CHECK(after.total_logs - before.total_logs == 2);
    CHECK(after.error_count - before.error_count == 1);
}

TEST_CASE("create_and_list_service_sample", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo;

    boost::uuids::random_generator gen;
    service_sample sample;
    sample.sampled_at = std::chrono::system_clock::now();
    sample.service_name = "ores.test.service";
    sample.instance_id = boost::uuids::to_string(gen());
    sample.host_id = boost::uuids::to_string(gen());
    sample.version = "1.0.0-test";
    BOOST_LOG_SEV(lg, debug) << "Instance ID: " << sample.instance_id;

    repo.insert_service_sample(h.context(), sample);

    const auto samples = repo.list_service_samples(h.context());
    const auto it = std::find_if(samples.begin(), samples.end(), [&](const service_sample& s) {
        return s.instance_id == sample.instance_id;
    });

    REQUIRE(it != samples.end());
    CHECK(it->service_name == sample.service_name);
    CHECK(it->host_id == sample.host_id);
    CHECK(it->version == sample.version);

    BOOST_LOG_SEV(lg, debug) << "Service sample read back with host id " << it->host_id;
}

TEST_CASE("list_service_roster_states_every_expected_instance", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    telemetry_repository repo;

    boost::uuids::random_generator gen;
    const auto suffix = boost::uuids::to_string(gen());
    const auto reporting = "ores.test.roster-" + suffix + ".service";
    const auto silent = "ores.test.silent-" + suffix + ".service";
    const std::string insert_expected =
        "INSERT INTO ores_telemetry_expected_services_tbl "
        "(service_name, replicas, display_name, description, service_account) "
        "VALUES ($1, $2::integer, $3, $4, nullif($5, ''))";
    ores::database::repository::execute_parameterized_command(
        h.context(),
        insert_expected,
        {reporting, "2", "Roster Test Service", "Reports for the test.", "roster_test_user"},
        lg,
        "Expecting the reporting service");
    ores::database::repository::execute_parameterized_command(
        h.context(),
        insert_expected,
        {silent, "1", "Silent Test Service", "Never reports.", ""},
        lg,
        "Expecting the silent service");

    const auto now = std::chrono::system_clock::now();
    const auto report = [&](const std::string& instance, std::chrono::minutes age) {
        service_sample sample;
        sample.sampled_at = now - age;
        sample.service_name = reporting;
        sample.instance_id = instance;
        sample.host_id = "roster-test-host";
        sample.version = "1.0.0-test";
        repo.insert_service_sample(h.context(), sample);
    };
    report("leftover", std::chrono::minutes(180));
    report("quiet", std::chrono::minutes(10));
    report("fresh", std::chrono::minutes(1));

    const auto roster = repo.list_service_roster(h.context(), now);
    std::vector<service_roster_slot> reporting_slots;
    std::vector<service_roster_slot> silent_slots;
    for (const auto& slot : roster) {
        if (slot.service_name == reporting)
            reporting_slots.push_back(slot);
        else if (slot.service_name == silent)
            silent_slots.push_back(slot);
    }
    BOOST_LOG_SEV(lg, debug) << "Roster slots: reporting=" << reporting_slots.size()
                             << " silent=" << silent_slots.size();

    REQUIRE(reporting_slots.size() == 2);
    CHECK(reporting_slots[0].display_name == "Roster Test Service");
    CHECK(reporting_slots[0].description == "Reports for the test.");
    CHECK(reporting_slots[0].service_account == std::optional<std::string>("roster_test_user"));
    CHECK(reporting_slots[0].slot == 1);
    CHECK(reporting_slots[0].instance_id == std::optional<std::string>("fresh"));
    CHECK(reporting_slots[0].state == ores::telemetry::domain::service_state::running);
    CHECK(reporting_slots[1].slot == 2);
    CHECK(reporting_slots[1].instance_id == std::optional<std::string>("quiet"));
    CHECK(reporting_slots[1].state == ores::telemetry::domain::service_state::lost);
    CHECK(reporting_slots[1].host_id == std::optional<std::string>("roster-test-host"));
    CHECK(reporting_slots[1].sampled_at.has_value());

    REQUIRE(silent_slots.size() == 1);
    CHECK(silent_slots[0].slot == 1);
    CHECK_FALSE(silent_slots[0].service_account.has_value());
    CHECK(silent_slots[0].state == ores::telemetry::domain::service_state::missing);
    CHECK_FALSE(silent_slots[0].instance_id.has_value());
    CHECK_FALSE(silent_slots[0].sampled_at.has_value());

    ores::database::repository::execute_parameterized_command(
        h.context(),
        "DELETE FROM ores_telemetry_expected_services_tbl WHERE service_name IN ($1, $2)",
        {reporting, silent},
        lg,
        "Removing the test's expected services");
}

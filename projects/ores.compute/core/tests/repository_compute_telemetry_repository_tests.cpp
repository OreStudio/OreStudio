/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.compute.api/generators/batch_generator.hpp"
#include "ores.compute.api/generators/grid_sample_generator.hpp"
#include "ores.compute.api/generators/node_sample_generator.hpp"
#include "ores.compute.core/repository/batch_repository.hpp"
#include "ores.compute.core/repository/compute_telemetry_repository.hpp"
#include "ores.compute.core/repository/grid_sample_repository.hpp"
#include "ores.compute.core/repository/node_sample_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <format>
#include <optional>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>

// The installation's fleet samples carry :rls_own_or_system_tenant_rows:, so
// a session reads its own tenant's rows and the system tenant's. The two reads
// below state no tenant predicate; the policy decides. A third tenant's rows
// are correctly not visible, because the policy admits only those two.

namespace {

const std::string test_suite("ores.compute.tests");
const std::string tags("[repository]");

boost::uuids::uuid random_uuid() {
    static boost::uuids::random_generator gen;
    return gen();
}

/// The host ids the read returned, as strings, for membership checks.
std::vector<std::string> host_ids_of(const std::vector<ores::compute::domain::node_sample>& rows) {
    std::vector<std::string> ids;
    ids.reserve(rows.size());
    for (const auto& row : rows)
        ids.push_back(boost::uuids::to_string(row.host_id));
    return ids;
}

/// Deletes the sample rows a test wrote when the test ends.
///
/// The tests run against the development database, so a row they leave behind
/// is a row the grid screen shows: a future-dated grid sample hid the real
/// newest one, and a generated node sample showed as a machine that does not
/// exist. A row is deleted as the tenant that wrote it, because the policy lets
/// a session delete only its own tenant's rows.
class sample_cleanup {
public:
    explicit sample_cleanup(ores::testing::database_helper& h)
        : h_(h) {}
    sample_cleanup(const sample_cleanup&) = delete;
    sample_cleanup& operator=(const sample_cleanup&) = delete;

    ~sample_cleanup() {
        for (const auto& step : steps_) {
            try {
                ores::database::repository::execute_parameterized_command(
                    h_.context().with_tenant(step.tenant, h_.db_user()),
                    step.sql,
                    step.params,
                    logger(),
                    "deleting a test sample");
            } catch (const std::exception&) {
                // A failed delete leaves a row behind and must not fail the test.
            }
        }
    }

    /// One grid sample row, by its id.
    void grid(const ores::utility::uuid::tenant_id& tenant, const boost::uuids::uuid& id) {
        steps_.push_back({tenant,
                          "DELETE FROM ores_compute_grid_samples_tbl WHERE id = $1::uuid",
                          {boost::uuids::to_string(id)}});
    }

    /// One node sample row, by its id.
    void node(const ores::utility::uuid::tenant_id& tenant, const boost::uuids::uuid& id) {
        steps_.push_back({tenant,
                          "DELETE FROM ores_compute_node_samples_tbl WHERE id = $1::uuid",
                          {boost::uuids::to_string(id)}});
    }

    /// One batch row with all its versions, by its id.
    void batch(const ores::utility::uuid::tenant_id& tenant, const boost::uuids::uuid& id) {
        steps_.push_back({tenant,
                          "DELETE FROM ores_compute_batches_tbl WHERE id = $1::uuid",
                          {boost::uuids::to_string(id)}});
    }

    /// Every row of a table that one tenant wrote in a time range.
    void range(const std::string& table,
               const ores::utility::uuid::tenant_id& tenant,
               std::chrono::system_clock::time_point from,
               std::chrono::system_clock::time_point to) {
        namespace dt = ores::platform::time;
        steps_.push_back(
            {tenant,
             std::format("DELETE FROM {} WHERE tenant_id = $1::uuid AND "
                         "sampled_at >= $2::timestamptz AND sampled_at < $3::timestamptz",
                         table),
             {tenant.to_string(),
              dt::datetime::to_db_string(from),
              dt::datetime::to_db_string(to)}});
    }

private:
    struct step {
        ores::utility::uuid::tenant_id tenant;
        std::string sql;
        std::vector<std::string> params;
    };

    static ores::logging::logger_t& logger() {
        static auto instance = ores::logging::make_logger("ores.compute.tests.sample_cleanup");
        return instance;
    }

    ores::testing::database_helper& h_;
    std::vector<step> steps_;
};

/// The generator leaves the 64-bit members of a node sample indeterminate, because
/// the codegen gives a default to int and bool alone. A test that writes the row
/// as it is would store stack garbage, so it states them.
ores::compute::domain::node_sample with_defined_counters(ores::compute::domain::node_sample row) {
    row.avg_task_duration_ms = 0;
    row.max_task_duration_ms = 0;
    row.input_bytes_fetched = 0;
    row.output_bytes_uploaded = 0;
    return row;
}

} // namespace

using namespace ores::compute::generators;
using namespace ores::logging;
using ores::compute::domain::grid_sample;
using ores::compute::domain::node_sample;
using ores::compute::repository::compute_telemetry_repository;
using ores::compute::repository::grid_sample_repository;
using ores::compute::repository::node_sample_repository;
using ores::testing::database_helper;
using ores::testing::test_database_manager;
using ores::utility::uuid::tenant_id;

TEST_CASE("read_latest_node_samples_sees_own_and_system_tenants_but_not_a_third", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    const auto own = h.tenant_id();
    const auto system = tenant_id::system();

    // provision_test_tenant switches its context argument to the system
    // tenant, so it gets a copy and never the helper's own context.
    auto provision_ctx = h.context();
    const auto third_code =
        test_database_manager::generate_test_tenant_code("ores.compute.telemetry");
    const auto third =
        tenant_id::from_string(test_database_manager::provision_test_tenant(
                                   provision_ctx, third_code, "compute telemetry third tenant"))
            .value();

    auto own_row = with_defined_counters(generate_synthetic_node_sample(gen));
    own_row.tenant_id = own;
    own_row.host_id = random_uuid();
    own_row.sampled_at = std::chrono::system_clock::now();

    auto system_row = with_defined_counters(generate_synthetic_node_sample(gen));
    system_row.tenant_id = system;
    system_row.host_id = random_uuid();
    system_row.sampled_at = std::chrono::system_clock::now();

    auto third_row = with_defined_counters(generate_synthetic_node_sample(gen));
    third_row.tenant_id = third;
    third_row.host_id = random_uuid();
    third_row.sampled_at = std::chrono::system_clock::now();

    node_sample_repository repo;
    sample_cleanup cleanup(h);
    cleanup.node(own, own_row.id);
    cleanup.node(system, system_row.id);
    cleanup.node(third, third_row.id);
    repo.write(h.context(), own_row);
    repo.write(h.context().with_tenant(system, h.db_user()), system_row);
    repo.write(h.context().with_tenant(third, h.db_user()), third_row);

    BOOST_LOG_SEV(lg, debug) << "Own host: " << boost::uuids::to_string(own_row.host_id)
                             << " system host: " << boost::uuids::to_string(system_row.host_id)
                             << " third host: " << boost::uuids::to_string(third_row.host_id);

    compute_telemetry_repository telemetry;
    const auto hosts = host_ids_of(telemetry.latest_node_samples(h.context()));
    BOOST_LOG_SEV(lg, info) << "The fleet read returned " << hosts.size()
                            << " host(s); it must include the session's own "
                            << "tenant and the system tenant, and no third tenant.";
    BOOST_LOG_SEV(lg, debug) << "Read host count: " << hosts.size();

    CHECK(std::ranges::contains(hosts, boost::uuids::to_string(own_row.host_id)));
    CHECK(std::ranges::contains(hosts, boost::uuids::to_string(system_row.host_id)));
    CHECK(!std::ranges::contains(hosts, boost::uuids::to_string(third_row.host_id)));
}

TEST_CASE("read_newest_grid_sample_returns_a_system_row_not_a_newer_third_tenant_row", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    const auto own = h.tenant_id();
    const auto system = tenant_id::system();

    auto provision_ctx = h.context();
    const auto third_code =
        test_database_manager::generate_test_tenant_code("ores.compute.telemetry");
    const auto third =
        tenant_id::from_string(test_database_manager::provision_test_tenant(
                                   provision_ctx, third_code, "compute telemetry third tenant"))
            .value();

    const auto now = std::chrono::system_clock::now();

    auto own_row = generate_synthetic_grid_sample(gen);
    own_row.tenant_id = own;
    own_row.sampled_at = now;

    // The system row is newer than the session's own, so a read that still
    // filtered on the session's tenant would return the own row. The third
    // tenant's row is newer still, and the policy hides it.
    auto system_row = generate_synthetic_grid_sample(gen);
    system_row.tenant_id = system;
    system_row.sampled_at = now + std::chrono::hours(24 * 365);

    auto third_row = generate_synthetic_grid_sample(gen);
    third_row.tenant_id = third;
    third_row.sampled_at = now + std::chrono::hours(24 * 365 * 2);

    grid_sample_repository repo;
    sample_cleanup cleanup(h);
    cleanup.grid(own, own_row.id);
    cleanup.grid(system, system_row.id);
    cleanup.grid(third, third_row.id);
    repo.write(h.context(), own_row);
    repo.write(h.context().with_tenant(system, h.db_user()), system_row);
    repo.write(h.context().with_tenant(third, h.db_user()), third_row);

    BOOST_LOG_SEV(lg, debug) << "Own id: " << boost::uuids::to_string(own_row.id)
                             << " system id: " << boost::uuids::to_string(system_row.id)
                             << " third id: " << boost::uuids::to_string(third_row.id);

    const auto newest = repo.read_newest(h.context());
    REQUIRE(newest.has_value());
    BOOST_LOG_SEV(lg, info) << "The newest grid sample belongs to tenant "
                            << newest->tenant_id.to_string()
                            << "; it must be the system tenant's, not a newer "
                            << "third tenant's row.";
    BOOST_LOG_SEV(lg, debug) << "Newest id: " << boost::uuids::to_string(newest->id);

    CHECK(newest->id != own_row.id);
    CHECK(newest->id != third_row.id);
    CHECK(newest->tenant_id == system);
}

namespace {

/// A bucket-aligned instant at random in the past, so no other row or test shares the range.
///
/// The rows a test writes stay in the database, and a row in the future would
/// become the newest sample that other tests read. So the range lies between
/// 1990 and 2010, and a repeated run draws another range.
std::chrono::system_clock::time_point random_past_bucket_start(std::chrono::seconds bucket) {
    static std::mt19937_64 engine{std::random_device{}()};
    constexpr long long from_1990 = 631152000;
    constexpr long long twenty_years = 20LL * 365 * 24 * 3600;
    const long long secs = from_1990 + static_cast<long long>(engine() % twenty_years);
    const long long aligned = secs / bucket.count() * bucket.count();
    return std::chrono::system_clock::time_point(std::chrono::seconds(aligned));
}

grid_sample sample_at(const grid_sample& model,
                      const tenant_id& tenant,
                      std::chrono::system_clock::time_point at,
                      int results_done) {
    auto row = model;
    row.id = random_uuid();
    row.tenant_id = tenant;
    row.sampled_at = at;
    row.results_done = results_done;
    return row;
}

} // namespace

TEST_CASE("grid_series_holds_the_newest_sample_of_each_bucket_and_marks_the_gaps", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto model = generate_synthetic_grid_sample(gen);

    auto provision_ctx = h.context();
    const auto third =
        tenant_id::from_string(
            test_database_manager::provision_test_tenant(
                provision_ctx,
                test_database_manager::generate_test_tenant_code("ores.compute.telemetry"),
                "compute telemetry third tenant"))
            .value();

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));

    // Bucket 0 holds three samples, bucket 1 none, bucket 2 one, bucket 3 none.
    // A third tenant's newer row sits in bucket 2 and the policy hides it. One
    // more row lies past the end of the range.
    grid_sample_repository repo;
    const auto own = h.tenant_id();
    sample_cleanup cleanup(h);
    cleanup.range("ores_compute_grid_samples_tbl", own, t0, t0 + seconds(400));
    cleanup.range("ores_compute_grid_samples_tbl", third, t0, t0 + seconds(400));
    repo.write(h.context(), sample_at(model, own, t0 + seconds(5), 1));
    repo.write(h.context(), sample_at(model, own, t0 + seconds(35), 3));
    repo.write(h.context(), sample_at(model, own, t0 + seconds(20), 2));
    repo.write(h.context(), sample_at(model, own, t0 + seconds(130), 7));
    repo.write(h.context().with_tenant(third, h.db_user()),
               sample_at(model, third, t0 + seconds(150), 99));
    repo.write(h.context(), sample_at(model, own, t0 + seconds(300), 50));

    compute_telemetry_repository telemetry;
    const auto series = telemetry.grid_series_in_range(h.context(), t0, t0 + seconds(240), 60);

    BOOST_LOG_SEV(lg, info) << "The series has " << series.bucket_count
                            << " bucket(s); each column must hold the newest sample of its "
                               "bucket, and the empty buckets must say they hold no sample.";

    CHECK(series.start == t0);
    CHECK(series.bucket_seconds == 60);
    REQUIRE(series.bucket_count == 4);
    REQUIRE(series.samples.size() == 4);
    REQUIRE(series.results_done.size() == 4);
    CHECK(series.samples == std::vector<int>{3, 0, 1, 0});
    CHECK(series.results_done == std::vector<int>{3, 0, 7, 0});
}

namespace {

/// A row of one party's job counters, with the host counts left at zero.
grid_sample party_row(const grid_sample& model,
                      const tenant_id& tenant,
                      const std::optional<boost::uuids::uuid>& party,
                      std::chrono::system_clock::time_point at,
                      int unsent,
                      int done) {
    auto row = model;
    row.id = random_uuid();
    row.tenant_id = tenant;
    row.party_id = party;
    row.sampled_at = at;
    row.total_hosts = 0;
    row.online_hosts = 0;
    row.idle_hosts = 0;
    row.results_inactive = 0;
    row.results_unsent = unsent;
    row.results_in_progress = 0;
    row.results_done = done;
    row.total_workunits = 0;
    row.total_batches = 0;
    row.active_batches = 0;
    row.outcomes_success = 0;
    row.outcomes_client_error = 0;
    row.outcomes_no_reply = 0;
    return row;
}

} // namespace

TEST_CASE("grid_series_sums_the_party_rows_of_an_instant_and_takes_the_fleet_from_the_system", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto model = generate_synthetic_grid_sample(gen);
    const auto own = h.tenant_id();
    const auto system = tenant_id::system();
    const auto party_a = random_uuid();
    const auto party_b = random_uuid();

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));
    const auto at = t0 + seconds(10);

    // One instant holds a row for each party and one for the work with no party. The
    // fleet is on the system tenant's row, which a tenant's session also reads.
    grid_sample_repository repo;
    sample_cleanup cleanup(h);
    cleanup.range("ores_compute_grid_samples_tbl", own, t0, t0 + seconds(120));
    cleanup.range("ores_compute_grid_samples_tbl", system, t0, t0 + seconds(120));
    repo.write(h.context(), party_row(model, own, party_a, at, 2, 4));
    repo.write(h.context(), party_row(model, own, party_b, at, 3, 6));
    repo.write(h.context(), party_row(model, own, std::nullopt, at, 1, 1));
    auto fleet = party_row(model, system, std::nullopt, at, 100, 100);
    fleet.total_hosts = 9;
    fleet.online_hosts = 7;
    fleet.idle_hosts = 3;
    repo.write(h.context().with_tenant(system, h.db_user()), fleet);

    compute_telemetry_repository telemetry;
    const auto series = telemetry.grid_series_in_range(h.context(), t0, t0 + seconds(60), 60);

    REQUIRE(series.bucket_count == 1);
    CHECK(series.samples == std::vector<int>{1});
    // The tenant's counters are the sum of its parties' rows and none of the system's.
    CHECK(series.results_unsent == std::vector<int>{6});
    CHECK(series.results_done == std::vector<int>{11});
    // The capacity is the installation's.
    CHECK(series.total_hosts == std::vector<int>{9});
    CHECK(series.online_hosts == std::vector<int>{7});
    CHECK(series.idle_hosts == std::vector<int>{3});
}

TEST_CASE("grid_series_of_a_party_session_sums_only_the_parties_it_acts_for", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto model = generate_synthetic_grid_sample(gen);
    const auto own = h.tenant_id();
    const auto party_a = random_uuid();
    const auto party_b = random_uuid();

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));
    const auto at = t0 + seconds(10);

    grid_sample_repository repo;
    sample_cleanup cleanup(h);
    cleanup.range("ores_compute_grid_samples_tbl", own, t0, t0 + seconds(120));
    repo.write(h.context(), party_row(model, own, party_a, at, 2, 4));
    repo.write(h.context(), party_row(model, own, party_b, at, 3, 6));
    repo.write(h.context(), party_row(model, own, std::nullopt, at, 1, 1));

    compute_telemetry_repository telemetry;
    const auto as_a = h.context().with_party(own, party_a, {party_a}, h.db_user());
    const auto series = telemetry.grid_series_in_range(as_a, t0, t0 + seconds(60), 60);

    // Party A's row and the row with no party, and never party B's.
    CHECK(series.results_unsent == std::vector<int>{3});
    CHECK(series.results_done == std::vector<int>{5});
}

TEST_CASE("parties_with_work_always_lists_the_empty_party", tags) {
    database_helper h;

    compute_telemetry_repository telemetry;
    const auto parties = telemetry.parties_with_work(h.context());

    REQUIRE(!parties.empty());
    CHECK(!parties.front().has_value());
}

TEST_CASE("grid_series_widens_the_range_to_whole_buckets", tags) {
    database_helper h;

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));

    compute_telemetry_repository telemetry;
    const auto series =
        telemetry.grid_series_in_range(h.context(), t0 + seconds(30), t0 + seconds(150), 60);

    CHECK(series.start == t0);
    CHECK(series.bucket_count == 3);
}

TEST_CASE("grid_series_of_an_empty_range_says_every_bucket_holds_no_sample", tags) {
    database_helper h;

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));

    compute_telemetry_repository telemetry;
    const auto series = telemetry.grid_series_in_range(h.context(), t0, t0 + seconds(600), 60);

    REQUIRE(series.bucket_count == 10);
    CHECK(series.samples == std::vector<int>(10, 0));
}

TEST_CASE("grid_series_refuses_a_bad_bucket_and_a_range_that_needs_too_many", tags) {
    database_helper h;

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));

    compute_telemetry_repository telemetry;
    CHECK_THROWS_AS(telemetry.grid_series_in_range(h.context(), t0, t0 + seconds(600), 0),
                    std::invalid_argument);
    CHECK_THROWS_AS(telemetry.grid_series_in_range(h.context(), t0 + seconds(60), t0, 60),
                    std::invalid_argument);
    CHECK_THROWS_AS(telemetry.grid_series_in_range(h.context(), t0, t0 + seconds(6000 * 60), 60),
                    std::invalid_argument);
}

namespace {

node_sample node_sample_at(const node_sample& model,
                           const tenant_id& tenant,
                           const boost::uuids::uuid& host,
                           const std::string& machine,
                           std::chrono::system_clock::time_point at,
                           int completed,
                           int failed,
                           int since_last,
                           std::int64_t avg_ms,
                           std::int64_t max_ms,
                           std::int64_t fetched,
                           std::int64_t uploaded) {
    auto row = model;
    row.id = random_uuid();
    row.tenant_id = tenant;
    row.host_id = host;
    row.machine = machine;
    row.sampled_at = at;
    row.tasks_completed = completed;
    row.tasks_failed = failed;
    row.tasks_since_last = since_last;
    row.avg_task_duration_ms = avg_ms;
    row.max_task_duration_ms = max_ms;
    row.input_bytes_fetched = fetched;
    row.output_bytes_uploaded = uploaded;
    return row;
}

} // namespace

TEST_CASE("node_series_sums_the_interval_counters_and_orders_the_nodes_by_machine", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto model = with_defined_counters(generate_synthetic_node_sample(gen));

    auto provision_ctx = h.context();
    const auto third =
        tenant_id::from_string(
            test_database_manager::provision_test_tenant(
                provision_ctx,
                test_database_manager::generate_test_tenant_code("ores.compute.telemetry"),
                "compute telemetry third tenant"))
            .value();

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));
    const auto own = h.tenant_id();

    const auto beta_node = random_uuid();
    const auto alpha_node = random_uuid();
    const auto hidden_node = random_uuid();

    // The beta node has two samples in bucket 0, the first naming no machine,
    // and one in bucket 2. The alpha node has one sample in bucket 1. A third
    // tenant's node reports in bucket 0 and the policy hides it.
    node_sample_repository repo;
    sample_cleanup cleanup(h);
    cleanup.range("ores_compute_node_samples_tbl", own, t0, t0 + seconds(400));
    cleanup.range("ores_compute_node_samples_tbl", third, t0, t0 + seconds(400));
    repo.write(
        h.context(),
        node_sample_at(model, own, beta_node, "", t0 + seconds(10), 5, 1, 2, 100, 150, 10, 1));
    repo.write(
        h.context(),
        node_sample_at(model, own, beta_node, "beta", t0 + seconds(40), 7, 1, 3, 200, 400, 20, 2));
    repo.write(
        h.context(),
        node_sample_at(model, own, beta_node, "beta", t0 + seconds(125), 9, 2, 2, 50, 60, 5, 5));
    repo.write(h.context(),
               node_sample_at(
                   model, own, alpha_node, "alpha", t0 + seconds(70), 4, 0, 4, 10, 20, 100, 200));
    repo.write(
        h.context().with_tenant(third, h.db_user()),
        node_sample_at(model, third, hidden_node, "gamma", t0 + seconds(15), 1, 0, 1, 1, 1, 1, 1));

    compute_telemetry_repository telemetry;
    const auto series = telemetry.node_series_in_range(h.context(), t0, t0 + seconds(180), 60);

    BOOST_LOG_SEV(lg, info) << "The node series lists " << series.nodes.size() << " node(s) over "
                            << series.bucket_count << " bucket(s).";

    REQUIRE(series.bucket_count == 3);
    REQUIRE(series.nodes.size() == 2);
    // Ordered by machine: alpha before beta, and the hidden node absent.
    CHECK(series.nodes[0].host_id == boost::uuids::to_string(alpha_node));
    CHECK(series.nodes[0].machine == "alpha");
    CHECK(series.nodes[1].host_id == boost::uuids::to_string(beta_node));
    CHECK(series.nodes[1].machine == "beta");

    REQUIRE(series.samples.size() == 6);
    // Row 0 is alpha: a sample in bucket 1 only.
    CHECK(series.samples == std::vector<int>{0, 1, 0, 2, 0, 1});
    CHECK(series.tasks_since_last == std::vector<int>{0, 4, 0, 5, 0, 2});
    CHECK(series.input_bytes_fetched == std::vector<std::int64_t>{0, 100, 0, 30, 0, 5});
    CHECK(series.output_bytes_uploaded == std::vector<std::int64_t>{0, 200, 0, 3, 0, 5});
    // The counters that count from the wrapper's start take the newest value.
    CHECK(series.tasks_completed == std::vector<int>{0, 4, 0, 7, 0, 9});
    CHECK(series.tasks_failed == std::vector<int>{0, 0, 0, 1, 0, 2});
    // The longest task is the maximum, and the mean is weighted by the tasks:
    // (100 * 2 + 200 * 3) / 5 = 160.
    CHECK(series.max_task_duration_ms == std::vector<std::int64_t>{0, 20, 0, 400, 0, 60});
    CHECK(series.avg_task_duration_ms == std::vector<std::int64_t>{0, 10, 0, 160, 0, 50});
}

TEST_CASE("node_series_of_an_empty_range_lists_no_nodes", tags) {
    database_helper h;

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));

    compute_telemetry_repository telemetry;
    const auto series = telemetry.node_series_in_range(h.context(), t0, t0 + seconds(600), 60);

    CHECK(series.bucket_count == 10);
    CHECK(series.nodes.empty());
    CHECK(series.samples.empty());
}

TEST_CASE("node_series_refuses_a_bad_bucket_and_an_empty_range", tags) {
    database_helper h;

    using std::chrono::seconds;
    const auto t0 = random_past_bucket_start(seconds(60));

    compute_telemetry_repository telemetry;
    CHECK_THROWS_AS(telemetry.node_series_in_range(h.context(), t0, t0 + seconds(600), 0),
                    std::invalid_argument);
    CHECK_THROWS_AS(telemetry.node_series_in_range(h.context(), t0 + seconds(60), t0, 60),
                    std::invalid_argument);
}

namespace {

/// The position of a group key in the usage, or -1 when the group is absent.
long usage_index_of(const ores::compute::repository::grid_usage& usage, const std::string& key) {
    const auto it = std::ranges::find(usage.keys, key);
    return it == usage.keys.end() ? -1 : std::distance(usage.keys.begin(), it);
}

/// A batch of the tenant, submitted by the party, or by none when the party is nil.
ores::compute::domain::batch usage_batch(ores::utility::generation::generation_context& gen,
                                         const tenant_id& tenant,
                                         const boost::uuids::uuid& party) {
    auto batch = ores::compute::generators::generate_synthetic_batch(gen);
    batch.tenant_id = tenant;
    batch.id = random_uuid();
    if (!party.is_nil())
        batch.party_id = party;
    else
        batch.party_id.reset();
    return batch;
}

}

TEST_CASE("grid_usage_of_a_tenant_session_groups_its_batches_by_party", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto own = h.tenant_id();
    const auto party_a = random_uuid();
    const auto party_b = random_uuid();

    ores::compute::repository::batch_repository repo;
    sample_cleanup cleanup(h);
    const auto a1 = usage_batch(gen, own, party_a);
    const auto a2 = usage_batch(gen, own, party_a);
    const auto b1 = usage_batch(gen, own, party_b);
    const auto none = usage_batch(gen, own, boost::uuids::nil_uuid());
    for (const auto* batch : {&a1, &a2, &b1, &none}) {
        cleanup.batch(own, batch->id);
        repo.write(h.context(), *batch);
    }

    compute_telemetry_repository telemetry;
    const auto usage = telemetry.grid_usage_by_scope(h.context());

    CHECK(usage.group_kind == "party");
    const auto a = usage_index_of(usage, boost::uuids::to_string(party_a));
    const auto b = usage_index_of(usage, boost::uuids::to_string(party_b));
    REQUIRE(a >= 0);
    REQUIRE(b >= 0);
    CHECK(usage.batches[static_cast<std::size_t>(a)] == 2);
    CHECK(usage.batches[static_cast<std::size_t>(b)] == 1);
    CHECK(usage_index_of(usage, "") >= 0);
    // A group that waits on nothing has a wait of zero, and every column has a row for each group.
    REQUIRE(usage.oldest_unsent_seconds.size() == usage.keys.size());
    CHECK(usage.oldest_unsent_seconds[static_cast<std::size_t>(a)] == 0);
}

TEST_CASE("grid_usage_of_a_session_that_acts_for_a_party_hides_the_other_parties", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto own = h.tenant_id();
    const auto party_a = random_uuid();
    const auto party_b = random_uuid();

    ores::compute::repository::batch_repository repo;
    sample_cleanup cleanup(h);
    const auto a1 = usage_batch(gen, own, party_a);
    const auto b1 = usage_batch(gen, own, party_b);
    for (const auto* batch : {&a1, &b1}) {
        cleanup.batch(own, batch->id);
        repo.write(h.context(), *batch);
    }

    compute_telemetry_repository telemetry;
    const auto as_a = h.context().with_party(own, party_a, {party_a}, h.db_user());
    const auto usage = telemetry.grid_usage_by_scope(as_a);

    CHECK(usage.group_kind == "party");
    CHECK(usage_index_of(usage, boost::uuids::to_string(party_a)) >= 0);
    CHECK(usage_index_of(usage, boost::uuids::to_string(party_b)) == -1);
}

TEST_CASE("grid_usage_reads_one_tenant_and_a_system_session_reads_no_other", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto own = h.tenant_id();
    const auto system = tenant_id::system();

    auto provision_ctx = h.context();
    const auto third_code =
        test_database_manager::generate_test_tenant_code("ores.compute.usage");
    const auto third =
        tenant_id::from_string(test_database_manager::provision_test_tenant(
                                   provision_ctx, third_code, "compute usage third tenant"))
            .value();

    const auto own_party = random_uuid();
    const auto third_party = random_uuid();
    ores::compute::repository::batch_repository repo;
    sample_cleanup cleanup(h);
    const auto mine = usage_batch(gen, own, own_party);
    const auto theirs = usage_batch(gen, third, third_party);
    cleanup.batch(own, mine.id);
    cleanup.batch(third, theirs.id);
    repo.write(h.context(), mine);
    repo.write(h.context().with_tenant(third, h.db_user()), theirs);

    compute_telemetry_repository telemetry;

    // A tenant reads its own work and no other tenant's.
    const auto as_tenant = telemetry.grid_usage_by_scope(h.context());
    CHECK(as_tenant.group_kind == "party");
    CHECK(usage_index_of(as_tenant, boost::uuids::to_string(own_party)) >= 0);
    CHECK(usage_index_of(as_tenant, boost::uuids::to_string(third_party)) == -1);

    // The system tenant is a tenant like the others for this read: it reads the
    // system tenant's work and neither of the two tenants above. An operator who
    // needs another tenant's work enters that tenant.
    const auto as_system = telemetry.grid_usage_by_scope(h.context().with_tenant(system, h.db_user()));
    CHECK(as_system.group_kind == "party");
    CHECK(usage_index_of(as_system, boost::uuids::to_string(own_party)) == -1);
    CHECK(usage_index_of(as_system, boost::uuids::to_string(third_party)) == -1);
}

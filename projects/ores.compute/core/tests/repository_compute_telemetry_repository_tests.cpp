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
#include "ores.compute.api/generators/grid_sample_generator.hpp"
#include "ores.compute.api/generators/node_sample_generator.hpp"
#include "ores.compute.core/repository/compute_telemetry_repository.hpp"
#include "ores.compute.core/repository/grid_sample_repository.hpp"
#include "ores.compute.core/repository/node_sample_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
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

    auto own_row = generate_synthetic_node_sample(gen);
    own_row.tenant_id = own;
    own_row.host_id = random_uuid();
    own_row.sampled_at = std::chrono::system_clock::now();

    auto system_row = generate_synthetic_node_sample(gen);
    system_row.tenant_id = system;
    system_row.host_id = random_uuid();
    system_row.sampled_at = std::chrono::system_clock::now();

    auto third_row = generate_synthetic_node_sample(gen);
    third_row.tenant_id = third;
    third_row.host_id = random_uuid();
    third_row.sampled_at = std::chrono::system_clock::now();

    node_sample_repository repo;
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

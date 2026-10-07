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
    BOOST_LOG_SEV(lg, debug) << "Read host count: " << hosts.size();

    CHECK(std::ranges::find(hosts, boost::uuids::to_string(own_row.host_id)) != hosts.end());
    CHECK(std::ranges::find(hosts, boost::uuids::to_string(system_row.host_id)) != hosts.end());
    CHECK(std::ranges::find(hosts, boost::uuids::to_string(third_row.host_id)) == hosts.end());
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
    BOOST_LOG_SEV(lg, debug) << "Newest id: " << boost::uuids::to_string(newest->id);

    CHECK(newest->id != own_row.id);
    CHECK(newest->id != third_row.id);
    CHECK(newest->tenant_id == system);
}

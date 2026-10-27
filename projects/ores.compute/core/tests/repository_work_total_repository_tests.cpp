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
#include "ores.compute.core/repository/work_total_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using ores::compute::domain::grid_sample;
using ores::compute::repository::work_total_repository;
using ores::compute::repository::work_totals;
using ores::testing::database_helper;
using ores::utility::uuid::tenant_id;

/// A time far in the future, so a test row is the newest whatever the poller wrote.
std::chrono::system_clock::time_point far_future() {
    return std::chrono::system_clock::time_point{std::chrono::microseconds{4102444800000000LL}};
}

/// Removes the rows a test wrote, so the development database does not keep them.
struct written_totals {
    database_helper& h;

    ores::database::context system() const {
        return h.context().with_tenant(tenant_id::system(), h.db_user());
    }

    ~written_totals() {
        try {
            static auto lg = ores::logging::make_logger("ores.compute.tests.work_total");
            ores::database::repository::execute_parameterized_command(
                system(),
                "DELETE FROM ores_compute_work_totals_tbl WHERE sampled_at = "
                "to_timestamp(4102444800)",
                {},
                lg,
                "deleting the test totals");
        } catch (const std::exception&) {
            // A failed delete leaves a row behind and must not fail the test.
        }
    }
};

}

TEST_CASE("add_sums_the_job_counters_of_the_rows_it_is_given", "[repository][work_total]") {
    grid_sample a;
    a.results_unsent = 2;
    a.results_in_progress = 1;
    a.total_workunits = 5;
    grid_sample b;
    b.results_unsent = 3;
    b.results_done = 4;
    b.total_workunits = 6;
    b.outcomes_success = 4;

    work_totals totals;
    totals.add(a);
    totals.add(b);

    CHECK(totals.results_unsent == 5);
    CHECK(totals.results_in_progress == 1);
    CHECK(totals.results_done == 4);
    CHECK(totals.total_workunits == 11);
    CHECK(totals.outcomes_success == 4);
}

TEST_CASE("read_newest_returns_the_totals_the_system_tenant_wrote", "[repository][work_total]") {
    database_helper h;
    written_totals written{h};
    work_total_repository repo;

    work_totals totals;
    totals.sampled_at = far_future();
    totals.tenants_counted = 3;
    totals.results_in_progress = 7;
    totals.total_workunits = 41;
    repo.write(written.system(), totals);

    const auto newest = repo.read_newest(written.system());
    REQUIRE(newest.has_value());
    CHECK(newest->tenants_counted == 3);
    CHECK(newest->results_in_progress == 7);
    CHECK(newest->total_workunits == 41);
}

TEST_CASE("a_tenants_session_reads_none_of_the_totals", "[repository][work_total]") {
    database_helper h;
    written_totals written{h};
    work_total_repository repo;

    work_totals totals;
    totals.sampled_at = far_future();
    totals.total_workunits = 41;
    repo.write(written.system(), totals);

    // The totals sum every tenant's work, so no tenant owns them and none reads them.
    CHECK_FALSE(repo.read_newest(h.context()).has_value());
}

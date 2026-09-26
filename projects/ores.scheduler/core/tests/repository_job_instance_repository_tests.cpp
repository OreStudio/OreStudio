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
#include "ores.scheduler.api/domain/job_instance.hpp"
#include "ores.scheduler.core/repository/job_instance_repository.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string_view>

// The job-instance repository is the component's one hand-written repository:
// its reads are raw multi-column queries and its insert uses RETURNING id to
// get the bigserial the table assigns. The generated eventing test exercises
// the job-definition stack end to end, so this file covers what that test does
// not touch.

namespace {

const std::string_view test_suite("scheduler.tests");
const std::string tags("[repository][job_instance]");

ores::scheduler::domain::job_instance make_instance(const boost::uuids::uuid& job_definition_id,
                                                    std::chrono::system_clock::time_point at) {
    ores::scheduler::domain::job_instance inst;
    // A system job instance: the table's read policy shows a NULL tenant to
    // every caller, and the scheduler loop writes system jobs this way.
    inst.tenant_id = std::nullopt;
    inst.party_id = std::nullopt;
    inst.job_definition_id = job_definition_id;
    inst.action_type = "execute_sql";
    inst.status = ores::scheduler::domain::job_status::starting;
    inst.triggered_at = at;
    inst.started_at = at;
    return inst;
}

}

using namespace ores::logging;
using ores::scheduler::domain::job_instance;
using ores::scheduler::domain::job_status;
using ores::scheduler::repository::job_instance_repository;
using ores::testing::scoped_database_helper;

TEST_CASE("job_instance_repository records a run from start to finish", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = h.context();

    const auto job_definition_id = boost::uuids::random_generator{}();
    const auto started_at = std::chrono::system_clock::now();

    job_instance_repository repo;
    const auto id = repo.write_started(ctx, make_instance(job_definition_id, started_at));
    CHECK(id > 0);

    // The insert returns the id the table assigned; the read must show the same
    // row back with the fields the writer set.
    const auto started = repo.read_latest(ctx, job_definition_id);
    REQUIRE(started.size() == 1);
    CHECK(started.front().id == id);
    CHECK(started.front().status == job_status::starting);
    CHECK(started.front().action_type == "execute_sql");
    CHECK(started.front().job_definition_id == job_definition_id);
    CHECK_FALSE(started.front().duration_ms.has_value());

    const auto last_run = repo.last_run_at(ctx, job_definition_id);
    REQUIRE(last_run.has_value());

    repo.write_completed(ctx, id, started_at, job_status::succeeded);

    const auto finished = repo.read_latest(ctx, job_definition_id);
    REQUIRE(finished.size() == 1);
    CHECK(finished.front().status == job_status::succeeded);
    CHECK(finished.front().id == id);
    // A completed row carries a duration; a starting one does not. The
    // repository derives it, so asserting presence here is the difference
    // between the two writes.
    REQUIRE(finished.front().duration_ms.has_value());
}

TEST_CASE("job_instance_repository reports the newest run of one definition", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = h.context();

    const auto job_definition_id = boost::uuids::random_generator{}();
    const auto other_definition_id = boost::uuids::random_generator{}();
    const auto first = std::chrono::system_clock::now();
    const auto second = first + std::chrono::seconds(30);

    job_instance_repository repo;
    const auto first_id = repo.write_started(ctx, make_instance(job_definition_id, first));
    const auto second_id = repo.write_started(ctx, make_instance(job_definition_id, second));
    repo.write_started(ctx, make_instance(other_definition_id, second));

    // read_latest is scoped to the definition it is asked about, and answers
    // newest first: the other definition's row must not appear, and the newer
    // of this definition's two rows must come first. A read that ignored the
    // definition would return three, and one that ignored the order would
    // return the first.
    const auto rows = repo.read_latest(ctx, job_definition_id);
    REQUIRE(rows.size() == 2);
    CHECK(rows.front().id == second_id);
    CHECK(rows.back().id == first_id);

    const auto last_run = repo.last_run_at(ctx, job_definition_id);
    REQUIRE(last_run.has_value());
    CHECK(*last_run >= second - std::chrono::seconds(1));
}

TEST_CASE("job_instance_repository lists the newest runs across definitions", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = h.context();

    const auto job_definition_id = boost::uuids::random_generator{}();
    const auto at = std::chrono::system_clock::now();

    job_instance_repository repo;
    const auto id = repo.write_started(ctx, make_instance(job_definition_id, at));

    // The cross-definition read is what the job-instance view serves, and it
    // must include the row just written -- the empty vector it would return if
    // the query were wrong is the failure this pins.
    const auto rows = repo.read_all_latest(ctx, 100);
    REQUIRE_FALSE(rows.empty());

    const auto found =
        std::find_if(rows.begin(), rows.end(), [id](const auto& r) { return r.id == id; });
    REQUIRE(found != rows.end());
    CHECK(found->job_definition_id == job_definition_id);
}

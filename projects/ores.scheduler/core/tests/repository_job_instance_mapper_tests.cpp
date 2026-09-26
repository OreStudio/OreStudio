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
#include "ores.logging/make_logger.hpp"
#include "ores.scheduler.api/domain/job_status.hpp"
#include "ores.scheduler.core/repository/job_instance_mapper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

// The hand-written mapper turns a row of strings into the domain type. It is
// pure, so it is tested without a database, which is the point: the repository
// test above needs a live PostgreSQL to say anything at all.

namespace {

const std::string_view test_suite("scheduler.tests");
const std::string tags("[repository][mapper][job_instance]");

}

using namespace ores::logging;
using ores::scheduler::domain::job_status;
using ores::scheduler::repository::job_instance_entity;
using ores::scheduler::repository::job_instance_mapper;

TEST_CASE("job_instance_mapper reads every column of a full row", tags) {
    auto lg(make_logger(test_suite));

    job_instance_entity e;
    e.id = "17";
    e.tenant_id = "11111111-1111-1111-1111-111111111111";
    e.party_id = "22222222-2222-2222-2222-222222222222";
    e.job_definition_id = "33333333-3333-3333-3333-333333333333";
    e.action_type = "nats_publish";
    e.status = "succeeded";
    e.triggered_at = "2026-01-15 13:45:00Z";
    e.started_at = "2026-01-15 13:45:01Z";
    e.completed_at = "2026-01-15 13:45:02Z";
    e.duration_ms = "1500";
    e.error_message = "";

    const auto inst = job_instance_mapper::map(e);

    CHECK(inst.id == 17);
    CHECK(boost::uuids::to_string(*inst.tenant_id) == "11111111-1111-1111-1111-111111111111");
    CHECK(boost::uuids::to_string(*inst.party_id) == "22222222-2222-2222-2222-222222222222");
    CHECK(boost::uuids::to_string(inst.job_definition_id) ==
          "33333333-3333-3333-3333-333333333333");
    CHECK(inst.action_type == "nats_publish");
    CHECK(inst.status == job_status::succeeded);
    REQUIRE(inst.completed_at.has_value());
    REQUIRE(inst.duration_ms.has_value());
    CHECK(*inst.duration_ms == 1500);
    CHECK(inst.error_message.empty());
    // The triggered and completed instants are ordered as the row states them.
    CHECK(*inst.completed_at > inst.triggered_at);
}

TEST_CASE("job_instance_mapper leaves an absent column absent", tags) {
    auto lg(make_logger(test_suite));

    job_instance_entity e;
    e.job_definition_id = "33333333-3333-3333-3333-333333333333";
    e.status = "starting";
    e.triggered_at = "2026-01-15 13:45:00Z";
    e.started_at = "2026-01-15 13:45:00Z";

    const auto inst = job_instance_mapper::map(e);

    // A system job instance has no tenant and no party, and a running one has
    // no completion. Each must stay empty rather than acquire a zero value.
    CHECK_FALSE(inst.tenant_id.has_value());
    CHECK_FALSE(inst.party_id.has_value());
    CHECK_FALSE(inst.completed_at.has_value());
    CHECK_FALSE(inst.duration_ms.has_value());
    CHECK(inst.status == job_status::starting);
}

TEST_CASE("job_instance_mapper maps the status strings the table allows", tags) {
    auto lg(make_logger(test_suite));

    const auto status_of = [](const std::string& s) {
        job_instance_entity e;
        e.status = s;
        return job_instance_mapper::map(e).status;
    };

    CHECK(status_of("starting") == job_status::starting);
    CHECK(status_of("succeeded") == job_status::succeeded);
    CHECK(status_of("failed") == job_status::failed);
    // The table's check constraint allows those three and nothing else, so an
    // unrecognised value can only arrive from a hand-edited row. The mapper
    // reads it as starting, which is what the record should say rather than
    // silently claiming success.
    CHECK(status_of("not a status") == job_status::starting);
}

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
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.scheduler/builder/job_definition_builder.hpp"

using namespace ores::scheduler::builder;
using namespace ores::scheduler::domain;

namespace {

const auto test_tenant = ores::utility::uuid::tenant_id::system();
const auto test_party  = boost::uuids::random_generator{}();

} // anonymous namespace

TEST_CASE("job_definition_builder produces a valid definition from all fields",
          "[builder][job_definition_builder]") {
    auto result = job_definition_builder{}
        .with_name("test_job")
        .with_description("A test job")
        .with_command("SELECT 1")
        .with_cron_schedule("0 0 * * *")
        .with_database("ores_test")
        .with_tenant(test_tenant)
        .with_party(test_party)
        .with_modified_by("test_user")
        .build();

    REQUIRE(result.has_value());

    const auto& def = *result;
    CHECK(def.job_name == "test_job");
    CHECK(def.description == "A test job");
    CHECK(def.command == "SELECT 1");
    CHECK(def.schedule_expression.to_string() == "0 0 * * *");
    CHECK(def.database_name == "ores_test");
    CHECK(def.modified_by == "test_user");
    CHECK(def.is_active == true);
    CHECK(def.version == 0);
    CHECK(!def.cron_job_id.has_value());
    // UUID should be non-nil
    CHECK(def.id != boost::uuids::uuid{});
}

TEST_CASE("job_definition_builder fails when name is missing",
          "[builder][job_definition_builder]") {
    auto result = job_definition_builder{}
        .with_command("SELECT 1")
        .with_cron_schedule("0 0 * * *")
        .with_database("ores_test")
        .with_tenant(test_tenant)
        .with_party(test_party)
        .with_modified_by("test_user")
        .build();

    REQUIRE_FALSE(result.has_value());
    CHECK(!result.error().empty());
}

TEST_CASE("job_definition_builder fails when command is missing",
          "[builder][job_definition_builder]") {
    auto result = job_definition_builder{}
        .with_name("test_job")
        .with_cron_schedule("0 0 * * *")
        .with_database("ores_test")
        .with_tenant(test_tenant)
        .with_party(test_party)
        .with_modified_by("test_user")
        .build();

    REQUIRE_FALSE(result.has_value());
}

TEST_CASE("job_definition_builder fails for invalid cron expression",
          "[builder][job_definition_builder]") {
    auto result = job_definition_builder{}
        .with_name("test_job")
        .with_command("SELECT 1")
        .with_cron_schedule("not a cron")
        .with_database("ores_test")
        .with_tenant(test_tenant)
        .with_party(test_party)
        .with_modified_by("test_user")
        .build();

    REQUIRE_FALSE(result.has_value());
    CHECK(!result.error().empty());
}

TEST_CASE("job_definition_builder accepts various valid cron expressions",
          "[builder][job_definition_builder]") {
    const std::vector<std::string> schedules = {
        "* * * * *",
        "0 12 * * 1-5",
        "*/15 * * * *",
    };

    for (const auto& sched : schedules) {
        auto result = job_definition_builder{}
            .with_name("job_for_" + sched)
            .with_command("SELECT 1")
            .with_cron_schedule(sched)
            .with_database("ores_test")
            .with_tenant(test_tenant)
            .with_party(test_party)
            .with_modified_by("test_user")
            .build();

        INFO("Testing schedule: " << sched);
        REQUIRE(result.has_value());
        CHECK(result->schedule_expression.to_string() == sched);
    }
}

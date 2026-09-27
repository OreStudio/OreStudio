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
#include "ores.reporting.core/service/scheduling_plan.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>

// The decision these pin is the one that made reconcile fail on every service
// start: it generated a fresh job id for a definition whose job already existed,
// and the scheduler refused the insert on the job-name index. No clock, no
// database and no bus are involved in asking which id to use, which is why the
// decision is a function.

using namespace ores::reporting::service;

namespace {

const std::string tags("[service][scheduling]");

boost::uuids::uuid uuid_of(const std::string& text) {
    return boost::uuids::string_generator()(text);
}

}

TEST_CASE("scheduler_job_name is the definition prefixed", tags) {
    const auto id = uuid_of("11111111-2222-3333-4444-555555555555");
    CHECK(scheduler_job_name(id) == "report_definition.11111111-2222-3333-4444-555555555555");
}

TEST_CASE("a definition with no job anywhere has no id to adopt", tags) {
    const std::map<std::string, boost::uuids::uuid> jobs;
    CHECK_FALSE(
        existing_job_for(jobs, uuid_of("11111111-2222-3333-4444-555555555555")).has_value());
}

TEST_CASE("a job under the definition's name is adopted", tags) {
    const auto definition = uuid_of("11111111-2222-3333-4444-555555555555");
    const auto job = uuid_of("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee");
    const std::map<std::string, boost::uuids::uuid> jobs{{scheduler_job_name(definition), job}};

    const auto adopted = existing_job_for(jobs, definition);
    REQUIRE(adopted.has_value());
    CHECK(*adopted == job);
}

TEST_CASE("another definition's job is not adopted", tags) {
    const auto definition = uuid_of("11111111-2222-3333-4444-555555555555");
    const auto other = uuid_of("99999999-2222-3333-4444-555555555555");
    const std::map<std::string, boost::uuids::uuid> jobs{
        {scheduler_job_name(other), uuid_of("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")}};

    CHECK_FALSE(existing_job_for(jobs, definition).has_value());
}

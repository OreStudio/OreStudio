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
#include <set>
#include <string>
#include <vector>

// The decision these pin: which scheduler jobs reconciliation must remove.
// The scheduler holds every component's jobs in one table and refuses two
// current jobs with one name, so the decision reads names as well as ids. No
// clock, no database and no bus are involved in asking which jobs to remove,
// which is why the decision is a function.

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

TEST_CASE("only a report job name is a report job name", tags) {
    CHECK(is_report_definition_job_name("report_definition.11111111-2222-3333-4444-555555555555"));
    CHECK_FALSE(is_report_definition_job_name("some_other_job"));
}

TEST_CASE("a report job a definition accounts for is kept", tags) {
    const auto definition = uuid_of("11111111-2222-3333-4444-555555555555");
    const auto job = uuid_of("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee");
    const std::map<std::string, boost::uuids::uuid> jobs{{scheduler_job_name(definition), job}};
    const std::set<boost::uuids::uuid> accounted{job};

    CHECK(unaccounted_report_jobs(jobs, accounted).empty());
}

TEST_CASE("a report job no definition accounts for is removed", tags) {
    const auto definition = uuid_of("11111111-2222-3333-4444-555555555555");
    const auto job = uuid_of("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee");
    const std::map<std::string, boost::uuids::uuid> jobs{{scheduler_job_name(definition), job}};

    CHECK(unaccounted_report_jobs(jobs, {}).size() == 1);
    CHECK(unaccounted_report_jobs(jobs, {}) == std::vector<boost::uuids::uuid>{job});
}

TEST_CASE("another component's job is never a report orphan", tags) {
    const std::map<std::string, boost::uuids::uuid> jobs{
        {"marketdata.curve.refresh", uuid_of("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")}};

    CHECK(unaccounted_report_jobs(jobs, {}).empty());
}

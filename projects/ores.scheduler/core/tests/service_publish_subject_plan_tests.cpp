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
#include "ores.scheduler.core/service/publish_subject_plan.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

// The subject is the input and the function name is the output, so the test
// asserts the literal name each subject must produce. An empty result is the
// refusal signal: the caller must not call a function with no name.

using ores::scheduler::service::publish_from_dq_function;
using ores::scheduler::service::publish_from_dq_subjects;

TEST_CASE("every subject the scheduler subscribes names its expansion function",
          "[service][publish]") {
    for (const auto subject : publish_from_dq_subjects)
        CHECK_FALSE(publish_from_dq_function(subject).empty());

    CHECK(publish_from_dq_function("scheduler.v1.ops.publish_job_definitions_from_dq") ==
          "ores_scheduler_publish_job_definitions_from_dq_fn");
    CHECK(publish_from_dq_function("scheduler.v1.ops.publish_retention_policies_from_dq") ==
          "ores_scheduler_publish_retention_policies_from_dq_fn");
}

TEST_CASE("a subject without a version has no function", "[service][publish]") {
    CHECK(publish_from_dq_function("scheduler.job-definitions.publish-from-dq").empty());
}

TEST_CASE("a subject outside the publish pattern has no function", "[service][publish]") {
    CHECK(publish_from_dq_function("scheduler.v1.job-definitions.list").empty());
}

TEST_CASE("a subject whose operation is not a plain publish name has no function",
          "[service][publish]") {
    CHECK(publish_from_dq_function("scheduler.v1.ops.x; drop table y").empty());
    CHECK(publish_from_dq_function("scheduler.v1.ops.publish_a_from_dq; select 1").empty());
    CHECK(publish_from_dq_function("scheduler.v1.ops.Publish_A_from_dq").empty());
}

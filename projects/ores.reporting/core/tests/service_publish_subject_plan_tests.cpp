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
#include "ores.reporting.core/service/publish_subject_plan.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

// The subject is the input and the function name is the output, so the test
// asserts the literal name each subject must produce. An empty result is the
// refusal signal: the caller must not call a function with no name.

using ores::reporting::service::publish_from_dq_function;

namespace {
const std::string tags("[service][publish]");
}

TEST_CASE("a report definitions subject names its expansion function", tags) {
    CHECK(publish_from_dq_function("reporting.v1.report-definitions.publish-from-dq") ==
          "ores_reporting_publish_report_definitions_from_dq_fn");
}

TEST_CASE("a hyphen becomes an underscore and an underscore stays", tags) {
    CHECK(publish_from_dq_function("reporting.v1.report_operations.publish-from-dq") ==
          "ores_reporting_publish_report_operations_from_dq_fn");
    CHECK(publish_from_dq_function("reporting.v1.a-b_c.publish-from-dq") ==
          "ores_reporting_publish_a_b_c_from_dq_fn");
}

TEST_CASE("a subject without a version has no function", tags) {
    CHECK(publish_from_dq_function("reporting.report-definitions.publish-from-dq").empty());
}

TEST_CASE("a subject outside the publish pattern has no function", tags) {
    CHECK(publish_from_dq_function("reporting.v1.report-definitions.list").empty());
}

TEST_CASE("a subject with nothing between the version and the suffix has no function", tags) {
    CHECK(publish_from_dq_function("reporting.v1..publish-from-dq").empty());
}

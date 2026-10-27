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

#include "ores.refdata.api/workflow/counterparty_apply_workflow.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::workflow::service::failure_policy;
using ores::workflow::service::workflow_registry;

const ores::workflow::service::workflow_definition*
workflow_counterparty_apply_workflow_registered(workflow_registry& registry) {
    ores::refdata::workflow::register_counterparty_apply_workflow(registry);
    return registry.find(std::string(ores::refdata::workflow::counterparty_apply_workflow_type));
}

}

TEST_CASE("the_counterparty_apply_workflow_stops_on_a_failure_and_keeps_its_completed_steps",
          "[counterparty_apply][workflow]") {
    workflow_registry registry;
    const auto* def = workflow_counterparty_apply_workflow_registered(registry);
    REQUIRE(def != nullptr);
    CHECK(def->on_failure == failure_policy::stop);
    CHECK_FALSE(def->steps_depend_on_request);
}

TEST_CASE("the_counterparty_apply_workflow_rechecks_then_applies_then_tells",
          "[counterparty_apply][workflow]") {
    workflow_registry registry;
    const auto* def = workflow_counterparty_apply_workflow_registered(registry);
    REQUIRE(def != nullptr);

    const auto steps = def->build_steps(R"({"request_id":"abc"})", "tenant", "corr");

    REQUIRE(steps.size() == 3);
    CHECK(steps[0].name == "recheck");
    CHECK(steps[1].name == "apply");
    CHECK(steps[2].name == "tell");
    CHECK(steps[0].consumes.empty());
    CHECK(steps[1].consumes == std::vector<std::string>{"recheck"});
    CHECK(steps[2].consumes == std::vector<std::string>{"apply"});
    CHECK(steps[0].command_subject == "refdata.v1.steps.recheck_counterparty_changes");
    CHECK(steps[1].command_subject == "refdata.v1.steps.apply_counterparty_changes");
    CHECK(steps[2].command_subject == "inbox.v1.steps.tell_apply_result");
}

TEST_CASE("every_step_of_the_counterparty_apply_workflow_states_a_deadline_and_no_compensation",
          "[counterparty_apply][workflow]") {
    workflow_registry registry;
    const auto* def = workflow_counterparty_apply_workflow_registered(registry);
    REQUIRE(def != nullptr);

    const auto steps = def->build_steps(R"({"request_id":"abc"})", "tenant", "corr");

    for (const auto& s : steps) {
        CHECK(s.timeout.count() > 0);
        CHECK(s.compensation_subject.empty());
        CHECK_FALSE(s.label.empty());
    }
}

TEST_CASE("every_step_is_told_the_request_it_acts_on", "[counterparty_apply][workflow]") {
    workflow_registry registry;
    const auto* def = workflow_counterparty_apply_workflow_registered(registry);
    REQUIRE(def != nullptr);

    const auto steps = def->build_steps(R"({"request_id":"abc-123"})", "tenant", "corr");

    for (const auto& s : steps) {
        const auto command = s.build_command("", {});
        CHECK(command.find("abc-123") != std::string::npos);
        CHECK(command.find("request_id") != std::string::npos);
    }
}

TEST_CASE("a_start_that_names_no_request_builds_no_steps", "[counterparty_apply][workflow]") {
    workflow_registry registry;
    const auto* def = workflow_counterparty_apply_workflow_registered(registry);
    REQUIRE(def != nullptr);

    CHECK(def->build_steps(R"({"request_id":""})", "tenant", "corr").empty());
    CHECK(def->build_steps("not json", "tenant", "corr").empty());
}

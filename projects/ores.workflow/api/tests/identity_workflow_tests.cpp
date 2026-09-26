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
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include "ores.workflow.api/workflow/identity_workflow.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[identity]");

namespace wf = ores::workflow::workflow;

/**
 * @brief Builds the fixture's steps from a request, the way the engine does.
 *
 * The registry is local because a definition's build_steps runs against a
 * registered definition, never against the built-in name.
 */
std::vector<ores::workflow::service::workflow_step_def>
identity_steps(const std::string& request_json) {
    ores::workflow::service::workflow_registry registry;
    wf::register_identity_workflow(registry);
    const auto* def = registry.find("identity_workflow");
    REQUIRE(def != nullptr);
    return def->build_steps(request_json, "tenant", "corr");
}

/**
 * @brief Reads back a command payload as the handler reads it.
 *
 * The handler decodes the published body straight into identity_step_request,
 * so a build_command result that does not read back as one is a payload the
 * fixture could never execute.
 */
wf::identity_step_request command_of(const ores::workflow::service::workflow_step_def& step) {
    const auto parsed = rfl::json::read<wf::identity_step_request>(step.build_command("{}", {}));
    REQUIRE(parsed.has_value());
    return *parsed;
}

}

TEST_CASE("identity_workflow_fills_in_every_omitted_step_field", tags) {
    // Only the name is given. Every other member is optional on the wire, so the
    // step still arrives with a behaviour, no compensation and no delay.
    const auto steps = identity_steps(R"({"steps":[{"name":"one"}]})");

    REQUIRE(steps.size() == 1);
    CHECK(steps[0].name == "one");
    CHECK(steps[0].command_subject == wf::identity_step_command_subject);
    CHECK(steps[0].compensation_subject.empty());

    const auto command = command_of(steps[0]);
    CHECK(command.name == "one");
    CHECK(command.outcome() == "complete");
    CHECK(command.compensates() == false);
    CHECK(command.delay() == 0);
}

TEST_CASE("identity_workflow_carries_the_behaviour_its_request_declares", tags) {
    const auto steps = identity_steps(
        R"({"steps":[{"name":"flaky","behaviour":"fail","compensate":true,"delay_seconds":45}]})");

    REQUIRE(steps.size() == 1);
    CHECK(steps[0].compensation_subject == wf::identity_compensation_command_subject);

    const auto command = command_of(steps[0]);
    CHECK(command.name == "flaky");
    CHECK(command.outcome() == "fail");
    CHECK(command.delay() == 45);
}

TEST_CASE("identity_workflow_builds_one_step_per_declared_entry", tags) {
    const auto steps = identity_steps(
        R"({"steps":[{"name":"one","behaviour":"warn"},{"name":"two"},{"name":"three"}]})");

    REQUIRE(steps.size() == 3);
    CHECK(steps[0].name == "one");
    CHECK(command_of(steps[0]).outcome() == "warn");
    CHECK(steps[1].name == "two");
    CHECK(command_of(steps[1]).outcome() == "complete");
    CHECK(steps[2].name == "three");
}

TEST_CASE("identity_workflow_compensates_by_completing", tags) {
    const auto steps = identity_steps(R"({"steps":[{"name":"one","compensate":true}]})");

    REQUIRE(steps.size() == 1);
    const auto compensation = rfl::json::read<wf::identity_step_request>(
        steps[0].build_compensation(steps[0].build_command("{}", {}), "{}"));
    REQUIRE(compensation.has_value());
    CHECK(compensation->name == "one");
    CHECK(compensation->outcome() == "complete");
}

TEST_CASE("identity_workflow_builds_nothing_from_a_request_it_cannot_read", tags) {
    // The engine reports an empty step list as "no steps", so a request that is
    // not a step list must not be mistaken for an empty workflow.
    CHECK(identity_steps("not json").empty());
    CHECK(identity_steps("{}").empty());
    CHECK(identity_steps(R"({"steps":[]})").empty());
}

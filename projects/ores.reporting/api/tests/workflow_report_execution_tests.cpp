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
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.api/workflow/report_execution_workflow.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

// The chain is the observable result: the engine rebuilds it from the start
// message on every dispatch, so these assert the step names a run configuration
// produces, and the commands its consumers build from the results before them.

namespace {

const std::string tags("[workflow][reporting]");

namespace wf = ores::reporting::workflow;
namespace msg = ores::reporting::messaging;
namespace svc = ores::workflow::service;

/**
 * @brief The run's start payload, as the trigger handler writes it.
 */
std::string run_request(std::string pre_processing,
                        std::string prepared_input_key,
                        std::string post_processing) {
    const msg::report_execution_request req{.report_instance_id = "instance",
                                            .definition_id = "definition",
                                            .tenant_id = "tenant",
                                            .correlation_id = "corr",
                                            .pre_processing = std::move(pre_processing),
                                            .prepared_input_key = std::move(prepared_input_key),
                                            .post_processing = std::move(post_processing)};
    return rfl::json::write(req);
}

std::vector<svc::workflow_step_def> steps_for(const std::string& request_json) {
    svc::workflow_registry registry;
    wf::register_report_execution_workflow(registry);
    const auto* def = registry.find("report_execution_workflow");
    REQUIRE(def != nullptr);
    return def->build_steps(request_json, "tenant", "corr");
}

std::vector<std::string> names_of(const std::vector<svc::workflow_step_def>& steps) {
    std::vector<std::string> names;
    names.reserve(steps.size());
    for (const auto& s : steps)
        names.push_back(s.name);
    return names;
}

std::string packaging_result(const std::string& uri) {
    return rfl::json::write(msg::prepare_ore_package_result{
        .success = true, .message = "input ready", .tarball_uris = {uri}});
}

}

TEST_CASE("a run that executes every phase builds the chain it always built", tags) {
    const auto steps = steps_for(run_request("execute", "", "execute"));

    CHECK(names_of(steps) == std::vector<std::string>{"gather_trades",
                                                      "gather_market_data",
                                                      "assemble_bundle",
                                                      "prepare_ore_package",
                                                      "submit_compute",
                                                      "collect_compute_results",
                                                      "finalise"});
}

TEST_CASE("a run that replaces both phases builds the substituted chain", tags) {
    const auto steps = steps_for(run_request("substitute", "prepared/input.tar.gz", "ignore"));

    CHECK(names_of(steps) ==
          std::vector<std::string>{
              "resolve_prepared_input", "submit_compute", "ignore_compute_results", "finalise"});
}

TEST_CASE("a run substitutes one phase at a time", tags) {
    CHECK(names_of(steps_for(run_request("substitute", "prepared/input.tar.gz", "execute"))) ==
          std::vector<std::string>{
              "resolve_prepared_input", "submit_compute", "collect_compute_results", "finalise"});

    CHECK(names_of(steps_for(run_request("execute", "", "ignore"))) ==
          std::vector<std::string>{"gather_trades",
                                   "gather_market_data",
                                   "assemble_bundle",
                                   "prepare_ore_package",
                                   "submit_compute",
                                   "ignore_compute_results",
                                   "finalise"});
}

TEST_CASE("submission reads whichever step filled the input role", tags) {
    const auto json = run_request("substitute", "prepared/input.tar.gz", "execute");
    const auto steps = steps_for(json);
    REQUIRE(steps[1].name == "submit_compute");

    const std::vector<svc::workflow_step_result> prev{
        {.name = "resolve_prepared_input",
         .response_json = packaging_result("prepared/input.tar.gz")}};
    const auto cmd =
        rfl::json::read<msg::submit_compute_request>(steps[1].build_command(json, prev));

    REQUIRE(cmd.has_value());
    CHECK(cmd->tarball_uris == std::vector<std::string>{"prepared/input.tar.gz"});
}

TEST_CASE("the substitute is indistinguishable to the step that follows it", tags) {
    const auto json = run_request("execute", "", "execute");
    const auto executed = steps_for(json);
    const auto substituted =
        steps_for(run_request("substitute", "prepared/input.tar.gz", "execute"));

    // The same answer, produced by the phase and by its replacement, must yield
    // the same command; anything else would make the substitution observable.
    const std::vector<svc::workflow_step_result> from_phase{
        {.name = "prepare_ore_package", .response_json = packaging_result("a.tar.gz")}};
    const std::vector<svc::workflow_step_result> from_substitute{
        {.name = "resolve_prepared_input", .response_json = packaging_result("a.tar.gz")}};

    CHECK(executed[4].name == "submit_compute");
    CHECK(substituted[1].name == "submit_compute");
    CHECK(executed[4].build_command(json, from_phase) ==
          substituted[1].build_command(json, from_substitute));
}

TEST_CASE("a consumer whose result is absent fails rather than guessing", tags) {
    const auto json = run_request("execute", "", "execute");
    const auto steps = steps_for(json);

    // gather_trades answered nothing, so bundle assembly has no input.
    CHECK_THROWS_AS(steps[2].build_command(json, {}), std::runtime_error);
}

TEST_CASE("a consumer whose result cannot be read fails rather than guessing", tags) {
    const auto json = run_request("execute", "", "execute");
    const auto steps = steps_for(json);

    const std::vector<svc::workflow_step_result> prev{
        {.name = "gather_trades", .response_json = R"({"not":"a trades result"})"}};
    CHECK_THROWS_AS(steps[2].build_command(json, prev), std::runtime_error);
}

TEST_CASE("finalisation needs no result from the phase before it", tags) {
    const auto json = run_request("execute", "", "ignore");
    const auto steps = steps_for(json);
    REQUIRE(steps.back().name == "finalise");

    const auto cmd =
        rfl::json::read<msg::finalise_report_request>(steps.back().build_command(json, {}));
    REQUIRE(cmd.has_value());
    CHECK(cmd->report_instance_id == "instance");
}

TEST_CASE("packaging names the report definition the run belongs to", tags) {
    const auto json = run_request("execute", "", "execute");
    const auto steps = steps_for(json);
    REQUIRE(steps[3].name == "prepare_ore_package");

    msg::assemble_bundle_result bundle;
    bundle.bundle_id = "bundle";
    msg::gather_trades_result trades;
    trades.storage_key = "trades";
    msg::gather_market_data_result market_data;
    market_data.storage_key = "market";
    market_data.fixings_storage_key = "fixings";
    const std::vector<svc::workflow_step_result> prev{
        {.name = "gather_trades", .response_json = rfl::json::write(trades)},
        {.name = "gather_market_data", .response_json = rfl::json::write(market_data)},
        {.name = "assemble_bundle", .response_json = rfl::json::write(bundle)}};

    const auto cmd =
        rfl::json::read<msg::prepare_ore_package_request>(steps[3].build_command(json, prev));
    REQUIRE(cmd.has_value());
    CHECK(cmd->definition_id == "definition");
    CHECK(cmd->bundle_id == "bundle");
    CHECK(cmd->fixings_storage_key == "fixings");
}

TEST_CASE("a configuration nobody implements builds no chain", tags) {
    CHECK_THROWS_AS(steps_for(run_request("magic", "", "execute")), std::runtime_error);
    CHECK_THROWS_AS(steps_for(run_request("execute", "", "magic")), std::runtime_error);
}

TEST_CASE("substituting without naming an input fails at chain build", tags) {
    CHECK_THROWS_AS(steps_for(run_request("substitute", "", "execute")), std::runtime_error);
}

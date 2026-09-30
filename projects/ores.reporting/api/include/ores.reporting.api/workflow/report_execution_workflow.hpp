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
#ifndef ORES_REPORTING_API_WORKFLOW_REPORT_EXECUTION_WORKFLOW_HPP
#define ORES_REPORTING_API_WORKFLOW_REPORT_EXECUTION_WORKFLOW_HPP

#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::workflow {

// The budgets a step states belong to the engine's vocabulary, so a definition
// reads them from one place rather than inventing its own numbers.
using ores::workflow::service::data_step_timeout;
using ores::workflow::service::write_step_timeout;

/**
 * @brief The budget for a step that waits on the compute grid.
 *
 * A pricing run is the tenant's own work and takes as long as it takes: a
 * large portfolio on a small grid is hours, not minutes. The hour here is a
 * deadline for a grid that has gone, not a target for how fast a run should
 * be; a deployment whose runs are longer states its own.
 */
inline constexpr std::chrono::seconds compute_step_timeout{3600};

/**
 * @brief The run configuration codes a report definition states.
 *
 * Pre-processing either generates the engine's input or resolves a prepared
 * archive the definition names. Post-processing either ingests the engine's
 * results or records only that they were produced. The codes are stated here
 * because the trigger validates them and the chain builder reads them.
 */
constexpr std::string_view pre_processing_execute = "execute";
constexpr std::string_view pre_processing_substitute = "substitute";
constexpr std::string_view post_processing_execute = "execute";
constexpr std::string_view post_processing_ignore = "ignore";

/**
 * @brief Whether @p value names a pre-processing behaviour this component implements.
 */
[[nodiscard]] constexpr bool is_known_pre_processing(std::string_view value) {
    return value == pre_processing_execute || value == pre_processing_substitute;
}

/**
 * @brief Whether @p value names a post-processing behaviour this component implements.
 */
[[nodiscard]] constexpr bool is_known_post_processing(std::string_view value) {
    return value == post_processing_execute || value == post_processing_ignore;
}

namespace detail {

using ores::workflow::service::find_step_result;
using ores::workflow::service::workflow_step_def;
using ores::workflow::service::workflow_step_results;

/**
 * @brief The payload of the named step, read as @p Result.
 *
 * A consumer that cannot find what it consumes, or cannot read it, stops the
 * run. Falling back to an empty field would hand the next phase a command that
 * looks well formed and is not.
 */
template <typename Result>
[[nodiscard]] inline Result require_step_result(const workflow_step_results& results,
                                                std::string_view step_name,
                                                std::string_view consumer) {
    const auto* json = find_step_result(results, step_name);
    if (!json)
        throw std::runtime_error(std::string(consumer) + ": the step '" + std::string(step_name) +
                                 "' has produced no result");
    auto parsed = rfl::json::read<Result>(*json);
    if (!parsed)
        throw std::runtime_error(std::string(consumer) + ": cannot read the result of step '" +
                                 std::string(step_name) + "'");
    return *parsed;
}

/**
 * @brief The run's request, or a loud failure.
 */
[[nodiscard]] inline ores::reporting::messaging::report_execution_request
require_run_request(const std::string& request_json, std::string_view consumer) {
    auto req = rfl::json::read<ores::reporting::messaging::report_execution_request>(request_json);
    if (!req)
        throw std::runtime_error(std::string(consumer) +
                                 ": cannot read the run's report_execution_request");
    return *req;
}

/**
 * @brief The compensation every step of this workflow shares: fail the report.
 */
template <typename Command>
[[nodiscard]] inline std::string fail_report_from(const std::string& cmd_json,
                                                  std::string_view error_message) {
    using namespace ores::reporting::messaging;
    auto cmd = rfl::json::read<Command>(cmd_json);
    if (!cmd)
        return "{}";
    return rfl::json::write(fail_report_request{.report_instance_id = cmd->report_instance_id,
                                                .tenant_id = cmd->tenant_id,
                                                .correlation_id = cmd->correlation_id,
                                                .error_message = std::string(error_message)});
}

[[nodiscard]] inline workflow_step_def make_gather_trades() {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "gather_trades";
    s.description = "Fetch trades matching the report's book scope.";
    s.command_subject = std::string(gather_trades_request::nats_subject);
    s.timeout = data_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command = [](const std::string& request_json,
                         const workflow_step_results&) -> std::string {
        const auto req = require_run_request(request_json, "gather_trades");
        return rfl::json::write(gather_trades_request{.report_instance_id = req.report_instance_id,
                                                      .definition_id = req.definition_id,
                                                      .tenant_id = req.tenant_id,
                                                      .correlation_id = req.correlation_id});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<gather_trades_request>(
            cmd_json, "Report execution failed during trade gathering");
    };

    return s;
}

[[nodiscard]] inline workflow_step_def make_gather_market_data() {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "gather_market_data";
    s.description = "Fetch market data (price series) for the report period.";
    s.command_subject = std::string(gather_market_data_request::nats_subject);
    s.timeout = data_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command = [](const std::string& request_json,
                         const workflow_step_results&) -> std::string {
        const auto req = require_run_request(request_json, "gather_market_data");
        return rfl::json::write(
            gather_market_data_request{.report_instance_id = req.report_instance_id,
                                       .definition_id = req.definition_id,
                                       .tenant_id = req.tenant_id,
                                       .correlation_id = req.correlation_id});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<gather_market_data_request>(
            cmd_json, "Report execution failed during market data gathering");
    };

    return s;
}

[[nodiscard]] inline workflow_step_def make_assemble_bundle() {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "assemble_bundle";
    s.description = "Assemble aggregated input data bundle from trades and market data.";
    s.command_subject = std::string(assemble_bundle_request::nats_subject);
    s.timeout = data_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command = [](const std::string& request_json,
                         const workflow_step_results& prev) -> std::string {
        const auto req = require_run_request(request_json, "assemble_bundle");
        const auto trades =
            require_step_result<gather_trades_result>(prev, "gather_trades", "assemble_bundle");
        const auto market_data = require_step_result<gather_market_data_result>(
            prev, "gather_market_data", "assemble_bundle");

        return rfl::json::write(
            assemble_bundle_request{.report_instance_id = req.report_instance_id,
                                    .definition_id = req.definition_id,
                                    .tenant_id = req.tenant_id,
                                    .correlation_id = req.correlation_id,
                                    .trades_storage_key = trades.storage_key,
                                    .market_data_storage_key = market_data.storage_key,
                                    .trade_count = trades.trade_count,
                                    .series_count = market_data.series_count});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<assemble_bundle_request>(
            cmd_json, "Report execution failed during bundle assembly");
    };

    return s;
}

[[nodiscard]] inline workflow_step_def make_resolve_prepared_input() {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "resolve_prepared_input";
    s.description = "Resolve the prepared input archive the run configuration names.";
    s.command_subject = std::string(resolve_prepared_input_request::nats_subject);
    s.timeout = data_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command = [](const std::string& request_json,
                         const workflow_step_results&) -> std::string {
        const auto req = require_run_request(request_json, "resolve_prepared_input");
        return rfl::json::write(
            resolve_prepared_input_request{.report_instance_id = req.report_instance_id,
                                           .tenant_id = req.tenant_id,
                                           .correlation_id = req.correlation_id,
                                           .prepared_input_key = req.prepared_input_key});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<resolve_prepared_input_request>(
            cmd_json, "Report execution failed during input resolution");
    };

    return s;
}

[[nodiscard]] inline workflow_step_def make_prepare_ore_package() {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "prepare_ore_package";
    s.description = "Package trades and market data into ORE XML tarballs for compute.";
    s.command_subject = std::string(prepare_ore_package_request::nats_subject);
    s.timeout = data_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command = [](const std::string& request_json,
                         const workflow_step_results& prev) -> std::string {
        const auto req = require_run_request(request_json, "prepare_ore_package");
        const auto bundle = require_step_result<assemble_bundle_result>(
            prev, "assemble_bundle", "prepare_ore_package");
        const auto trades =
            require_step_result<gather_trades_result>(prev, "gather_trades", "prepare_ore_package");
        const auto market_data = require_step_result<gather_market_data_result>(
            prev, "gather_market_data", "prepare_ore_package");

        return rfl::json::write(
            prepare_ore_package_request{.report_instance_id = req.report_instance_id,
                                        .bundle_id = bundle.bundle_id,
                                        .tenant_id = req.tenant_id,
                                        .correlation_id = req.correlation_id,
                                        .trades_storage_key = trades.storage_key,
                                        .market_data_storage_key = market_data.storage_key});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<prepare_ore_package_request>(
            cmd_json, "Report execution failed during ORE package preparation");
    };

    return s;
}

/**
 * @brief Submission, reading whichever step filled the pre-processing role.
 */
[[nodiscard]] inline workflow_step_def make_submit_compute(std::string input_step) {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "submit_compute";
    s.description = "Submit ORE packages to the compute grid for pricing.";
    s.command_subject = std::string(submit_compute_request::nats_subject);
    s.timeout = compute_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command =
        [input_step = std::move(input_step)](const std::string& request_json,
                                             const workflow_step_results& prev) -> std::string {
        const auto req = require_run_request(request_json, "submit_compute");
        const auto input =
            require_step_result<prepare_ore_package_result>(prev, input_step, "submit_compute");

        return rfl::json::write(submit_compute_request{.report_instance_id = req.report_instance_id,
                                                       .tenant_id = req.tenant_id,
                                                       .correlation_id = req.correlation_id,
                                                       .tarball_uris = input.tarball_uris});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<submit_compute_request>(
            cmd_json, "Report execution failed during compute submission");
    };

    return s;
}

[[nodiscard]] inline workflow_step_def make_collect_compute_results() {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "collect_compute_results";
    s.description = "Collect and aggregate compute grid results.";
    s.command_subject = std::string(collect_compute_results_request::nats_subject);
    s.timeout = compute_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command = [](const std::string& request_json,
                         const workflow_step_results& prev) -> std::string {
        const auto req = require_run_request(request_json, "collect_compute_results");
        const auto submitted = require_step_result<submit_compute_result>(
            prev, "submit_compute", "collect_compute_results");

        return rfl::json::write(
            collect_compute_results_request{.report_instance_id = req.report_instance_id,
                                            .tenant_id = req.tenant_id,
                                            .correlation_id = req.correlation_id,
                                            .batch_id = submitted.batch_id});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<collect_compute_results_request>(
            cmd_json, "Report execution failed during result collection");
    };

    return s;
}

/**
 * @brief Post-processing, substituted: the batch is named and not read.
 */
[[nodiscard]] inline workflow_step_def make_ignore_compute_results() {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "ignore_compute_results";
    s.description = "Record the compute batch and leave its results unread.";
    s.command_subject = std::string(ignore_compute_results_request::nats_subject);
    s.timeout = data_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command = [](const std::string& request_json,
                         const workflow_step_results& prev) -> std::string {
        const auto req = require_run_request(request_json, "ignore_compute_results");
        const auto submitted = require_step_result<submit_compute_result>(
            prev, "submit_compute", "ignore_compute_results");

        return rfl::json::write(
            ignore_compute_results_request{.report_instance_id = req.report_instance_id,
                                           .tenant_id = req.tenant_id,
                                           .correlation_id = req.correlation_id,
                                           .batch_id = submitted.batch_id});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<ignore_compute_results_request>(
            cmd_json, "Report execution failed during result ingestion");
    };

    return s;
}

[[nodiscard]] inline workflow_step_def make_finalise() {
    using namespace ores::reporting::messaging;
    workflow_step_def s;
    s.name = "finalise";
    s.description = "Mark the report instance as completed.";
    s.command_subject = std::string(finalise_report_request::nats_subject);
    s.timeout = data_step_timeout;
    s.compensation_subject = std::string(fail_report_request::nats_subject);

    s.build_command = [](const std::string& request_json,
                         const workflow_step_results&) -> std::string {
        const auto req = require_run_request(request_json, "finalise");
        return rfl::json::write(
            finalise_report_request{.report_instance_id = req.report_instance_id,
                                    .tenant_id = req.tenant_id,
                                    .correlation_id = req.correlation_id});
    };

    s.build_compensation = [](const std::string& cmd_json, const std::string&) -> std::string {
        return fail_report_from<finalise_report_request>(cmd_json,
                                                         "Workflow compensation triggered");
    };

    return s;
}

} // namespace detail

/**
 * @brief Registers the report_execution_workflow definition.
 *
 * The chain is a pure function of the run configuration carried in the start
 * message, because the engine rebuilds it on every dispatch and on recovery.
 * A run that executes every phase is: gather_trades, gather_market_data,
 * assemble_bundle, prepare_ore_package, submit_compute, collect_compute_results,
 * finalise. A run that substitutes pre-processing replaces the first four with
 * resolve_prepared_input; one that ignores results replaces
 * collect_compute_results with ignore_compute_results.
 *
 * Compensation: fail_report marks the instance as failed on any step failure.
 */
inline void
register_report_execution_workflow(ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = "report_execution_workflow";
    def.description = "Executes a risk report end-to-end: prepares the engine's input, "
                      "submits it to the compute grid, records the results, and finalises "
                      "the report.";

    def.build_steps = [](const std::string& request_json,
                         const std::string& /*tenant_id*/,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        const auto req = detail::require_run_request(request_json, "build_steps");
        std::vector<workflow_step_def> steps;

        std::string input_step;
        if (req.pre_processing == pre_processing_substitute) {
            // The substitute resolves an archive the definition names, so a run
            // that names none has no chain to build. Refusing here fails the run
            // before a step is dispatched rather than at the step that needs it.
            if (req.prepared_input_key.empty())
                throw std::runtime_error("report_execution_workflow: pre-processing is "
                                         "substituted but no prepared input is named");
            input_step = "resolve_prepared_input";
            steps.push_back(detail::make_resolve_prepared_input());
        } else if (req.pre_processing == pre_processing_execute) {
            input_step = "prepare_ore_package";
            steps.push_back(detail::make_gather_trades());
            steps.push_back(detail::make_gather_market_data());
            steps.push_back(detail::make_assemble_bundle());
            steps.push_back(detail::make_prepare_ore_package());
        } else {
            throw std::runtime_error("report_execution_workflow: unknown pre-processing '" +
                                     req.pre_processing + "'");
        }

        steps.push_back(detail::make_submit_compute(std::move(input_step)));

        if (req.post_processing == post_processing_ignore) {
            steps.push_back(detail::make_ignore_compute_results());
        } else if (req.post_processing == post_processing_execute) {
            steps.push_back(detail::make_collect_compute_results());
        } else {
            throw std::runtime_error("report_execution_workflow: unknown post-processing '" +
                                     req.post_processing + "'");
        }

        steps.push_back(detail::make_finalise());
        return steps;
    };

    registry.register_definition(std::move(def));
}

}

#endif

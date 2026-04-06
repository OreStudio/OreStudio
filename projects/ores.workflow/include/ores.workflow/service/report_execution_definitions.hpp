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
#ifndef ORES_WORKFLOW_SERVICE_REPORT_EXECUTION_DEFINITIONS_HPP
#define ORES_WORKFLOW_SERVICE_REPORT_EXECUTION_DEFINITIONS_HPP

#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/messaging/report_execution_protocol.hpp"
#include "ores.workflow/service/workflow_definition.hpp"
#include "ores.workflow/service/workflow_registry.hpp"

namespace ores::workflow::service {

/**
 * @brief Registers the report_execution_workflow definition.
 *
 * Initial implementation: 3 steps.
 *   0. gather_trades     — fetch trades by book scope (ores.reporting.service)
 *   1. assemble_bundle   — persist aggregated input data (ores.reporting.service)
 *   2. finalise          — mark report instance completed (ores.reporting.service)
 *
 * Additional steps (gather_market_data, prepare_ore_package,
 * submit_compute) are wired in as they are implemented.
 *
 * Compensation: gather_trades is read-only (no compensation).
 * assemble_bundle compensation deletes the bundle row.
 * On any failure, fail_report marks the instance as failed.
 */
inline void register_report_execution_workflow(workflow_registry& registry) {
    using namespace ores::reporting::messaging;

    workflow_definition def;
    def.type_name = "report_execution_workflow";

    // ----------------------------------------------------------------
    // Step 0: gather trades
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "gather_trades";
        s.command_subject = std::string(gather_trades_request::nats_subject);
        // Compensation marks the report instance as failed.
        s.compensation_subject = std::string(fail_report_request::nats_subject);

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>&) -> std::string {
            auto req = rfl::json::read<report_execution_request>(request_json);
            if (!req) return "{}";
            return rfl::json::write(gather_trades_request{
                .report_instance_id = req->report_instance_id,
                .definition_id      = req->definition_id,
                .tenant_id          = req->tenant_id,
                .correlation_id     = req->correlation_id});
        };

        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {
            auto cmd = rfl::json::read<gather_trades_request>(cmd_json);
            if (!cmd) return "{}";
            return rfl::json::write(fail_report_request{
                .report_instance_id = cmd->report_instance_id,
                .tenant_id          = cmd->tenant_id,
                .correlation_id     = cmd->correlation_id,
                .error_message      = "Report execution failed during trade gathering"});
        };

        def.steps.push_back(std::move(s));
    }

    // ----------------------------------------------------------------
    // Step 1: gather market data
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "gather_market_data";
        s.command_subject = std::string(gather_market_data_request::nats_subject);
        s.compensation_subject = std::string(fail_report_request::nats_subject);

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>&) -> std::string {
            auto req = rfl::json::read<report_execution_request>(request_json);
            if (!req) return "{}";
            return rfl::json::write(gather_market_data_request{
                .report_instance_id = req->report_instance_id,
                .definition_id      = req->definition_id,
                .tenant_id          = req->tenant_id,
                .correlation_id     = req->correlation_id});
        };

        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {
            auto cmd = rfl::json::read<gather_market_data_request>(cmd_json);
            if (!cmd) return "{}";
            return rfl::json::write(fail_report_request{
                .report_instance_id = cmd->report_instance_id,
                .tenant_id          = cmd->tenant_id,
                .correlation_id     = cmd->correlation_id,
                .error_message      = "Report execution failed during market data gathering"});
        };

        def.steps.push_back(std::move(s));
    }

    // ----------------------------------------------------------------
    // Step 2: assemble report input bundle
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "assemble_bundle";
        s.command_subject = std::string(assemble_bundle_request::nats_subject);
        s.compensation_subject = std::string(fail_report_request::nats_subject);

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>& results) -> std::string {
            auto req = rfl::json::read<report_execution_request>(request_json);
            if (!req) return "{}";

            assemble_bundle_request cmd;
            cmd.report_instance_id = req->report_instance_id;
            cmd.definition_id      = req->definition_id;
            cmd.tenant_id          = req->tenant_id;
            cmd.correlation_id     = req->correlation_id;

            // Extract trades info from step 0 result.
            if (results.size() > 0) {
                if (auto r = rfl::json::read<gather_trades_result>(results[0])) {
                    cmd.trades_storage_key = r->storage_key;
                    cmd.trade_count        = r->trade_count;
                }
            }
            // Extract market data info from step 1 result.
            if (results.size() > 1) {
                if (auto r = rfl::json::read<gather_market_data_result>(results[1])) {
                    cmd.market_data_storage_key = r->storage_key;
                    cmd.series_count            = r->series_count;
                }
            }

            return rfl::json::write(cmd);
        };

        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {
            auto cmd = rfl::json::read<assemble_bundle_request>(cmd_json);
            if (!cmd) return "{}";
            return rfl::json::write(fail_report_request{
                .report_instance_id = cmd->report_instance_id,
                .tenant_id          = cmd->tenant_id,
                .correlation_id     = cmd->correlation_id,
                .error_message      = "Report execution failed during bundle assembly"});
        };

        def.steps.push_back(std::move(s));
    }

    // ----------------------------------------------------------------
    // Step 3: ORE package preparation (ore.service)
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "prepare_ore_package";
        s.command_subject = std::string(prepare_ore_package_request::nats_subject);
        s.compensation_subject = std::string(fail_report_request::nats_subject);

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>& results) -> std::string {
            auto req = rfl::json::read<report_execution_request>(request_json);
            if (!req) return "{}";

            prepare_ore_package_request cmd;
            cmd.report_instance_id = req->report_instance_id;
            cmd.tenant_id          = req->tenant_id;
            cmd.correlation_id     = req->correlation_id;

            // Extract bundle info from step 2 (assemble_bundle) result.
            if (results.size() > 2) {
                if (auto r = rfl::json::read<assemble_bundle_result>(results[2])) {
                    cmd.bundle_id = r->bundle_id;
                }
            }
            // Forward storage keys from step 0 and step 1 results.
            if (results.size() > 0) {
                if (auto r = rfl::json::read<gather_trades_result>(results[0])) {
                    cmd.trades_storage_key = r->storage_key;
                }
            }
            if (results.size() > 1) {
                if (auto r = rfl::json::read<gather_market_data_result>(results[1])) {
                    cmd.market_data_storage_key = r->storage_key;
                }
            }

            return rfl::json::write(cmd);
        };

        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {
            auto cmd = rfl::json::read<prepare_ore_package_request>(cmd_json);
            if (!cmd) return "{}";
            return rfl::json::write(fail_report_request{
                .report_instance_id = cmd->report_instance_id,
                .tenant_id          = cmd->tenant_id,
                .correlation_id     = cmd->correlation_id,
                .error_message      = "Report execution failed during ORE package preparation"});
        };

        def.steps.push_back(std::move(s));
    }

    // ----------------------------------------------------------------
    // Step 4: submit to compute grid (compute.service)
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "submit_compute";
        s.command_subject = std::string(submit_compute_request::nats_subject);
        s.compensation_subject = std::string(fail_report_request::nats_subject);

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>& results) -> std::string {
            auto req = rfl::json::read<report_execution_request>(request_json);
            if (!req) return "{}";

            submit_compute_request cmd;
            cmd.report_instance_id = req->report_instance_id;
            cmd.tenant_id          = req->tenant_id;
            cmd.correlation_id     = req->correlation_id;

            // Extract tarball URIs from step 3 (prepare_ore_package) result.
            if (results.size() > 3) {
                if (auto r = rfl::json::read<prepare_ore_package_result>(results[3])) {
                    cmd.tarball_uris = r->tarball_uris;
                }
            }

            return rfl::json::write(cmd);
        };

        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {
            auto cmd = rfl::json::read<submit_compute_request>(cmd_json);
            if (!cmd) return "{}";
            return rfl::json::write(fail_report_request{
                .report_instance_id = cmd->report_instance_id,
                .tenant_id          = cmd->tenant_id,
                .correlation_id     = cmd->correlation_id,
                .error_message      = "Report execution failed during compute submission"});
        };

        def.steps.push_back(std::move(s));
    }

    // ----------------------------------------------------------------
    // Step 5: collect compute results (stub pass-through — Phase 3.11)
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "collect_compute_results";
        s.command_subject = std::string(collect_compute_results_request::nats_subject);
        s.compensation_subject = std::string(fail_report_request::nats_subject);

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>& results) -> std::string {
            auto req = rfl::json::read<report_execution_request>(request_json);
            if (!req) return "{}";

            collect_compute_results_request cmd;
            cmd.report_instance_id = req->report_instance_id;
            cmd.tenant_id          = req->tenant_id;
            cmd.correlation_id     = req->correlation_id;

            // Extract batch_id from step 4 (submit_compute) result.
            if (results.size() > 4) {
                if (auto r = rfl::json::read<submit_compute_result>(results[4])) {
                    cmd.batch_id = r->batch_id;
                }
            }

            return rfl::json::write(cmd);
        };

        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {
            auto cmd = rfl::json::read<collect_compute_results_request>(cmd_json);
            if (!cmd) return "{}";
            return rfl::json::write(fail_report_request{
                .report_instance_id = cmd->report_instance_id,
                .tenant_id          = cmd->tenant_id,
                .correlation_id     = cmd->correlation_id,
                .error_message      = "Report execution failed during result collection"});
        };

        def.steps.push_back(std::move(s));
    }

    // ----------------------------------------------------------------
    // Step 6: finalise report instance
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "finalise";
        s.command_subject = std::string(finalise_report_request::nats_subject);
        s.compensation_subject = std::string(fail_report_request::nats_subject);

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>&) -> std::string {
            auto req = rfl::json::read<report_execution_request>(request_json);
            if (!req) return "{}";
            return rfl::json::write(finalise_report_request{
                .report_instance_id = req->report_instance_id,
                .tenant_id          = req->tenant_id,
                .correlation_id     = req->correlation_id});
        };

        // Compensation: mark report as failed.
        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {
            auto cmd = rfl::json::read<finalise_report_request>(cmd_json);
            if (!cmd) return "{}";
            return rfl::json::write(fail_report_request{
                .report_instance_id = cmd->report_instance_id,
                .tenant_id          = cmd->tenant_id,
                .correlation_id     = cmd->correlation_id,
                .error_message      = "Workflow compensation triggered"});
        };

        def.steps.push_back(std::move(s));
    }

    registry.register_definition(std::move(def));
}

}

#endif

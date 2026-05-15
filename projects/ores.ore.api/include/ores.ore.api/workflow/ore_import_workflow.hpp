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
#ifndef ORES_ORE_API_WORKFLOW_ORE_IMPORT_WORKFLOW_HPP
#define ORES_ORE_API_WORKFLOW_ORE_IMPORT_WORKFLOW_HPP

#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.ore.api/messaging/ore_import_engine_protocol.hpp"
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"

namespace ores::ore::workflow {

/**
 * @brief Registers the ore_import_workflow definition.
 *
 * The ore_import_workflow has a single step:
 *   0. ore.v1.ore.import.execute — fetch, scan, plan, and save all ORE
 *      entities; publishes publish_step_completion with an
 *      ore_import_execute_result payload carrying all saved entity IDs.
 *
 * Compensation:
 *   ore.v1.ore.import.rollback — deletes all entities saved in step 0,
 *   in reverse order (trades → books → portfolios → currencies).
 *
 * The workflow instance's request_json is an ore_import_execute_request.
 * build_command passes it through unchanged.  build_compensation extracts
 * the saved entity IDs from the ore_import_execute_result stored as
 * the step's response_json and builds an ore_import_rollback_request.
 */
inline void register_ore_import_workflow(
    ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = "ore_import_workflow";
    def.description = "Imports an ORE XML tarball: downloads from storage, "
        "parses XML, maps to domain entities, and persists all "
        "currencies, portfolios, books, and trades.";

    def.build_steps = [](const std::string& /*request_json*/,
        const std::string& /*tenant_id*/,
        const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {

        std::vector<workflow_step_def> steps;

        // ----------------------------------------------------------------
        // Step 0: execute the full ORE import
        // ----------------------------------------------------------------
        {
            workflow_step_def s;
            s.name = "ore_import_execute";
            s.description = "Fetch tarball from storage, parse ORE XML, map to "
                "domain entities, and persist all to repositories.";
            s.command_subject = std::string(
                ores::ore::messaging::ore_import_execute_request::nats_subject);
            s.compensation_subject = std::string(
                ores::ore::messaging::ore_import_rollback_request::nats_subject);

            s.build_command = [](const std::string& request_json,
                const std::vector<std::string>&) -> std::string {
                return request_json;
            };

            s.build_compensation = [](const std::string& cmd_json,
                const std::string& result_json) -> std::string {

                using ores::ore::messaging::ore_import_execute_request;
                using ores::ore::messaging::ore_import_execute_result;
                using ores::ore::messaging::ore_import_rollback_request;

                auto cmd = rfl::json::read<ore_import_execute_request>(cmd_json);
                auto res = rfl::json::read<ore_import_execute_result>(result_json);

                ore_import_rollback_request rollback;
                if (cmd) {
                    rollback.correlation_id = cmd->correlation_id;
                    rollback.bearer_token   = cmd->bearer_token;
                }
                if (res) {
                    rollback.saved_currency_iso_codes = res->saved_currency_iso_codes;
                    rollback.saved_portfolio_ids      = res->saved_portfolio_ids;
                    rollback.saved_book_ids           = res->saved_book_ids;
                    rollback.saved_trade_ids          = res->saved_trade_ids;
                }
                return rfl::json::write(rollback);
            };

            steps.push_back(std::move(s));
        }

        return steps;
    };

    registry.register_definition(std::move(def));
}

}

#endif

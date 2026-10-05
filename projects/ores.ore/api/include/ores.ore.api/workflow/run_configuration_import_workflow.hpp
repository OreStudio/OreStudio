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
#ifndef ORES_ORE_API_WORKFLOW_RUN_CONFIGURATION_IMPORT_WORKFLOW_HPP
#define ORES_ORE_API_WORKFLOW_RUN_CONFIGURATION_IMPORT_WORKFLOW_HPP

#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <rfl/json.hpp>

namespace ores::ore::workflow {

/**
 * @brief Registers the run_configuration_import_workflow definition.
 *
 * One step:
 *   0. ore.v1.run_configuration.import.execute: maps the run's files and
 *      stores each document through the component that owns it, then answers
 *      with a run_configuration_import_execute_result naming what it stored.
 *      A failure part way deletes what the step had stored before it fails.
 *
 * Compensation:
 *   ore.v1.run_configuration.import.rollback: deletes what the step stored,
 *   from the result it answered with.
 *
 * The instance's request_json is a run_configuration_import_execute_request,
 * which build_command passes through unchanged.
 */
inline void
register_run_configuration_import_workflow(ores::workflow::service::workflow_registry& registry) {
    using namespace ores::workflow::service;
    using namespace ores::ore::messaging;

    workflow_definition def;
    def.type_name = "run_configuration_import_workflow";
    def.description = "Imports an ORE input directory into a report definition: stores the run "
                      "document and each configuration document through the component that "
                      "owns it.";
    def.build_steps = [](const std::string&,
                         const std::string&,
                         const std::string&) -> std::vector<workflow_step_def> {
        workflow_step_def s;
        s.name = "run_configuration_import_execute";
        s.description = "Map the run's files and store each document through its owner.";
        s.command_subject = std::string(run_configuration_import_execute_request::nats_subject);
        s.timeout = data_step_timeout;
        s.compensation_subject =
            std::string(run_configuration_import_rollback_request::nats_subject);
        s.build_command = [](const std::string& request_json,
                             const workflow_step_results&) -> std::string {
            return request_json;
        };
        s.build_compensation = [](const std::string& cmd_json,
                                  const std::string& result_json) -> std::string {
            const auto cmd = rfl::json::read<run_configuration_import_execute_request>(cmd_json);
            const auto res = rfl::json::read<run_configuration_import_execute_result>(result_json);
            run_configuration_import_rollback_request rollback;
            if (cmd) {
                rollback.correlation_id = cmd->correlation_id;
                rollback.bearer_token = cmd->bearer_token;
                rollback.report_definition_id = cmd->report_definition_id;
            }
            if (res) {
                rollback.run_document_saved = res->run_document_saved;
                rollback.saved_documents = res->saved_documents;
            }
            return rfl::json::write(rollback);
        };
        return {std::move(s)};
    };
    registry.register_definition(std::move(def));
}

}

#endif

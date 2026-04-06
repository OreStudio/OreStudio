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
#ifndef ORES_WORKFLOW_SERVICE_WORKFLOW_DEFINITION_HPP
#define ORES_WORKFLOW_SERVICE_WORKFLOW_DEFINITION_HPP

#include <functional>
#include <string>
#include <vector>

namespace ores::workflow::service {

/**
 * @brief Declarative definition of one step within a workflow.
 *
 * The workflow engine uses these descriptors to build and dispatch commands
 * without needing bespoke executor classes per workflow type.
 */
struct workflow_step_def {
    /**
     * @brief Human-readable step name stored in workflow_step.name.
     */
    std::string name;

    /**
     * @brief NATS subject to which the step command is published.
     *
     * E.g. "refdata.v1.parties.save"
     */
    std::string command_subject;

    /**
     * @brief NATS subject for the compensation command.
     *
     * Empty string means this step has no compensation action.
     * E.g. "refdata.v1.parties.delete"
     */
    std::string compensation_subject;

    /**
     * @brief Builds the step command payload.
     *
     * @param request_json  The workflow instance's originating request JSON.
     * @param step_results  Result JSON from each previously completed step,
     *                      in step-index order (index 0 = first completed step).
     * @return Serialised JSON to be published as the command body.
     */
    std::function<std::string(
        const std::string& request_json,
        const std::vector<std::string>& step_results)> build_command;

    /**
     * @brief Builds the compensation command payload.
     *
     * Called when compensation is triggered after this step completed.
     *
     * @param command_json  The original command payload sent for this step.
     * @param result_json   The result payload received from the domain service.
     * @return Serialised JSON to be published as the compensation command body.
     */
    std::function<std::string(
        const std::string& command_json,
        const std::string& result_json)> build_compensation;
};

/**
 * @brief Declarative definition of a complete named workflow.
 *
 * Registered once at startup in the workflow_registry. The engine looks up
 * the definition by type_name to know the step sequence and how to build
 * each command.
 */
struct workflow_definition {
    /**
     * @brief Unique type name matching workflow_instance.type.
     *
     * E.g. "provision_parties_workflow"
     */
    std::string type_name;

    /**
     * @brief Ordered sequence of step descriptors.
     *
     * Steps execute in order from index 0. On success the engine advances
     * to the next step. On failure the engine compensates in reverse order.
     */
    std::vector<workflow_step_def> steps;
};

}

#endif

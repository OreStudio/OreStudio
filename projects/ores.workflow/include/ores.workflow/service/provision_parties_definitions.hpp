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
#ifndef ORES_WORKFLOW_SERVICE_PROVISION_PARTIES_DEFINITIONS_HPP
#define ORES_WORKFLOW_SERVICE_PROVISION_PARTIES_DEFINITIONS_HPP

#include "ores.workflow/service/workflow_definition.hpp"
#include "ores.workflow/service/workflow_registry.hpp"

namespace ores::workflow::service {

/**
 * @brief Registers the provision_parties workflow definition.
 *
 * The provision_parties workflow has three steps per party:
 *   0. refdata.v1.parties.save      — create party record
 *   1. iam.v1.accounts.save         — create IAM account
 *   2. iam.v1.account-parties.save  — link account to party
 *
 * Compensation (reverse order):
 *   iam.v1.account-parties.delete
 *   iam.v1.accounts.delete
 *   refdata.v1.parties.delete
 *
 * NOTE (Phase 1): The provision_parties workflow currently still uses the
 * synchronous executor (provision_parties_workflow.cpp). The definition
 * registered here enables the engine infrastructure but is not yet active.
 * Phase 2.1 will add X-Workflow-Step-Id handling to the domain services,
 * remove the synchronous executor, and activate the event-driven path.
 */
inline void register_provision_parties_workflow(workflow_registry& registry) {
    workflow_definition def;
    def.type_name = "provision_parties_workflow";

    // Step 0: save party (refdata)
    {
        workflow_step_def s;
        s.name = "save_party";
        s.command_subject = "refdata.v1.parties.save";
        s.compensation_subject = "refdata.v1.parties.delete";
        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>&) {
            // Phase 2.1: extract party fields from request_json.
            // Placeholder: forward the full request.
            return request_json;
        };
        s.build_compensation = [](const std::string&,
            const std::string& result_json) {
            // Phase 2.1: build delete request from result_json party_id.
            return result_json;
        };
        def.steps.push_back(std::move(s));
    }

    // Step 1: save account (IAM)
    {
        workflow_step_def s;
        s.name = "save_account";
        s.command_subject = "iam.v1.accounts.save";
        s.compensation_subject = "iam.v1.accounts.delete";
        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>&) {
            return request_json;
        };
        s.build_compensation = [](const std::string&,
            const std::string& result_json) {
            return result_json;
        };
        def.steps.push_back(std::move(s));
    }

    // Step 2: link account to party (IAM)
    {
        workflow_step_def s;
        s.name = "link_account_party";
        s.command_subject = "iam.v1.account-parties.save";
        s.compensation_subject = "iam.v1.account-parties.delete";
        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>&) {
            return request_json;
        };
        s.build_compensation = [](const std::string&,
            const std::string& result_json) {
            return result_json;
        };
        def.steps.push_back(std::move(s));
    }

    registry.register_definition(std::move(def));
}

}

#endif

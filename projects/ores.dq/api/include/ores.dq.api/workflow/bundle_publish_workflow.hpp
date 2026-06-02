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
#ifndef ORES_DQ_API_WORKFLOW_BUNDLE_PUBLISH_WORKFLOW_HPP
#define ORES_DQ_API_WORKFLOW_BUNDLE_PUBLISH_WORKFLOW_HPP

#include <rfl/json.hpp>
#include <string>
#include <vector>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.dq.api/messaging/publish_from_dq_protocol.hpp"
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"

namespace ores::dq::workflow {

/**
 * @brief Per-dataset entry in a bundle publish workflow request.
 *
 * Carries everything the step command builder needs to construct the
 * publish_from_dq_command for one dataset without re-querying the database.
 */
struct bundle_publish_workflow_dataset {
    std::string dataset_id;
    std::string dataset_code;
    std::string target_subject;
    std::string mode;
    std::string params_json;
};

/**
 * @brief Workflow request for publishing an entire bundle.
 *
 * Serialised as request_json in start_workflow_message. The build_steps
 * lambda deserialises this to produce one workflow_step_def per dataset.
 */
struct bundle_publish_workflow_request {
    std::string bundle_code;
    std::string tenant_id;
    std::string published_by;
    std::vector<bundle_publish_workflow_dataset> datasets;
};

/**
 * @brief Registers the bundle_publish_workflow definition.
 *
 * One step per publishable dataset in the bundle. Each step dispatches a
 * publish_from_dq_command to the dataset's target_subject. Steps have no
 * compensation action — publish functions are idempotent upserts.
 */
inline void register_bundle_publish_workflow(
    ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name  = "bundle_publish_workflow";
    def.description = "Publishes all datasets in a DQ bundle to their target "
        "production tables. One workflow step per dataset; steps are dispatched "
        "in display_order. No compensation — publish functions are idempotent.";

    def.build_steps = [](const std::string& request_json,
        const std::string& /*tenant_id*/,
        const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {

        auto parsed = rfl::json::read<bundle_publish_workflow_request>(request_json);
        if (!parsed) return {};

        const auto& req = *parsed;
        std::vector<workflow_step_def> steps;
        steps.reserve(req.datasets.size());

        for (const auto& ds : req.datasets) {
            workflow_step_def s;
            s.name = ds.dataset_code;
            s.description = "Publish " + ds.dataset_code + " via " + ds.target_subject;
            s.command_subject = ds.target_subject;
            s.compensation_subject = "";

            const std::string dataset_id   = ds.dataset_id;
            const std::string tenant_id_v  = req.tenant_id;
            const std::string mode         = ds.mode;
            const std::string params       = ds.params_json;

            s.build_command = [dataset_id, tenant_id_v, mode, params](
                const std::string& /*request_json*/,
                const std::vector<std::string>& /*step_results*/) -> std::string {

                ores::dq::messaging::publish_from_dq_command cmd;
                cmd.dataset_id  = dataset_id;
                cmd.tenant_id   = tenant_id_v;
                cmd.mode        = mode;
                cmd.params_json = params;
                return rfl::json::write(cmd);
            };

            s.build_compensation = [](const std::string&,
                const std::string&) -> std::string { return "{}"; };

            steps.push_back(std::move(s));
        }

        return steps;
    };

    registry.register_definition(std::move(def));
}

}

#endif

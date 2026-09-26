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
#ifndef ORES_WORKFLOW_API_WORKFLOW_IDENTITY_WORKFLOW_HPP
#define ORES_WORKFLOW_API_WORKFLOW_IDENTITY_WORKFLOW_HPP

#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <rfl/json.hpp>
#include <string>
#include <vector>

namespace ores::workflow::workflow {

/**
 * @brief What one step of the identity workflow should do.
 *
 * The behaviour is the point of the fixture: the engine's paths differ by what a
 * step reports, not by what it computes, so a step that declares its outcome lets
 * one definition cover the happy path, the warning path, the failure path and the
 * compensating path without a definition each.
 */
struct identity_step_request {
    std::string name;
    /// @c complete, @c warn or @c fail. Anything else is refused at build time.
    std::string behaviour = "complete";
    /// Whether the step declares a compensation subject, which is what decides
    /// whether a later failure unwinds or simply fails.
    bool compensate = false;
    /// Seconds to wait before reporting, so a run can be observed in flight.
    int delay_seconds = 0;
};

/**
 * @brief The identity workflow's request: the steps to run, in order.
 *
 * Serialised as request_json in start_workflow_message.
 */
struct identity_workflow_request {
    std::vector<identity_step_request> steps;
};

/// The subject a step's command is dispatched to. One subject serves every step,
/// because the payload says what the step does.
inline constexpr std::string_view identity_step_command_subject =
    "workflow.v1.identity.step";

/// The subject a step's compensation is dispatched to.
inline constexpr std::string_view identity_compensation_command_subject =
    "workflow.v1.identity.compensate";

/**
 * @brief Registers the identity_workflow definition.
 *
 * A fixture the component runs against itself. Its steps do no domain work: each
 * one reports the outcome its request declares, so the engine's start, dispatch,
 * success, warning, failure and compensation paths are all reachable from the
 * shell with no other component's data.
 *
 * Steps are dispatched to @ref identity_step_command_subject, which the service
 * serves itself rather than delegating to a commissioner.
 */
inline void register_identity_workflow(ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = "identity_workflow";
    def.description = "Runs the steps its request declares and reports the outcome each one "
                      "asks for. A fixture for exercising the engine: no domain work, no other "
                      "component, one step per declared entry.";

    def.build_steps = [](const std::string& request_json,
                         const std::string& /*tenant_id*/,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        auto parsed = rfl::json::read<identity_workflow_request>(request_json);
        if (!parsed)
            return {};

        std::vector<workflow_step_def> steps;
        steps.reserve(parsed->steps.size());

        for (const auto& wanted : parsed->steps) {
            workflow_step_def s;
            s.name = wanted.name;
            s.description = "Identity step '" + wanted.name + "' reporting '" + wanted.behaviour +
                            "'";
            s.command_subject = std::string(identity_step_command_subject);
            s.compensation_subject =
                wanted.compensate ? std::string(identity_compensation_command_subject) : "";

            const auto behaviour = wanted.behaviour;
            const auto delay = wanted.delay_seconds;
            const auto name = wanted.name;

            // The command carries what the handler needs to know, so the handler
            // itself stays a reader of the payload rather than a decision-maker.
            s.build_command = [name, behaviour, delay](const std::string& /*request_json*/,
                                                       const std::vector<std::string>& /*results*/)
                -> std::string {
                return rfl::json::write(identity_step_request{.name = name,
                                                              .behaviour = behaviour,
                                                              .compensate = false,
                                                              .delay_seconds = delay});
            };

            s.build_compensation = [name](const std::string&, const std::string&) -> std::string {
                return rfl::json::write(identity_step_request{.name = name,
                                                              .behaviour = "complete",
                                                              .compensate = false,
                                                              .delay_seconds = 0});
            };

            steps.push_back(std::move(s));
        }

        return steps;
    };

    registry.register_definition(std::move(def));
}

}

#endif

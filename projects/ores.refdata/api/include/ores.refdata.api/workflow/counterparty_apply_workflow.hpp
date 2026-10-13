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
#ifndef ORES_REFDATA_API_WORKFLOW_COUNTERPARTY_APPLY_WORKFLOW_HPP
#define ORES_REFDATA_API_WORKFLOW_COUNTERPARTY_APPLY_WORKFLOW_HPP

#include "ores.inbox.api/workflow/apply_step_command.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <rfl/json.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace ores::refdata::workflow {

using ores::workflow::service::write_step_timeout;

inline constexpr std::string_view counterparty_apply_workflow_type = "counterparty_apply_workflow";
inline constexpr std::string_view recheck_counterparty_changes_subject =
    "refdata.v1.steps.recheck_counterparty_changes";
inline constexpr std::string_view apply_counterparty_changes_subject =
    "refdata.v1.steps.apply_counterparty_changes";

/**
 * @brief The request that starts a counterparty apply: the approved request to apply.
 */
struct counterparty_apply_workflow_request {
    std::string request_id;
};

/**
 * @brief Registers the counterparty_apply_workflow definition.
 *
 * Three steps, each idempotent and each with a deadline:
 *  - recheck reads the stored lines against the live rows, because the rows
 *    may have moved since the raise;
 *  - apply writes every line in one transaction and marks each applied once;
 *  - tell says how it ended to the person who asked.
 *
 * The policy is stop. The work that matters is one transaction that either
 * happened or did not, so the recovery wanted is a retry and not a rollback. A
 * refusal ends the request in apply_failed and is never retried. A failure of
 * the infrastructure leaves the request approved and the run stopped, for the
 * engine's retry verb to resume.
 */
inline void
register_counterparty_apply_workflow(ores::workflow::service::workflow_registry& registry) {
    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = std::string(counterparty_apply_workflow_type);
    def.description = "Applies the stored lines of an approved counterparty request: re-checks "
                      "them against the live rows, writes every line in one transaction, "
                      "and tells the person who asked. Stops on a failure and keeps the "
                      "completed steps, because the recovery wanted is a retry.";
    def.on_failure = failure_policy::stop;

    def.build_steps = [](const std::string& request_json,
                         const std::string& /*tenant_id*/,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        // A start that names no request has no steps. The engine logs that and
        // creates no instance, which is the refusal a caller needs.
        const auto parsed = rfl::json::read<counterparty_apply_workflow_request>(request_json);
        if (!parsed || parsed->request_id.empty())
            return {};

        const auto command = rfl::json::write(
            ores::inbox::workflow::apply_step_command{.request_id = parsed->request_id});
        const auto build = [command](const std::string& /*request_json*/,
                                     const workflow_step_results& /*results*/) {
            return command;
        };

        workflow_step_def recheck;
        recheck.name = "recheck";
        recheck.label = "Re-check the changes";
        recheck.description = "Read the stored lines against the live rows.";
        recheck.command_subject = std::string(recheck_counterparty_changes_subject);
        recheck.timeout = write_step_timeout;
        recheck.build_command = build;

        workflow_step_def apply;
        apply.name = "apply";
        apply.label = "Apply the changes";
        apply.description = "Write every line in one transaction and mark each applied.";
        apply.consumes = {"recheck"};
        apply.command_subject = std::string(apply_counterparty_changes_subject);
        apply.timeout = write_step_timeout;
        apply.build_command = build;

        workflow_step_def tell;
        tell.name = "tell";
        tell.label = "Tell the person who asked";
        tell.description = "Say how the apply ended.";
        tell.consumes = {"apply"};
        tell.command_subject = std::string(ores::inbox::workflow::tell_apply_result_subject);
        tell.timeout = write_step_timeout;
        tell.build_command = build;

        return {recheck, apply, tell};
    };

    registry.register_definition(std::move(def));
}

}

#endif

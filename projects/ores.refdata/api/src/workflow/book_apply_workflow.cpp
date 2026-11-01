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
#include "ores.refdata.api/workflow/book_apply_workflow.hpp"
#include "ores.inbox.api/workflow/apply_step_command.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <exception>
#include <string>
#include <vector>

namespace ores::refdata::workflow {

using ores::workflow::service::write_step_timeout;

void register_book_apply_workflow(ores::workflow::service::workflow_registry& registry) {
    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = std::string(book_apply_workflow_type);
    def.description = "Applies the stored lines of an approved book request: re-checks "
                      "them against the live books, writes every line in one transaction, "
                      "and tells the person who asked. Stops on a failure and keeps the "
                      "completed steps, because the recovery wanted is a retry.";
    def.on_failure = failure_policy::stop;

    def.build_steps = [](const std::string& request_json,
                         const std::string& /*tenant_id*/,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        // A start that names no request has no steps. The engine logs that and
        // creates no instance, which is the refusal a caller needs.
        book_apply_workflow_request parsed;
        try {
            parsed = utility::codec::object::read_json<book_apply_workflow_request>(request_json);
        } catch (const std::exception&) {
            return {};
        }
        if (parsed.request_id.empty())
            return {};

        const auto command = utility::codec::object::write_json(
            ores::inbox::workflow::apply_step_command{.request_id = parsed.request_id});
        const auto build = [command](const std::string& /*request_json*/,
                                     const workflow_step_results& /*results*/) {
            return command;
        };

        workflow_step_def recheck;
        recheck.name = "recheck";
        recheck.label = "Re-check the changes";
        recheck.description = "Read the stored lines against the live books.";
        recheck.command_subject = std::string(recheck_book_changes_subject);
        recheck.timeout = write_step_timeout;
        recheck.build_command = build;

        workflow_step_def apply;
        apply.name = "apply";
        apply.label = "Apply the changes";
        apply.description = "Write every line in one transaction and mark each applied.";
        apply.consumes = {"recheck"};
        apply.command_subject = std::string(apply_book_changes_subject);
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

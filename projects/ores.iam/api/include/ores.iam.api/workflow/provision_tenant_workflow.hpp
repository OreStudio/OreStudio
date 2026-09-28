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
#ifndef ORES_IAM_API_WORKFLOW_PROVISION_TENANT_WORKFLOW_HPP
#define ORES_IAM_API_WORKFLOW_PROVISION_TENANT_WORKFLOW_HPP

#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <algorithm>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::iam::workflow {

/// The workflow type a provision tenant run declares in its start message. It is
/// not the request's subject: =iam.v1.tenants.provision= starts the run, and the
/// run names this type.
inline constexpr std::string_view provision_tenant_workflow_type = "provision_tenant_workflow";

/**
 * @brief The subject every step of a provisioning run is dispatched to.
 *
 * One subject serves every step, because the payload says which kind of work
 * the step is: the engine persists only a step's name and subject, so a subject
 * per kind would put the catalogue in the persisted rows as well as in code.
 * The handler on this subject is ores.iam's, which owns the orchestration, so a
 * step kind is code in this component and needs no other component to change.
 */
inline constexpr std::string_view provision_tenant_step_subject = "iam.v1.tenants.provision-step";

/// The step kind that completes the tenant, appended to every run after the
/// kinds the profile declares. A run finishes only by running its steps out, so
/// "the tenant is ready" is a step rather than a property of the definition.
inline constexpr std::string_view complete_provisioning_step_kind = "complete_provisioning";

/// The step kinds a seed profile may order. A kind that is not here is a code
/// change: the catalogue is fixed and a profile states only which kinds it
/// orders and with what arguments. The completing step is deliberately absent,
/// because every run appends it; a profile that orders it is refused rather than
/// given a second one.
inline constexpr std::string_view provision_step_kinds[] = {"publish_bundle",
                                                            "import_lei_hierarchy",
                                                            "provision_party",
                                                            "load_staff",
                                                            "attach_photos",
                                                            "start_market_feeds"};

/// Whether the catalogue knows the kind as one a profile may order.
[[nodiscard]] inline bool is_declared_step_kind(std::string_view kind) {
    return std::any_of(std::begin(provision_step_kinds),
                       std::end(provision_step_kinds),
                       [kind](std::string_view known) { return known == kind; });
}

/**
 * @brief One step the run takes, as the seed profile declared it.
 */
struct provision_tenant_step {
    /// The step kind, from the catalogue in code.
    std::string kind;
    /// The kind's own arguments, as the profile's row states them.
    std::string arguments_json;
};

/**
 * @brief One parameter value the run's steps read, by declared name.
 */
struct provision_tenant_parameter {
    std::string name;
    std::string value;
};

/**
 * @brief What a provision tenant run works from.
 *
 * Serialised as @c request_json in a @c start_workflow_message. The request
 * handler reads the profile, its declared parameters and its ordered steps
 * before it starts the run, so the definition that builds the engine steps
 * needs no database of its own and the run follows the profile as it stood
 * when the run started rather than as it stands when a step is dispatched.
 */
struct provision_tenant_workflow_request {
    std::string profile_code;
    std::string tenant_code;
    std::string tenant_hostname;
    /// The tenant administrator the run's steps act as. The engine sends a step
    /// without the caller's token, so a step's handler has to mint its own, and
    /// this is the account it mints it for.
    std::string admin_account_id;
    /// The profile's declared parameters, each with the value it took.
    std::vector<provision_tenant_parameter> parameters;
    /// The profile's ordered steps.
    std::vector<provision_tenant_step> steps;
};

/**
 * @brief What one step of a provisioning run is asked to do.
 *
 * The step handler reads the kind from here, because the engine dispatches
 * every step to one subject and the payload is what tells them apart.
 *
 * A kind reads its own arguments from @c arguments_json, and takes a value the
 * profile left out of the request's @c parameters of the same name. That is how
 * the Operational profile supplies a root LEI: its row states no arguments at
 * all, and the value is the form's.
 */
struct provision_tenant_step_command {
    std::string kind;
    /// The tenant the run provisions, which the step headers also carry. Stated
    /// here as well so a step command read from the log says what it acted on.
    std::string tenant_id;
    std::string tenant_code;
    std::string tenant_hostname;
    std::string admin_account_id;
    std::string arguments_json;
    std::vector<provision_tenant_parameter> parameters;
};

namespace detail {

/// A name no earlier step of the same run has taken, so that a later step's
/// command builder can address this step's result by name. A profile that
/// orders the same kind twice therefore yields @c publish_bundle and
/// @c publish_bundle_2 rather than two steps that cannot be told apart.
[[nodiscard]] inline std::string unique_step_name(const std::string& kind,
                                                  std::unordered_map<std::string, int>& seen) {
    const auto count = ++seen[kind];
    return count == 1 ? kind : kind + "_" + std::to_string(count);
}

} // namespace detail

/**
 * @brief Registers the provision_tenant_workflow definition.
 *
 * One engine step per step kind the run's request declares, in the order the
 * profile declared them, plus one final step that completes the tenant. Every
 * step is dispatched to @ref provision_tenant_step_subject, which ores.iam
 * serves: the engine sends a step and waits for the handler to report it, so a
 * step's kind must have an executor there.
 *
 * A kind outside the catalogue is refused by throwing, which the engine logs
 * and abandons. The request handler refuses it first, naming the kind, so the
 * throw is the guard against a request that never came through that handler.
 */
inline void
register_provision_tenant_workflow(ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = std::string(provision_tenant_workflow_type);
    def.description =
        "Provisions a tenant from a seed profile: publishes the bundles the profile orders, "
        "imports its LEI hierarchy, provisions its parties, and completes the tenant. One engine "
        "step per declared step kind, in the profile's order, plus the step that completes it.";

    def.build_steps = [](const std::string& request_json,
                         const std::string& tenant_id,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        auto parsed = rfl::json::read<provision_tenant_workflow_request>(request_json);
        if (!parsed)
            throw std::runtime_error(
                "A provision tenant run was started with a request this deployment cannot read.");

        std::vector<workflow_step_def> steps;
        steps.reserve(parsed->steps.size() + 1);
        std::unordered_map<std::string, int> seen;

        const auto add_step = [&](const std::string& kind, const std::string& arguments_json) {
            workflow_step_def step;
            step.name = detail::unique_step_name(kind, seen);
            step.description = "Provisioning step '" + kind + "'";
            step.command_subject = std::string(provision_tenant_step_subject);
            step.compensation_subject = "";

            provision_tenant_step_command command;
            command.kind = kind;
            command.tenant_id = tenant_id;
            command.tenant_code = parsed->tenant_code;
            command.tenant_hostname = parsed->tenant_hostname;
            command.admin_account_id = parsed->admin_account_id;
            command.arguments_json = arguments_json;
            command.parameters = parsed->parameters;

            step.build_command = [command](const std::string&, const workflow_step_results&) {
                return rfl::json::write(command);
            };
            step.build_compensation = [](const std::string&, const std::string&) {
                return "{}";
            };
            steps.push_back(std::move(step));
        };

        for (const auto& declared : parsed->steps) {
            if (declared.kind == complete_provisioning_step_kind)
                throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                         "', which every run appends itself.");
            if (!is_declared_step_kind(declared.kind))
                throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                         "', which this deployment does not know.");
            add_step(declared.kind, declared.arguments_json);
        }

        add_step(std::string(complete_provisioning_step_kind), "{}");
        return steps;
    };

    registry.register_definition(std::move(def));
}

}

#endif

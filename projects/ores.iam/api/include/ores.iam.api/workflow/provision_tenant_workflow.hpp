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

// The budgets a step states belong to the engine's vocabulary, so a definition
// reads them from one place rather than inventing its own numbers.
using ores::workflow::service::data_step_timeout;
using ores::workflow::service::orchestrating_step_timeout;
using ores::workflow::service::write_step_timeout;

/// The workflow type a provision tenant run declares in its start message. It is
/// not the request's subject: =iam.v1.ops.provision_tenant= starts the run, and the
/// run names this type.
inline constexpr std::string_view provision_tenant_workflow_type = "provision_tenant_workflow";

/// The workflow type a run that provisions one party of an existing tenant
/// declares in its start message. It is not the request's subject either:
/// =iam.v1.ops.provision_party= starts the run, and the run names this type.
inline constexpr std::string_view provision_party_workflow_type = "provision_party_workflow";

/**
 * @brief What each run acts on, as the start message states it.
 *
 * The engine stores the pair and never reads it, so the word a component chooses
 * is its own. These two are the pair ores.iam puts on the runs it starts, and a
 * component that wants to find the work it has in flight on one of its own
 * entities asks for the same pair.
 */
inline constexpr std::string_view provision_tenant_target_kind = "tenant";
inline constexpr std::string_view provision_party_target_kind = "party";

/**
 * @brief The subject every step of a provisioning run is dispatched to.
 *
 * One subject serves every step, because the payload says which kind of work
 * the step is: the engine persists only a step's name and subject, so a subject
 * per kind would put the catalogue in the persisted rows as well as in code.
 * The handler on this subject is ores.iam's, which owns the orchestration, so a
 * step kind is code in this component and needs no other component to change.
 */
inline constexpr std::string_view provision_tenant_step_subject = "iam.v1.ops.provision_tenant-step";

/// The step kind that completes the tenant, appended to every run after the
/// kinds the profile declares. A run finishes only by running its steps out, so
/// "the tenant is ready" is a step rather than a property of the definition.
inline constexpr std::string_view complete_provisioning_step_kind = "complete_provisioning";

/// The step kinds the notation knows, each named once so a handler that reads
/// the catalogue by kind names the same string the catalogue declares.
inline constexpr std::string_view system_provision_step_kind = "system_provision";
inline constexpr std::string_view publish_bundle_step_kind = "publish_bundle";
inline constexpr std::string_view import_lei_hierarchy_step_kind = "import_lei_hierarchy";
inline constexpr std::string_view provision_party_step_kind = "provision_party";
inline constexpr std::string_view load_staff_step_kind = "load_staff";
inline constexpr std::string_view attach_photos_step_kind = "attach_photos";
inline constexpr std::string_view start_market_feeds_step_kind = "start_market_feeds";

/// The step kinds the notation knows. A kind that is not here is a code change:
/// the catalogue is fixed and a profile states only which kinds it orders and
/// with what arguments. The completing step is deliberately absent, because
/// every run appends it; a profile that orders it is refused rather than given
/// a second one. Not every kind here is one this build executes; see
/// @ref provision_executed_step_kinds for the subset a profile may order today.
inline constexpr std::string_view provision_step_kinds[] = {system_provision_step_kind,
                                                            publish_bundle_step_kind,
                                                            import_lei_hierarchy_step_kind,
                                                            provision_party_step_kind,
                                                            load_staff_step_kind,
                                                            attach_photos_step_kind,
                                                            start_market_feeds_step_kind};

/// The bundle the system-provisioning step publishes. Its members are the
/// datasets the installation itself is read from, so they belong to the system
/// tenant and never to a profile: a profile describes one tenant, and these
/// rows are not one. Extending what the installation publishes is a change to
/// this bundle's membership, not to the step.
inline constexpr std::string_view system_core_bundle_code = "system_core";

/**
 * @brief What a person is shown for one step kind, and what it does.
 *
 * The catalogue above states the notation; these are the words a screen shows
 * for it. They live beside the kinds because a kind is code: a new kind cannot
 * be added without saying what it is in a person's terms. The words travel
 * with the run rather than being written where they are read, so the browser,
 * the shell and a log all state one thing about a step.
 */
struct step_kind_words {
    std::string_view label;
    std::string_view description;
    /**
     * @brief How long the kind may run before the engine fails it.
     *
     * The value belongs beside the words for the same reason they do: how long
     * a kind takes is knowledge about the kind, and a new kind cannot be added
     * here without stating both.
     */
    std::chrono::seconds timeout;
};

/**
 * @brief The words for a kind, or the kind itself when this build has none.
 *
 * An unknown kind is one the executor refuses, so its identity is the most a
 * screen can honestly say about it.
 */
[[nodiscard]] inline step_kind_words words_for_step_kind(std::string_view kind) {
    if (kind == system_provision_step_kind)
        return {"Publish the system's own data",
                "Publishes the datasets the installation itself is read from into the system "
                "tenant, before the tenant has any of its own.",
                orchestrating_step_timeout};
    if (kind == publish_bundle_step_kind)
        return {"Publish the reference data",
                "Publishes the reference data the tenant works from: the bundles the starting "
                "point orders, and the datasets those bundles name.",
                orchestrating_step_timeout};
    if (kind == import_lei_hierarchy_step_kind)
        return {"Import the legal entities",
                "Reads the legal entity the starting point names by its LEI and the entities it "
                "consolidates, and publishes them as the tenant's parties.",
                orchestrating_step_timeout};
    if (kind == provision_party_step_kind)
        return {"Provision the parties",
                "Publishes each party's reference data, records the legal entity it was built "
                "from, activates it, marks its onboarding complete and joins the caller to it.",
                orchestrating_step_timeout};
    if (kind == load_staff_step_kind)
        return {"Load the staff",
                "Creates an account for each person the starting point lists, each in its own "
                "party.",
                orchestrating_step_timeout};
    if (kind == attach_photos_step_kind)
        return {"Attach the photographs",
                "Gives the accounts their photographs, and the tenant's party its logo.",
                write_step_timeout};
    if (kind == start_market_feeds_step_kind)
        return {"Start the market feeds",
                "Starts the synthetic market data the tenant's curves and prices are built from.",
                orchestrating_step_timeout};
    if (kind == complete_provisioning_step_kind)
        return {"Finish",
                "Marks the tenant ready: it stops bootstrapping and becomes active.",
                write_step_timeout};
    // A kind this build does not execute, which is refused before a run exists
    // to order it; the budget it states is the one that would never be reached.
    return {kind, {}, write_step_timeout};
}

/// Whether the catalogue knows the kind.
[[nodiscard]] inline bool is_declared_step_kind(std::string_view kind) {
    return std::any_of(std::begin(provision_step_kinds),
                       std::end(provision_step_kinds),
                       [kind](std::string_view known) { return known == kind; });
}

/// The declarable kinds this build executes. The catalogue above states the
/// notation; this list states which of its kinds have an executor here, so a
/// profile that orders the rest is refused before its run starts rather than
/// half-provisioned. Growing it is a code change beside the executor.
inline constexpr std::string_view provision_executed_step_kinds[] = {system_provision_step_kind,
                                                                     publish_bundle_step_kind,
                                                                     import_lei_hierarchy_step_kind,
                                                                     provision_party_step_kind,
                                                                     load_staff_step_kind,
                                                                     attach_photos_step_kind,
                                                                     start_market_feeds_step_kind};

/// Whether this build executes the kind. A kind the catalogue does not know is
/// not executed either, so one predicate answers both refusals.
[[nodiscard]] inline bool is_executed_step_kind(std::string_view kind) {
    return std::any_of(std::begin(provision_executed_step_kinds),
                       std::end(provision_executed_step_kinds),
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
    /// The tenant the run provisions. It is not the run's own tenant: a run
    /// belongs to the tenant that asked for it, so that the tenant can follow
    /// its own work, while the tenant this names is the one every step acts
    /// on.
    std::string tenant_id;
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
 * the Operational profile supplies a root LEI: its row names the bundle that
 * imports the hierarchy and leaves the LEI out, so the value is the form's.
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

/// The run's declaration, or a throw naming what the request lacks.
[[nodiscard]] inline provision_tenant_workflow_request
read_workflow_request(const std::string& request_json) {
    auto parsed = rfl::json::read<provision_tenant_workflow_request>(request_json);
    if (!parsed)
        throw std::runtime_error(
            "A provisioning run was started with a request this deployment cannot read.");
    // The run's own tenant scopes its rows; the tenant the run provisions is
    // what every step acts on, and only the request names it.
    if (parsed->tenant_id.empty())
        throw std::runtime_error(
            "A provisioning run was started with a request that names no tenant to provision.");
    return *parsed;
}

/// One engine step for one kind, dispatched to the provisioning step subject
/// with the run's tenant, administrator and parameters.
[[nodiscard]] inline ores::workflow::service::workflow_step_def
make_step(const provision_tenant_workflow_request& run,
          const std::string& kind,
          const std::string& arguments_json,
          std::unordered_map<std::string, int>& seen) {
    const auto words = words_for_step_kind(kind);

    ores::workflow::service::workflow_step_def step;
    step.name = unique_step_name(kind, seen);
    step.label = std::string(words.label);
    step.description = std::string(words.description);
    step.command_subject = std::string(provision_tenant_step_subject);
    step.compensation_subject = "";
    step.timeout = words.timeout;

    provision_tenant_step_command command;
    command.kind = kind;
    command.tenant_id = run.tenant_id;
    command.tenant_code = run.tenant_code;
    command.tenant_hostname = run.tenant_hostname;
    command.admin_account_id = run.admin_account_id;
    command.arguments_json = arguments_json;
    command.parameters = run.parameters;

    step.build_command = [command](const std::string&,
                                   const ores::workflow::service::workflow_step_results&) {
        return rfl::json::write(command);
    };
    step.build_compensation = [](const std::string&, const std::string&) {
        return "{}";
    };
    return step;
}

/**
 * @brief One engine step per kind the run declares, in the order it declares
 * them.
 *
 * A kind the catalogue does not know, and a kind this build does not execute,
 * are both refused by throwing before the engine creates the run, naming the
 * kind in the message. The throw is the guard the engine logs and abandons, so
 * a starting point that orders an unbuilt kind can never half-provision what it
 * was asked for.
 *
 * The completing step a tenant run appends is deliberately not here: the run
 * that provisions a party of an existing tenant has nothing to complete.
 */
[[nodiscard]] inline std::vector<ores::workflow::service::workflow_step_def>
build_declared_steps(const provision_tenant_workflow_request& run,
                     std::unordered_map<std::string, int>& seen) {
    std::vector<ores::workflow::service::workflow_step_def> steps;
    steps.reserve(run.steps.size());

    for (const auto& declared : run.steps) {
        if (declared.kind == complete_provisioning_step_kind)
            throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                     "', which every tenant run appends itself.");
        if (!is_declared_step_kind(declared.kind))
            throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                     "', which this deployment does not know.");
        if (!is_executed_step_kind(declared.kind))
            throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                     "', which this deployment does not execute.");
        steps.push_back(make_step(run, declared.kind, declared.arguments_json, seen));
    }
    return steps;
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
 */
inline void
register_provision_tenant_workflow(ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = std::string(provision_tenant_workflow_type);
    def.on_failure = failure_policy::stop;
    def.steps_depend_on_request = true;
    def.description =
        "Provisions a tenant from a seed profile: publishes the bundles the profile orders, "
        "imports its LEI hierarchy, provisions its parties, loads its staff, attaches its "
        "images, starts its market feeds, and completes the tenant. One engine step per declared "
        "step kind, in the profile's order, plus the step that completes it.";

    def.build_steps = [](const std::string& request_json,
                         const std::string& /*tenant_id*/,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        const auto run = detail::read_workflow_request(request_json);
        std::unordered_map<std::string, int> seen;
        auto steps = detail::build_declared_steps(run, seen);
        steps.push_back(
            detail::make_step(run, std::string(complete_provisioning_step_kind), "{}", seen));
        return steps;
    };

    registry.register_definition(std::move(def));
}

/**
 * @brief Registers the provision_party_workflow definition.
 *
 * The party stage of a tenant that already exists, as its own run: one engine
 * step per kind the request declares, which for a party is the profile's
 * provision_party step with the party it acts on named. Nothing completes the
 * run, because the tenant it belongs to is already active and provisioning one
 * more of its parties neither activates nor finishes it.
 *
 * The tenant run and this one share the subject, the step command and the
 * notation, so a kind means one thing on both sides and the executor that
 * serves one serves the other.
 */
inline void
register_provision_party_workflow(ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = std::string(provision_party_workflow_type);
    def.on_failure = failure_policy::stop;
    def.steps_depend_on_request = true;
    def.description =
        "Provisions one party of an existing tenant: publishes the bundles the starting point "
        "orders against it, activates it, marks its onboarding complete, and associates the "
        "administrator who asked with it. One engine step per declared kind, and no completing "
        "step, because the tenant it belongs to is already active.";

    def.build_steps = [](const std::string& request_json,
                         const std::string& /*tenant_id*/,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        const auto run = detail::read_workflow_request(request_json);
        std::unordered_map<std::string, int> seen;
        return detail::build_declared_steps(run, seen);
    };

    registry.register_definition(std::move(def));
}

}

#endif

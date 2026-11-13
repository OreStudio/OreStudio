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

#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <algorithm>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
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

/// The workflow type the new tenant's own setup run declares in its start
/// message. It is not a request's subject either: the system-side run's last
/// step starts this one, and this run names this type.
inline constexpr std::string_view tenant_setup_workflow_type = "tenant_setup_workflow";

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
 * @brief The subject ores.iam serves the provisioning steps it owns.
 *
 * One subject serves every kind a service owns, because the payload says which
 * kind of work the step is: the engine persists only a step's name and subject,
 * so a subject per kind would put the catalogue in the persisted rows as well.
 * A kind is dispatched to the service that owns the work it does, so the kinds
 * ores.iam executes land here and a kind another service owns is dispatched
 * there instead, as provision_step_owners states.
 */
inline constexpr std::string_view provision_tenant_step_subject =
    "iam.v1.ops.provision_tenant_step";

/**
 * @brief The subject ores.refdata serves the provisioning steps it owns.
 *
 * ores.refdata owns the calendars, so the step that materialises them runs
 * there and reaches the store directly rather than through a request to itself.
 */
inline constexpr std::string_view refdata_provision_tenant_step_subject =
    "refdata.v1.ops.provision_tenant_step";

/// The step kind that completes the tenant, appended to every run after the
/// kinds the profile declares. A run finishes only by running its steps out, so
/// "the tenant is ready" is a step rather than a property of the definition.
inline constexpr std::string_view complete_provisioning_step_kind = "complete_provisioning";

/// The step kind that hands the tenant to its own administrator, appended to
/// the system-side run after the kinds the profile declares. Its work is to
/// start the tenant's own setup run, which finishes the tenant. A profile
/// never orders it: where the two runs are joined is the notation's statement
/// and not a starting point's, so a profile that names it is refused rather
/// than given a second handover.
inline constexpr std::string_view start_tenant_setup_step_kind = "start_tenant_setup";

/// The step kinds the notation knows, each named once so a handler that reads
/// the catalogue by kind names the same string the catalogue declares.
inline constexpr std::string_view system_provision_step_kind = "system_provision";
inline constexpr std::string_view publish_system_curves_step_kind = "publish_system_curves";
inline constexpr std::string_view publish_bundle_step_kind = "publish_bundle";
inline constexpr std::string_view build_calendars_step_kind = "build_calendars";
inline constexpr std::string_view start_auto_approval_step_kind = "start_auto_approval";
inline constexpr std::string_view end_auto_approval_step_kind = "end_auto_approval";
inline constexpr std::string_view import_lei_hierarchy_step_kind = "import_lei_hierarchy";
inline constexpr std::string_view provision_party_step_kind = "provision_party";
inline constexpr std::string_view load_staff_step_kind = "load_staff";
inline constexpr std::string_view set_staff_passwords_step_kind = "set_staff_passwords";
inline constexpr std::string_view attach_photos_step_kind = "attach_photos";
inline constexpr std::string_view subscribe_market_data_step_kind = "subscribe_market_data";
inline constexpr std::string_view publish_party_curves_step_kind = "publish_party_curves";
inline constexpr std::string_view give_party_surface_blueprints_step_kind =
    "give_party_surface_blueprints";
inline constexpr std::string_view publish_party_forwards_step_kind = "publish_party_forwards";
inline constexpr std::string_view publish_party_surfaces_step_kind = "publish_party_surfaces";
inline constexpr std::string_view generate_trades_step_kind = "generate_trades";
inline constexpr std::string_view schedule_reports_step_kind = "schedule_reports";

/// The step kinds the notation knows. A kind that is not here is a code change:
/// the catalogue is fixed and a profile states only which kinds it orders and
/// with what arguments. The completing step is deliberately absent, because
/// every run appends it; a profile that orders it is refused rather than given
/// a second one. Not every kind here is one this build executes; see
/// @ref provision_executed_step_kinds for the subset a profile may order today.
inline constexpr std::string_view provision_step_kinds[] = {system_provision_step_kind,
                                                            publish_system_curves_step_kind,
                                                            publish_bundle_step_kind,
                                                            build_calendars_step_kind,
                                                            start_auto_approval_step_kind,
                                                            end_auto_approval_step_kind,
                                                            import_lei_hierarchy_step_kind,
                                                            provision_party_step_kind,
                                                            load_staff_step_kind,
                                                            set_staff_passwords_step_kind,
                                                            attach_photos_step_kind,
                                                            subscribe_market_data_step_kind,
                                                            publish_party_curves_step_kind,
                                                            give_party_surface_blueprints_step_kind,
                                                            publish_party_forwards_step_kind,
                                                            publish_party_surfaces_step_kind,
                                                            generate_trades_step_kind,
                                                            schedule_reports_step_kind};

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
 * @brief Which service serves the step of one kind.
 *
 * A step runs in the service that owns the work it does: that service holds the
 * code, reaches the data and pays for both, and ores.iam carries only the kinds
 * whose work is identity.
 */
struct step_owner {
    std::string_view kind;
    std::string_view subject;
};

/**
 * @brief The kinds that run outside ores.iam, and the service that runs each.
 *
 * A kind that is not in this table runs in ores.iam, which serves the run and
 * the identity work every step of it needs. The table is written here because
 * the definition states a step's subject when it builds the step, and the
 * engine persists that subject with the row. A kind the table names is
 * dispatched to a service that serves it; a kind no service serves is refused
 * where a person can read the refusal.
 */
inline constexpr step_owner provision_step_owners[] = {
    {build_calendars_step_kind, refdata_provision_tenant_step_subject},
};

/**
 * @brief The subject the step of @p kind is dispatched to.
 *
 * A kind no entry names is dispatched to ores.iam, which refuses what it has no
 * executor for, so a definition naming a kind no service serves fails loudly
 * instead of waiting for nobody.
 */
[[nodiscard]] inline std::string_view step_subject_for_kind(std::string_view kind) {
    for (const auto& owner : provision_step_owners)
        if (kind == owner.kind)
            return owner.subject;
    return provision_tenant_step_subject;
}

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
    if (kind == publish_system_curves_step_kind)
        return {"Publish the system's curves",
                "Bootstraps each IR curve the system tenant holds a config for from its quotes "
                "and publishes it, so forwards can be read at once. A curve whose quotes have not "
                "arrived in time is named with its reason, and the run goes on.",
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
    if (kind == set_staff_passwords_step_kind)
        return {"Set the staff passwords",
                "Sets each staff account's password through the server's own password API, so "
                "the demonstration tenant's people can sign in.",
                orchestrating_step_timeout};
    if (kind == start_auto_approval_step_kind)
        return {"Turn auto-approval on",
                "Closes the tenant's approvals without a person while it is provisioned. Every "
                "screen says so while it is on.",
                write_step_timeout};
    if (kind == end_auto_approval_step_kind)
        return {"Hand approvals to people",
                "Turns auto-approval off, so people decide from here on. A profile that omits "
                "this step leaves auto-approval on.",
                write_step_timeout};
    if (kind == build_calendars_step_kind)
        return {"Build the holiday calendars",
                "Builds the business days of every calendar a currency or a currency pair "
                "convention names, so a value date never falls on a holiday.",
                orchestrating_step_timeout};
    if (kind == attach_photos_step_kind)
        return {"Attach the photographs",
                "Gives the accounts their photographs, and the tenant's party its logo.",
                write_step_timeout};
    if (kind == subscribe_market_data_step_kind)
        return {"Subscribe to market data",
                "Gives each party of the tenant a subscription to a provider and a route to the "
                "provider's source.",
                orchestrating_step_timeout};
    if (kind == publish_party_curves_step_kind)
        return {"Publish each party's curves",
                "Gives each party it names its own copy of the system's curve blueprints, "
                "bootstraps each curve from the quotes the party's subscriptions deliver, and "
                "publishes it. A curve that does not publish is named with its party and its "
                "reason, and the step fails.",
                orchestrating_step_timeout};
    if (kind == give_party_surface_blueprints_step_kind)
        return {"Give each party its surface blueprints",
                "Gives each party it names its own copy of the system's FX volatility surface "
                "blueprints, reading the volatility quotes the party's subscriptions deliver. A "
                "blueprint whose quotes do not arrive in time is named with its party, and the "
                "step fails.",
                orchestrating_step_timeout};
    if (kind == publish_party_forwards_step_kind)
        return {"Publish each party's FX forwards",
                "Publishes a forward set for every currency pair convention each party it names "
                "can build from its spot and its published funding curves. A pair the party "
                "cannot build is named with the reason. A forward that does not publish is named "
                "with its party, and the step fails.",
                orchestrating_step_timeout};
    if (kind == publish_party_surfaces_step_kind)
        return {"Publish each party's volatility surfaces",
                "Calibrates each surface blueprint of each party it names from the party's "
                "volatility quotes and published forward sets, and publishes it. A surface that "
                "fails its arbitrage or error checks is named and left unpublished. A surface "
                "that cannot be calibrated or does not publish is named with its party, and the "
                "step fails.",
                orchestrating_step_timeout};
    if (kind == generate_trades_step_kind)
        return {"Generate the trades",
                "Records a trade generation config for each party the starting point names and "
                "writes the population it states into that party's own books.",
                orchestrating_step_timeout};
    if (kind == schedule_reports_step_kind)
        return {"Schedule the reports",
                "Schedules every report definition each party it names publishes, as the run's "
                "administrator, so the catalogue ticks without a person acting. A report whose "
                "schedule is the on-demand placeholder is left to be asked for by hand.",
                orchestrating_step_timeout};
    if (kind == start_tenant_setup_step_kind)
        return {"Hand the tenant to its administrator",
                "Starts the tenant's own setup run, which runs as the tenant "
                "administrator and finishes the tenant.",
                write_step_timeout};
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

/// The declarable kinds this build executes somewhere, in ores.iam or in the
/// service that owns the work. The catalogue above states the notation; this
/// list states which of its kinds a profile may order, so a profile that orders
/// the rest is refused before its run starts rather than half-provisioned.
/// Growing it is a code change beside the executor and, for a kind another
/// service owns, a row in provision_step_owners.
inline constexpr std::string_view provision_executed_step_kinds[] = {
    system_provision_step_kind,
    publish_system_curves_step_kind,
    publish_bundle_step_kind,
    build_calendars_step_kind,
    start_auto_approval_step_kind,
    end_auto_approval_step_kind,
    import_lei_hierarchy_step_kind,
    provision_party_step_kind,
    load_staff_step_kind,
    set_staff_passwords_step_kind,
    attach_photos_step_kind,
    subscribe_market_data_step_kind,
    publish_party_curves_step_kind,
    give_party_surface_blueprints_step_kind,
    publish_party_forwards_step_kind,
    publish_party_surfaces_step_kind,
    generate_trades_step_kind,
    schedule_reports_step_kind};

/// Whether this build executes the kind. A kind the catalogue does not know is
/// not executed either, so one predicate answers both refusals.
[[nodiscard]] inline bool is_executed_step_kind(std::string_view kind) {
    return std::any_of(std::begin(provision_executed_step_kinds),
                       std::end(provision_executed_step_kinds),
                       [kind](std::string_view known) { return known == kind; });
}

/**
 * @brief Whether a step kind acts in the system tenant rather than the tenant
 * being provisioned.
 *
 * The split between the two runs is this predicate. The provisioning of one
 * tenant is two runs joined at the authentication boundary: the caller owns
 * the run whose kinds act in the system tenant, and the new tenant owns the
 * run whose kinds act inside it. @c system_provision publishes the
 * installation's own data, which belongs to the system tenant and exists before
 * the new tenant holds anything, and @c publish_system_curves publishes the
 * system tenant's curves from the quotes that data's feeds start. A kind added
 * to the catalogue has to be classified here, so that a future kind cannot
 * land in a run by accident.
 */
[[nodiscard]] inline bool is_system_scoped_step_kind(std::string_view kind) {
    return kind == system_provision_step_kind || kind == publish_system_curves_step_kind;
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
 * @brief The members of provision_tenant_step, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_tenant_step>) {
    using ::ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"kind", &provision_tenant_step::kind},
        field{"arguments_json", &provision_tenant_step::arguments_json}};
}

static_assert(::ores::utility::codec::names_every_member<provision_tenant_step>(),
              "provision_tenant_step gained a member its wire_fields does not name.");

/**
 * @brief One parameter value the run's steps read, by declared name.
 */
struct provision_tenant_parameter {
    std::string name;
    std::string value;
};

/**
 * @brief The members of provision_tenant_parameter, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_tenant_parameter>) {
    using ::ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"name", &provision_tenant_parameter::name},
                                              field{"value", &provision_tenant_parameter::value}};
}

static_assert(::ores::utility::codec::names_every_member<provision_tenant_parameter>(),
              "provision_tenant_parameter gained a member its wire_fields does not name.");

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
 * @brief The members of provision_tenant_workflow_request, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_tenant_workflow_request>) {
    using ::ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"profile_code", &provision_tenant_workflow_request::profile_code},
        field{"tenant_id", &provision_tenant_workflow_request::tenant_id},
        field{"tenant_code", &provision_tenant_workflow_request::tenant_code},
        field{"tenant_hostname", &provision_tenant_workflow_request::tenant_hostname},
        field{"admin_account_id", &provision_tenant_workflow_request::admin_account_id},
        field{"parameters", &provision_tenant_workflow_request::parameters},
        field{"steps", &provision_tenant_workflow_request::steps}};
}

static_assert(::ores::utility::codec::names_every_member<provision_tenant_workflow_request>(),
              "provision_tenant_workflow_request gained a member its wire_fields does not name.");

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

/**
 * @brief The members of provision_tenant_step_command, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_tenant_step_command>) {
    using ::ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"kind", &provision_tenant_step_command::kind},
        field{"tenant_id", &provision_tenant_step_command::tenant_id},
        field{"tenant_code", &provision_tenant_step_command::tenant_code},
        field{"tenant_hostname", &provision_tenant_step_command::tenant_hostname},
        field{"admin_account_id", &provision_tenant_step_command::admin_account_id},
        field{"arguments_json", &provision_tenant_step_command::arguments_json},
        field{"parameters", &provision_tenant_step_command::parameters}};
}

static_assert(::ores::utility::codec::names_every_member<provision_tenant_step_command>(),
              "provision_tenant_step_command gained a member its wire_fields does not name.");

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
    auto parsed =
        ores::utility::codec::json::read_expected<provision_tenant_workflow_request>(request_json);
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
    step.command_subject = std::string(step_subject_for_kind(kind));
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
        return ores::utility::codec::json::write(command);
    };
    step.build_compensation = [](const std::string&, const std::string&, const std::string&) {
        return "{}";
    };
    return step;
}

/**
 * @brief The kinds a kind cannot run before, nearest first.
 *
 * The chain's shape, stated once, for the same reason the words and the budgets
 * are: what a kind needs is knowledge about the kind. Until this existed the
 * order the profile declared was the only statement of it, and an order cannot
 * say that two steps are independent -- so a bootstrapping chain ran one step at
 * a time whether or not its work allowed it, and a run could not say what it was
 * waiting for.
 */
[[nodiscard]] inline std::vector<std::string_view> needs_of_step_kind(std::string_view kind) {
    if (kind == publish_bundle_step_kind)
        return {system_provision_step_kind};
    // The curves read the quotes of the feeds the system data starts, and the
    // configs that data's bootstrap seed holds.
    if (kind == publish_system_curves_step_kind)
        return {system_provision_step_kind};
    if (kind == import_lei_hierarchy_step_kind)
        return {publish_bundle_step_kind};
    // The calendars, their exceptions and the links that say which are in use
    // all arrive with the base bundle.
    if (kind == build_calendars_step_kind)
        return {publish_bundle_step_kind};
    if (kind == provision_party_step_kind)
        return {import_lei_hierarchy_step_kind};
    if (kind == load_staff_step_kind)
        return {provision_party_step_kind};
    if (kind == set_staff_passwords_step_kind)
        return {load_staff_step_kind};
    if (kind == attach_photos_step_kind)
        return {load_staff_step_kind, provision_party_step_kind};
    // The step subscribes every party the tenant holds, so the parties the import
    // creates must exist first; a party created after the step is never subscribed.
    if (kind == subscribe_market_data_step_kind)
        return {provision_party_step_kind, publish_bundle_step_kind};
    // A trade names a book, a counterparty and the person who entered it, so
    // the books and the staff must exist first, and it is booked against the
    // market data the parties already subscribe to.
    if (kind == generate_trades_step_kind)
        return {load_staff_step_kind, subscribe_market_data_step_kind};
    // A party's curves read the quotes its subscriptions deliver into its own series.
    if (kind == publish_party_curves_step_kind)
        return {subscribe_market_data_step_kind};
    // A party's surface blueprints read the volatility quotes its subscriptions deliver.
    if (kind == give_party_surface_blueprints_step_kind)
        return {subscribe_market_data_step_kind};
    // A forward set reads the party's spot and the funding curves it has published. It lays out
    // its ladder on the business days of its pair's calendars, so the calendars must be built:
    // a calendar still being materialised reads as a calendar with no dates, and the pair is
    // refused for a reason that disappears a moment later.
    if (kind == publish_party_forwards_step_kind)
        return {publish_party_curves_step_kind, build_calendars_step_kind};
    // A surface reads the party's own blueprints and the forward sets it has published.
    if (kind == publish_party_surfaces_step_kind)
        return {publish_party_forwards_step_kind, give_party_surface_blueprints_step_kind};
    // The report definitions a party runs are published by the bundles the
    // staff step names, and a schedule names the party whose consent its runs
    // act under, so the parties and their published definitions exist first.
    if (kind == schedule_reports_step_kind)
        return {load_staff_step_kind};
    if (kind == start_tenant_setup_step_kind)
        return {system_provision_step_kind};
    return {};
}

/**
 * @brief The step that stands in for a kind this run did not order.
 *
 * A profile need not order every kind: the run that provisions a party of an
 * existing tenant orders neither the bundle step nor the import, and that step's
 * need for parties is already met by whatever provisioned the tenant. So a need
 * the run did not order is answered by the nearest step it did, found by walking
 * back through the absent kind's own needs. A need the run ordered is answered
 * by itself, which is the common case.
 */
[[nodiscard]] inline const std::string*
nearest_producer(std::string_view kind,
                 const std::unordered_map<std::string, std::string>& present,
                 int depth = 0) {
    // The catalogue is a fixed, shallow chain, so a walk this long means the
    // table above has grown a cycle rather than that the run is unusual.
    if (depth > 8)
        return nullptr;
    if (const auto found = present.find(std::string(kind)); found != present.end())
        return &found->second;
    for (const auto& need : needs_of_step_kind(kind))
        if (const auto* producer = nearest_producer(need, present, depth + 1))
            return producer;
    return nullptr;
}

/**
 * @brief States, for each step, the steps it must wait for.
 *
 * A step reads the nearest step this run declared for each kind it needs. Only
 * steps already declared are in reach, because the engine advances along the
 * chain's declaration order and refuses a consumer of a step it reaches later.
 * The declaration order is therefore a topological one, and it stays that way
 * without the engine having to check the profile against the catalogue above.
 */
inline void declare_inputs(std::vector<ores::workflow::service::workflow_step_def>& steps,
                           const std::vector<std::string>& kinds) {
    std::unordered_map<std::string, std::string> present;
    for (std::size_t i = 0; i < steps.size(); ++i) {
        for (const auto& need : needs_of_step_kind(kinds[i])) {
            const auto* producer = nearest_producer(need, present);
            if (producer == nullptr)
                continue;
            // Two needs may resolve to one step -- a step that is the nearest
            // present producer of two kinds -- and a step reads it once.
            if (std::find(steps[i].consumes.begin(), steps[i].consumes.end(), *producer) ==
                steps[i].consumes.end())
                steps[i].consumes.push_back(*producer);
        }
        present.insert_or_assign(kinds[i], steps[i].name);
    }

    // A step with no input starts at once, so auto-approval has to bracket the
    // run explicitly: on before any other step, and off after all of them.
    const auto reads = [](auto& step, const std::string& name) {
        if (std::find(step.consumes.begin(), step.consumes.end(), name) == step.consumes.end())
            step.consumes.push_back(name);
    };
    const auto start = present.find(std::string(start_auto_approval_step_kind));
    for (std::size_t i = 0; i < steps.size(); ++i) {
        if (kinds[i] == end_auto_approval_step_kind) {
            for (std::size_t earlier = 0; earlier < i; ++earlier)
                reads(steps[i], steps[earlier].name);
            for (std::size_t later = i + 1; later < steps.size(); ++later)
                reads(steps[later], steps[i].name);
        } else if (start != present.end() && kinds[i] != start_auto_approval_step_kind) {
            reads(steps[i], start->second);
        }
    }
}

/**
 * @brief One engine step per kind one half of a profile declares, in the order
 * it declares them, followed by the kind that run appends.
 *
 * The provisioning of one tenant is two runs, and this builds one of them.
 * @p system_scoped chooses the half: true builds the kinds that act in the
 * system tenant, false the kinds that act in the new tenant. Every declared
 * kind is checked before the split, so a run refuses a profile that orders an
 * unbuilt kind whichever half it would have landed in, and a kind one run does
 * not take is a kind the other run takes.
 *
 * A kind the catalogue does not know, and a kind this build does not execute,
 * are both refused by throwing before the engine creates the run, naming the
 * kind in the message. The throw is the guard the engine logs and abandons, so
 * a starting point that orders an unbuilt kind can never half-provision what it
 * was asked for.
 *
 * The kinds a run appends are deliberately not here: complete_provisioning
 * finishes the tenant and start_tenant_setup hands it to its own
 * administrator, so a profile that orders either is refused. @p appended_kind
 * is the one this run adds instead, empty for a run that appends none and
 * composes its own final step.
 */
[[nodiscard]] inline std::vector<ores::workflow::service::workflow_step_def>
build_declared_steps(const provision_tenant_workflow_request& run,
                     bool system_scoped,
                     const std::string& appended_kind,
                     const std::string& appended_arguments_json,
                     std::unordered_map<std::string, int>& seen) {
    std::vector<ores::workflow::service::workflow_step_def> steps;
    std::vector<std::string> kinds;
    steps.reserve(run.steps.size() + 1);
    kinds.reserve(run.steps.size() + 1);

    for (const auto& declared : run.steps) {
        if (declared.kind == complete_provisioning_step_kind)
            throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                     "', which every tenant run appends itself.");
        if (declared.kind == start_tenant_setup_step_kind)
            throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                     "', which the provisioning run appends itself.");
        if (!is_declared_step_kind(declared.kind))
            throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                     "', which this deployment does not know.");
        if (!is_executed_step_kind(declared.kind))
            throw std::runtime_error("The profile orders the step kind '" + declared.kind +
                                     "', which this deployment does not execute.");
        if (is_system_scoped_step_kind(declared.kind) != system_scoped)
            continue;
        steps.push_back(make_step(run, declared.kind, declared.arguments_json, seen));
        kinds.push_back(declared.kind);
    }

    if (!appended_kind.empty()) {
        steps.push_back(make_step(run, appended_kind, appended_arguments_json, seen));
        kinds.push_back(appended_kind);
    }

    declare_inputs(steps, kinds);
    return steps;
}

} // namespace detail

/**
 * @brief Registers the provision_tenant_workflow definition.
 *
 * The system side of a tenant's provisioning, as its own run: one engine step
 * per system-scoped kind the request declares, in the order the profile
 * declared them, plus the step that hands the tenant to its own administrator
 * and starts the run that finishes it. Every step is dispatched to
 * @ref provision_tenant_step_subject, which ores.iam serves: the engine sends a
 * step and waits for the handler to report it, so a step's kind must have an
 * executor there.
 */
inline void
register_provision_tenant_workflow(ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = std::string(provision_tenant_workflow_type);
    def.on_failure = failure_policy::stop;
    def.steps_depend_on_request = true;
    def.description =
        "Publishes the installation's own data into the system tenant and hands the tenant to "
        "its own administrator, who runs its setup. One engine step per system-scoped kind the "
        "profile orders, plus the step that starts the tenant's own run.";

    def.build_steps = [](const std::string& request_json,
                         const std::string& /*tenant_id*/,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        const auto run = detail::read_workflow_request(request_json);
        std::unordered_map<std::string, int> seen;
        return detail::build_declared_steps(run,
                                            /*system_scoped=*/true,
                                            std::string(start_tenant_setup_step_kind),
                                            request_json,
                                            seen);
    };

    registry.register_definition(std::move(def));
}

/**
 * @brief Registers the tenant_setup_workflow definition.
 *
 * The new tenant's own setup, as its own run: one engine step per kind the
 * request declares that is not system-scoped, in the order the profile declared
 * them, plus the step that completes the tenant. The run is owned by the new
 * tenant and its steps act as that tenant's administrator, which the system
 * side created. Its request is the same one the system-side run received: one
 * run hands its own request to the next rather than composing another.
 *
 * The tenant run and this one share the subject, the step command and the
 * notation, so a kind means one thing on both sides and the executor that
 * serves one serves the other.
 */
inline void register_tenant_setup_workflow(ores::workflow::service::workflow_registry& registry) {

    using namespace ores::workflow::service;

    workflow_definition def;
    def.type_name = std::string(tenant_setup_workflow_type);
    def.on_failure = failure_policy::stop;
    def.steps_depend_on_request = true;
    def.description =
        "Finishes a tenant from the seed profile the system-side run received: publishes the "
        "bundles the profile orders, imports its LEI hierarchy, provisions its parties, loads "
        "its staff, sets their passwords, attaches its images, starts its market feeds, generates "
        "its trades, schedules the reports its parties publish and completes the tenant. One "
        "engine step per declared kind that acts inside the tenant, in the profile's order, plus "
        "the step that completes it.";

    def.build_steps = [](const std::string& request_json,
                         const std::string& /*tenant_id*/,
                         const std::string& /*correlation_id*/) -> std::vector<workflow_step_def> {
        const auto run = detail::read_workflow_request(request_json);
        std::unordered_map<std::string, int> seen;
        auto steps = detail::build_declared_steps(
            run, /*system_scoped=*/false, std::string{}, std::string{}, seen);
        auto finish =
            detail::make_step(run, std::string(complete_provisioning_step_kind), "{}", seen);
        // Nothing is finished before everything is: the finishing step marks the
        // tenant active, so it reads every step the run declared rather than
        // only the last. A chain of six kinds therefore ends in a fan-in of
        // six, which is also what lets the middle of the chain fan out.
        finish.consumes.reserve(steps.size());
        for (const auto& declared : steps)
            finish.consumes.push_back(declared.name);
        steps.push_back(std::move(finish));
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
        return detail::build_declared_steps(
            run, /*system_scoped=*/false, std::string{}, std::string{}, seen);
    };

    registry.register_definition(std::move(def));
}

}

#endif

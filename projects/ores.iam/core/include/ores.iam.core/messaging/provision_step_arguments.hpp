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
#ifndef ORES_IAM_MESSAGING_PROVISION_STEP_ARGUMENTS_HPP
#define ORES_IAM_MESSAGING_PROVISION_STEP_ARGUMENTS_HPP

#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace ores::iam::messaging {

/// What the step executor does with a step command's kind.
enum class provision_step_action {
    system_provision,
    publish_bundle,
    import_lei_hierarchy,
    provision_party,
    load_staff,
    attach_photos,
    start_market_feeds,
    complete_provisioning,
    refuse
};

/**
 * @brief Classifies a step command's kind into the action the executor takes.
 *
 * The catalogue and the executed predicate both live in the workflow
 * definition, so this function only maps them onto the executor's actions: a
 * kind this build does not execute is refused, and the completing step the
 * definition appends is recognised even though no profile may order it.
 */
[[nodiscard]] inline provision_step_action classify_step_kind(std::string_view kind) {
    using namespace ores::iam::workflow;

    if (kind == complete_provisioning_step_kind)
        return provision_step_action::complete_provisioning;
    if (!is_executed_step_kind(kind))
        return provision_step_action::refuse;
    if (kind == system_provision_step_kind)
        return provision_step_action::system_provision;
    if (kind == "publish_bundle")
        return provision_step_action::publish_bundle;
    if (kind == "import_lei_hierarchy")
        return provision_step_action::import_lei_hierarchy;
    if (kind == "provision_party")
        return provision_step_action::provision_party;
    if (kind == "load_staff")
        return provision_step_action::load_staff;
    if (kind == "attach_photos")
        return provision_step_action::attach_photos;
    if (kind == "start_market_feeds")
        return provision_step_action::start_market_feeds;
    return provision_step_action::refuse;
}

namespace detail {

[[nodiscard]] inline rfl::Object<rfl::Generic>
read_step_arguments(const std::string& arguments_json) {
    if (arguments_json.empty())
        return {};

    auto parsed = rfl::json::read<rfl::Generic>(arguments_json);
    if (!parsed)
        throw std::runtime_error("The step's arguments are not JSON.");

    const auto* object = std::get_if<rfl::Object<rfl::Generic>>(&parsed->get());
    if (object == nullptr)
        throw std::runtime_error("The step's arguments are not a JSON object.");
    return *object;
}

[[nodiscard]] inline std::vector<std::string> read_string_list(const rfl::Generic& value,
                                                               std::string_view field) {
    const auto* list = std::get_if<std::vector<rfl::Generic>>(&value.get());
    if (list == nullptr)
        throw std::runtime_error("The step's argument '" + std::string(field) +
                                 "' is not a list of strings.");

    std::vector<std::string> result;
    result.reserve(list->size());
    for (const auto& element : *list) {
        const auto* text = std::get_if<std::string>(&element.get());
        if (text == nullptr)
            throw std::runtime_error("The step's argument '" + std::string(field) +
                                     "' is not a list of strings.");
        result.push_back(*text);
    }
    return result;
}

[[nodiscard]] inline const rfl::Generic* member(const rfl::Object<rfl::Generic>& arguments,
                                                std::string_view field) {
    if (arguments.count(std::string(field)) == 0)
        return nullptr;
    return &arguments.at(std::string(field));
}

[[nodiscard]] inline std::string read_string(const rfl::Object<rfl::Generic>& arguments,
                                             std::string_view field) {
    const auto* value = member(arguments, field);
    if (value == nullptr)
        return {};

    const auto* text = std::get_if<std::string>(&value->get());
    if (text == nullptr)
        throw std::runtime_error("The step's argument '" + std::string(field) +
                                 "' is not a string.");
    return *text;
}

[[nodiscard]] inline bool read_bool(const rfl::Object<rfl::Generic>& arguments,
                                    std::string_view field) {
    const auto* value = member(arguments, field);
    if (value == nullptr)
        return false;

    const auto* flag = std::get_if<bool>(&value->get());
    if (flag == nullptr)
        throw std::runtime_error("The step's argument '" + std::string(field) +
                                 "' is not a boolean.");
    return *flag;
}

[[nodiscard]] inline std::vector<rfl::Object<rfl::Generic>>
read_object_list(const rfl::Generic& value, std::string_view field) {
    const auto* list = std::get_if<std::vector<rfl::Generic>>(&value.get());
    if (list == nullptr)
        throw std::runtime_error("The step's argument '" + std::string(field) +
                                 "' is not a list of objects.");

    std::vector<rfl::Object<rfl::Generic>> result;
    result.reserve(list->size());
    for (const auto& element : *list) {
        const auto* object = std::get_if<rfl::Object<rfl::Generic>>(&element.get());
        if (object == nullptr)
            throw std::runtime_error("The step's argument '" + std::string(field) +
                                     "' is not a list of objects.");
        result.push_back(*object);
    }
    return result;
}

/// A string a kind cannot act without. The context names what carries the field,
/// so the message says which entry of a list has to be fixed.
[[nodiscard]] inline std::string read_required_string(const rfl::Object<rfl::Generic>& arguments,
                                                      std::string_view field,
                                                      std::string_view context) {
    auto value = read_string(arguments, field);
    if (value.empty())
        throw std::runtime_error("The step's " + std::string(context) + " names no '" +
                                 std::string(field) + "' argument.");
    return value;
}

/// The bundle codes a step's arguments name. A step that names no bundle is a
/// profile row that asked for no data, so the field is required rather than
/// treated as a no-op.
[[nodiscard]] inline std::vector<std::string>
read_bundles(const rfl::Object<rfl::Generic>& arguments) {
    const auto* found = member(arguments, "bundles");
    if (found == nullptr)
        throw std::runtime_error("The step names no 'bundles' argument.");

    auto bundles = read_string_list(*found, "bundles");
    if (bundles.empty())
        throw std::runtime_error("The step names no bundle in its 'bundles' argument.");
    return bundles;
}

[[nodiscard]] inline std::string
parameter_value(const std::vector<ores::iam::workflow::provision_tenant_parameter>& parameters,
                std::string_view name) {
    for (const auto& parameter : parameters)
        if (parameter.name == name)
            return parameter.value;
    return {};
}

} // namespace detail

/**
 * @brief The bundle codes a step publishes, in the order it names them.
 *
 * publish_bundle and provision_party share this shape: a 'bundles' list the
 * profile states. A step that names no bundle is refused rather than treated as
 * a no-op, because a profile row that asked for no data is a mistake.
 */
[[nodiscard]] inline std::vector<std::string>
parse_step_bundles(const std::string& arguments_json) {
    return detail::read_bundles(detail::read_step_arguments(arguments_json));
}

/**
 * @brief Puts a run's parameter values in place of every @c {name} a text
 * names.
 *
 * A profile's row states the data it wants, and part of that data is a value a
 * person typed: the counterparty set a starting point publishes is the value
 * of its @c counterparty_size parameter, so the row names the parameter rather
 * than one of its values. This is the same reading an LEI import does when its
 * row leaves the root LEI out, written so a code inside a longer dataset code
 * can be built from it.
 *
 * A reference the run supplies no value for is refused rather than left in
 * place: a step that published without the member its row named would leave the
 * tenant with half the data the person asked for.
 */
[[nodiscard]] inline std::string resolve_parameter_references(
    std::string text,
    const std::vector<ores::iam::workflow::provision_tenant_parameter>& parameters) {
    auto opening = text.find('{');
    while (opening != std::string::npos) {
        const auto closing = text.find('}', opening);
        if (closing == std::string::npos)
            throw std::runtime_error("The step's argument '" + text +
                                     "' names a parameter without closing its braces.");

        const auto name = text.substr(opening + 1, closing - opening - 1);
        const auto value = detail::parameter_value(parameters, name);
        if (value.empty())
            throw std::runtime_error("The step's argument '" + text + "' names the parameter '" +
                                     name + "', which the run supplies no value for.");

        text.replace(opening, closing - opening + 1, value);
        opening = text.find('{', opening + value.size());
    }
    return text;
}

/// The bundles a publish_bundle step publishes, and the optional members of
/// them its profile opts in.
struct publish_bundle_step_arguments {
    std::vector<std::string> bundles;
    std::vector<std::string> opted_in_datasets;
};

/**
 * @brief Reads a publish_bundle step's arguments.
 *
 * The bundles are the profile's data. The opted-in datasets are the members of
 * those bundles that a bundle publishes only when a profile asks for them, so a
 * profile that names none publishes the bundles whole and one that names a
 * member has it published beside the rest. A code may name a run parameter in
 * braces; see @ref resolve_parameter_references.
 */
[[nodiscard]] inline publish_bundle_step_arguments parse_publish_bundle_arguments(
    const std::string& arguments_json,
    const std::vector<ores::iam::workflow::provision_tenant_parameter>& parameters) {
    publish_bundle_step_arguments result;
    result.bundles = parse_step_bundles(arguments_json);

    const auto arguments = detail::read_step_arguments(arguments_json);
    if (const auto* found = detail::member(arguments, "opted_in_datasets"))
        result.opted_in_datasets = detail::read_string_list(*found, "opted_in_datasets");

    for (auto& dataset : result.opted_in_datasets)
        dataset = resolve_parameter_references(dataset, parameters);
    return result;
}

/// The bundles a provision_party step publishes, and the one party it acts on.
struct provision_party_step_arguments {
    std::vector<std::string> bundles;
    /// The party to provision, named by its identifier or its exact full name.
    /// Empty means every party the tenant holds, which is what a tenant's own
    /// run asks for: the parties its hierarchy import created.
    std::string party;
    /**
     * The legal entity the party was built from, when it was built from one the
     * deployment holds.
     *
     * Empty for a party a person described by hand, which is a party the
     * register has no LEI for; a run that records none is the same run as one
     * whose party is not in the register.
     */
    std::string lei;
};

/**
 * @brief Reads a provision_party step's arguments.
 *
 * A profile names the bundles. A row that names no party provisions every party
 * the tenant holds, which is the tenant run's reading; a row that names one, or
 * a request that names one, provisions that party alone. The LEI is the
 * request's and never the profile's: the entity is chosen by the person who
 * adds the party, not by the starting point it is added from.
 */
[[nodiscard]] inline provision_party_step_arguments
parse_provision_party_arguments(const std::string& arguments_json) {
    provision_party_step_arguments result;
    result.bundles = parse_step_bundles(arguments_json);

    const auto arguments = detail::read_step_arguments(arguments_json);
    result.party = detail::read_string(arguments, "party");
    result.lei = detail::read_string(arguments, "lei");
    return result;
}

/// The bundles an import_lei_hierarchy step publishes and the root LEI it
/// imports under.
struct lei_hierarchy_step_arguments {
    std::vector<std::string> bundles;
    std::string root_lei;
};

/**
 * @brief Reads an import_lei_hierarchy step's arguments.
 *
 * The profile names the bundle that carries the hierarchy. The root LEI may
 * come from the step's arguments or, when the row states none, from the run's
 * parameters, which is how the form supplies it. A run that supplies neither
 * imports nothing rather than being refused: a starting point may leave the
 * legal entity out, and its tenant is then built from the party it creates for
 * itself. The step says it had nothing to import, which the run reports as a
 * warning, because a run that silently did nothing where it promised work is
 * worse than one that says so.
 */
[[nodiscard]] inline lei_hierarchy_step_arguments parse_lei_hierarchy_arguments(
    const std::string& arguments_json,
    const std::vector<ores::iam::workflow::provision_tenant_parameter>& parameters) {
    lei_hierarchy_step_arguments result;
    result.bundles = parse_step_bundles(arguments_json);

    const auto arguments = detail::read_step_arguments(arguments_json);
    result.root_lei = detail::read_string(arguments, "root_lei");
    if (result.root_lei.empty())
        result.root_lei = detail::parameter_value(parameters, "root_lei");

    return result;
}

/// One party a kind acts on, named by its legal name, with the bundles the kind
/// publishes against it and whether it becomes the administrator's default.
struct staff_party_assignment {
    std::string party_name;
    std::vector<std::string> bundles;
    bool is_default = false;
};

/**
 * @brief Reads a load_staff step's arguments.
 *
 * The profile names each party by the legal name the LEI import created it
 * with, so the kind reaches the party through the party read rather than
 * through an office code of its own. Each entry carries the bundles that load
 * that party's staff and organisation.
 */
[[nodiscard]] inline std::vector<staff_party_assignment>
parse_staff_assignments(const std::string& arguments_json) {
    const auto arguments = detail::read_step_arguments(arguments_json);
    const auto* found = detail::member(arguments, "parties");
    if (found == nullptr)
        throw std::runtime_error("The step names no 'parties' argument.");

    auto entries = detail::read_object_list(*found, "parties");
    if (entries.empty())
        throw std::runtime_error("The step names no party in its 'parties' argument.");

    std::vector<staff_party_assignment> result;
    result.reserve(entries.size());
    for (const auto& entry : entries) {
        staff_party_assignment assignment;
        assignment.party_name = detail::read_required_string(entry, "name", "party entry");
        assignment.bundles = detail::read_bundles(entry);
        assignment.is_default = detail::read_bool(entry, "default");
        result.push_back(std::move(assignment));
    }
    return result;
}

/// One party an attach_photos step acts on, with the dataset that carries its
/// staff's photo keys.
struct photo_party_assignment {
    std::string party_name;
    std::string dataset;
};

/// The parties an attach_photos step dresses, and the template key of the
/// party logo it attaches to each of them.
struct photo_step_arguments {
    std::vector<photo_party_assignment> parties;
    std::string party_logo;
};

/**
 * @brief Reads an attach_photos step's arguments.
 *
 * A party's staff photos come from the dataset its entry names, so the kind
 * reads no office code and composes no dataset name of its own.
 */
[[nodiscard]] inline photo_step_arguments parse_photo_arguments(const std::string& arguments_json) {
    const auto arguments = detail::read_step_arguments(arguments_json);
    const auto* found = detail::member(arguments, "parties");
    if (found == nullptr)
        throw std::runtime_error("The step names no 'parties' argument.");

    auto entries = detail::read_object_list(*found, "parties");
    if (entries.empty())
        throw std::runtime_error("The step names no party in its 'parties' argument.");

    photo_step_arguments result;
    result.party_logo = detail::read_string(arguments, "party_logo");
    result.parties.reserve(entries.size());
    for (const auto& entry : entries) {
        photo_party_assignment assignment;
        assignment.party_name = detail::read_required_string(entry, "name", "party entry");
        assignment.dataset = detail::read_required_string(entry, "dataset", "party entry");
        result.parties.push_back(std::move(assignment));
    }
    return result;
}

/// The configuration bundles a start_market_feeds step publishes against the
/// system party, and the theme whose feeds it starts.
struct market_feed_step_arguments {
    std::vector<std::string> bundles;
    std::string theme;
};

/**
 * @brief Reads a start_market_feeds step's arguments.
 *
 * The theme names the dataset the feeds' configurations belong to; the kind
 * reads every other fact — the system party, the bindings' parties, the
 * folder — from the deployment.
 */
[[nodiscard]] inline market_feed_step_arguments
parse_market_feed_arguments(const std::string& arguments_json) {
    const auto arguments = detail::read_step_arguments(arguments_json);
    market_feed_step_arguments result;
    result.bundles = detail::read_bundles(arguments);
    result.theme = detail::read_string(arguments, "theme");
    if (result.theme.empty())
        throw std::runtime_error("The step names no 'theme' argument.");
    return result;
}

}

#endif

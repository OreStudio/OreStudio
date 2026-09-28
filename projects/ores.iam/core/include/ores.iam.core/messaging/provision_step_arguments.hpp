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
    publish_bundle,
    import_lei_hierarchy,
    provision_party,
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
    if (kind == "publish_bundle")
        return provision_step_action::publish_bundle;
    if (kind == "import_lei_hierarchy")
        return provision_step_action::import_lei_hierarchy;
    if (kind == "provision_party")
        return provision_step_action::provision_party;
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
    const auto arguments = detail::read_step_arguments(arguments_json);
    const auto* found = detail::member(arguments, "bundles");
    if (found == nullptr)
        throw std::runtime_error("The step names no 'bundles' argument.");

    auto bundles = detail::read_string_list(*found, "bundles");
    if (bundles.empty())
        throw std::runtime_error("The step names no bundle in its 'bundles' argument.");
    return bundles;
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
 * parameters, which is how the form supplies it.
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
    if (result.root_lei.empty())
        throw std::runtime_error("The step states no 'root_lei' argument and the run supplies no "
                                 "'root_lei' parameter.");

    return result;
}

}

#endif

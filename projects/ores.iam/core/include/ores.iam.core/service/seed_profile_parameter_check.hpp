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
#ifndef ORES_IAM_SERVICE_SEED_PROFILE_PARAMETER_CHECK_HPP
#define ORES_IAM_SERVICE_SEED_PROFILE_PARAMETER_CHECK_HPP

#include "ores.iam.api/domain/seed_profile_parameter.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <cctype>
#include <optional>
#include <rfl/json.hpp>
#include <string>
#include <utility>
#include <vector>

namespace ores::iam::service {

/**
 * @brief One parameter a provisioning request supplies, and its value.
 */
struct parameter_value {
    std::string name;
    std::string value;
};

/**
 * @brief The values a profile's declared parameters take for one request, or
 * the reason they were refused.
 *
 * @c refusal is empty exactly when the values were accepted. It names the field
 * it is about, because the person who typed the value is the one who has to fix
 * it, and the screen shows them one field at a time.
 */
struct checked_parameters {
    /// The values, in the order the profile declares its parameters.
    std::vector<parameter_value> values;
    /// Empty when the values were accepted; otherwise what to show the person.
    std::string refusal;

    [[nodiscard]] bool accepted() const {
        return refusal.empty();
    }
};

namespace detail {

/// Whether the text is an optionally signed run of digits and nothing else.
[[nodiscard]] inline bool reads_as_integer(const std::string& text) {
    if (text.empty())
        return false;
    auto digits = text.front() == '-' ? text.begin() + 1 : text.begin();
    if (digits == text.end())
        return false;
    return std::all_of(digits, text.end(), [](unsigned char c) { return std::isdigit(c) != 0; });
}

/// Whether the text is a boolean written the way the form writes one.
[[nodiscard]] inline bool reads_as_boolean(const std::string& text) {
    std::string lowered;
    lowered.reserve(text.size());
    for (const auto c : text)
        lowered.push_back(static_cast<char>(std::tolower(static_cast<unsigned char>(c))));
    return lowered == "true" || lowered == "false";
}

/// The choices a choice parameter declares, or nothing when the column is not a
/// JSON array of strings. A parameter that declares none reads as an empty list.
[[nodiscard]] inline std::optional<std::vector<std::string>>
read_choices(const std::string& choices_json) {
    if (choices_json.empty())
        return std::vector<std::string>{};
    auto parsed = rfl::json::read<std::vector<std::string>>(choices_json);
    if (!parsed)
        return std::nullopt;
    return *parsed;
}

} // namespace detail

/**
 * @brief Checks a request's parameter entries against the schema a profile
 * declares.
 *
 * Each entry is read as @c name=value. The check refuses, naming the field it
 * is about: an entry that is not a pair; a name supplied twice; a name the
 * profile does not declare; a required parameter the request states as empty;
 * a choice parameter whose value is not one of its choices; a value that does
 * not read as the parameter's declared data type; and a parameter whose
 * declared data type is not one of string, integer, boolean, choice or legal
 * entity, which is a defect in the profile rather than in the request. A legal
 * entity is named by its LEI, whose shape is the same fact the read that fills
 * it answers with.
 *
 * A parameter the request omits takes the profile's default, and one the
 * profile declares as required, with no default to take, is refused. An
 * accepted result carries one value per declared parameter, in the order the
 * profile declares them.
 */
[[nodiscard]] inline checked_parameters
check_parameters(const std::vector<domain::seed_profile_parameter>& declared,
                 const std::vector<std::string>& supplied) {
    std::vector<parameter_value> given;
    given.reserve(supplied.size());
    for (const auto& entry : supplied) {
        const auto at = entry.find('=');
        if (at == std::string::npos || at == 0)
            return {{}, "The value '" + entry + "' is not a name=value pair."};
        given.push_back({entry.substr(0, at), entry.substr(at + 1)});
    }

    for (std::size_t i = 0; i < given.size(); ++i)
        for (std::size_t j = i + 1; j < given.size(); ++j)
            if (given[i].name == given[j].name)
                return {{}, "The parameter '" + given[i].name + "' is supplied more than once."};

    for (const auto& g : given) {
        const auto declared_it = std::find_if(
            declared.begin(), declared.end(), [&](const auto& d) { return d.name == g.name; });
        if (declared_it == declared.end())
            return {{}, "The profile does not declare a parameter named '" + g.name + "'."};
    }

    checked_parameters result;
    result.values.reserve(declared.size());
    for (const auto& d : declared) {
        const auto given_it = std::find_if(
            given.begin(), given.end(), [&](const auto& g) { return g.name == d.name; });
        // An omitted parameter is the profile's to fill: it declares the value
        // the form starts with, so a caller that leaves the field untouched
        // carries nothing. A parameter the request states as empty is a
        // different thing -- the caller cleared a field -- and a required one
        // is refused rather than silently replaced.
        if (given_it == given.end()) {
            if (!d.default_value.empty()) {
                result.values.push_back({d.name, d.default_value});
                continue;
            }
            if (d.is_required)
                return {{}, "The parameter '" + d.name + "' is required and has no value."};
            result.values.push_back({d.name, ""});
            continue;
        }
        if (given_it->value.empty()) {
            if (d.is_required)
                return {{}, "The parameter '" + d.name + "' is required and has no value."};
            result.values.push_back({d.name, ""});
            continue;
        }

        const auto& value = given_it->value;
        if (d.data_type == "choice") {
            const auto choices = detail::read_choices(d.choices_json);
            if (!choices)
                return {{}, "The parameter '" + d.name + "' declares choices that cannot be read."};
            if (std::find(choices->begin(), choices->end(), value) == choices->end()) {
                std::string allowed;
                for (const auto& choice : *choices) {
                    if (!allowed.empty())
                        allowed += ", ";
                    allowed += choice;
                }
                return {{},
                        "The value '" + value + "' for '" + d.name + "' is not one of: " + allowed +
                            "."};
            }
        } else if (d.data_type == "integer") {
            if (!detail::reads_as_integer(value))
                return {{}, "The value '" + value + "' for '" + d.name + "' is not an integer."};
        } else if (d.data_type == "boolean") {
            if (!detail::reads_as_boolean(value))
                return {{}, "The value '" + value + "' for '" + d.name + "' is not true or false."};
        } else if (d.data_type == "legal_entity") {
            /*
             * A legal entity is named by its LEI. The shape is checked here
             * because a screen fills this setting from a search over the
             * entities the deployment holds, and a value that is not an LEI
             * cannot have come from one.
             */
            const bool is_lei =
                value.size() == 20 && std::all_of(value.begin(), value.end(), [](unsigned char c) {
                    return std::isalnum(c) != 0;
                });
            if (!is_lei)
                return {{}, "The value '" + value + "' for '" + d.name + "' is not an LEI."};
        } else if (d.data_type != "string") {
            return {{},
                    "The parameter '" + d.name + "' declares the unknown data type '" +
                        d.data_type + "'."};
        }

        result.values.push_back({d.name, value});
    }
    return result;
}

}

#endif

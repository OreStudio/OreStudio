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
#include "ores.http/routes/refdata/curve_amend_rules.hpp"
#include "ores.utility/codec/text_form.hpp"
#include <algorithm>
#include <cctype>
#include <charconv>
#include <regex>
#include <string>
#include <vector>

namespace ores::http::routes::refdata::amend {

/**
 * @brief Why the leaf would not take the value, or nothing when it would.
 *
 * The value set tables are the ORE parser's own lists, so a value outside them is
 * one ORE refuses to read. The store's guard does not cover the curve tables yet,
 * so this boundary is where the refusal happens.
 */
std::optional<std::string> refusal_of(const ores::http::routes::value_fields& fields,
                                      const std::string& label,
                                      bool required,
                                      const document::cell& value) {
    if (!value || value->empty())
        return required ? std::optional<std::string>(label + " is required.") : std::nullopt;
    if (!fields.choices.empty()) {
        const auto matches = std::ranges::find_if(
            fields.choices, [&](const auto& choice) { return choice.value == *value; });
        if (matches == fields.choices.end()) {
            std::string accepted;
            for (const auto& choice : fields.choices)
                accepted += (accepted.empty() ? "" : ", ") + choice.value;
            return label + " must be one of " + accepted + ", and \"" + *value + "\" is not.";
        }
    }
    if (fields.minimum || fields.maximum) {
        double number = 0;
        const auto read = std::from_chars(value->data(), value->data() + value->size(), number);
        if (read.ec != std::errc())
            return label + " must be a number.";
        if (fields.minimum && number < *fields.minimum)
            return label + " must be at least " + document::shortest_text(*fields.minimum) + ".";
        if (fields.maximum && number > *fields.maximum)
            return label + " must be at most " + document::shortest_text(*fields.maximum) + ".";
    }
    if (!fields.pattern.empty() && !std::regex_match(*value, std::regex(fields.pattern)))
        return label + " must match " + fields.pattern + ".";
    return std::nullopt;
}

/// Whether a service's refusal says the row moved on since the caller read it.
bool is_stale(const std::string& message) {
    std::string lowered;
    lowered.reserve(message.size());
    for (const auto character : message)
        lowered.push_back(static_cast<char>(std::tolower(static_cast<unsigned char>(character))));
    return lowered.find("version conflict") != std::string::npos ||
           lowered.find("version_conflict") != std::string::npos ||
           lowered.find("does not match") != std::string::npos;
}

}

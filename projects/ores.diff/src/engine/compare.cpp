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
#include "ores.diff/engine/compare.hpp"

#include <string_view>
#include <unordered_map>

namespace ores::diff::engine {

domain::diff_result
compute(const std::vector<domain::field_value>& previous,
        const std::vector<domain::field_value>& current) {
    // Index the previous fields by name. First occurrence wins when a
    // name repeats; string_view keys borrow from `previous`, which
    // outlives the map.
    std::unordered_map<std::string_view, const domain::field_value*> by_name;
    by_name.reserve(previous.size());
    for (const auto& field : previous)
        by_name.try_emplace(field.name, &field);

    domain::diff_result result;

    // Current-list order drives the output: changed and added fields.
    std::unordered_map<std::string_view, bool> seen_in_current;
    seen_in_current.reserve(current.size());
    for (const auto& field : current) {
        if (!seen_in_current.try_emplace(field.name, true).second)
            continue; // First occurrence wins.

        const auto it = by_name.find(field.name);
        if (it == by_name.end()) {
            // Added: absent side is empty.
            result.entries.push_back(
                {.field_name = field.name,
                 .old_value = {},
                 .new_value = field.value});
        } else if (it->second->value != field.value) {
            result.entries.push_back(
                {.field_name = field.name,
                 .old_value = it->second->value,
                 .new_value = field.value});
        }
    }

    // Removed fields follow, in the previous list's order.
    std::unordered_map<std::string_view, bool> seen_in_previous;
    seen_in_previous.reserve(previous.size());
    for (const auto& field : previous) {
        if (!seen_in_previous.try_emplace(field.name, true).second)
            continue; // First occurrence wins.

        if (!seen_in_current.contains(field.name)) {
            result.entries.push_back(
                {.field_name = field.name,
                 .old_value = field.value,
                 .new_value = {}});
        }
    }

    return result;
}

}

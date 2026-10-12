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
#include "ores.analytics.core/service/publish_subject_plan.hpp"
#include <algorithm>
#include <cctype>

namespace ores::analytics::service {

std::string publish_from_dq_function(std::string_view subject) {
    // The operation is the last segment of a reserved-namespace subject, so
    // the name of the function it expands through is the operation itself:
    // analytics.v1.ops.publish_pricing_engines_from_dq becomes
    // ores_analytics_publish_pricing_engines_from_dq_fn.
    constexpr std::string_view ops = "v1.ops.";
    const auto pos = subject.find(ops);
    if (pos == std::string_view::npos)
        return {};
    const auto operation = subject.substr(pos + ops.size());
    constexpr std::string_view suffix = "_from_dq";
    if (operation.size() <= suffix.size() || !operation.ends_with(suffix))
        return {};
    // The name is put into SQL as an identifier, so it holds lower-case letters,
    // digits and underscores and nothing else.
    const auto plain = std::ranges::all_of(operation, [](const unsigned char c) {
        return std::islower(c) || std::isdigit(c) || c == '_';
    });
    if (!plain)
        return {};
    return "ores_analytics_" + std::string(operation) + "_fn";
}

}

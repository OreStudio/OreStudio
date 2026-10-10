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
#include "ores.refdata.service/messaging/book_proposal_mapping.hpp"
#include <algorithm>

namespace ores::refdata::messaging {

std::vector<std::string> required_permissions(const std::vector<domain::book_change>& lines) {
    const bool deletes = std::ranges::any_of(
        lines, [](const auto& l) { return l.operation == "delete"; });
    const bool writes = std::ranges::any_of(
        lines, [](const auto& l) { return l.operation != "delete"; });

    std::vector<std::string> needed;
    if (writes)
        needed.push_back("refdata::books:write");
    if (deletes)
        needed.push_back("refdata::books:delete");
    return needed;
}

std::vector<book_line_outcome> to_outcomes(const service::book_preview& preview) {
    std::vector<book_line_outcome> out;
    out.reserve(preview.lines.size());
    for (const auto& l : preview.lines) {
        out.push_back({.line_no = l.line_no,
                       .operation = l.operation,
                       .entity_id = l.entity_id,
                       .columns = l.columns,
                       .refusal = l.refusal});
    }
    return out;
}

}

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
#include "ores.refdata.service/messaging/counterparty_proposal_mapping.hpp"
#include <algorithm>
#include <string>

namespace ores::refdata::messaging {

namespace {

template <typename Line>
bool all_valid(const std::vector<Line>& lines) {
    return std::ranges::all_of(
        lines, [](const auto& l) { return l.operation == "put" || l.operation == "delete"; });
}

template <typename Line>
bool any_op(const std::vector<Line>& lines, const char* operation) {
    return std::ranges::any_of(lines, [&](const auto& l) { return l.operation == operation; });
}

}

std::string invalid_lines(const service::counterparty_lines& lines) {
    if (lines.size() == 0)
        return "Propose at least one change.";
    if (!all_valid(lines.counterparties) || !all_valid(lines.identifiers) ||
        !all_valid(lines.links))
        return "The operation must be put or delete.";
    return {};
}

std::vector<std::string> required_permissions(const service::counterparty_lines& lines) {
    std::vector<std::string> needed;
    const auto need = [&](const auto& kind_lines, const char* write, const char* remove) {
        if (any_op(kind_lines, "put"))
            needed.push_back(write);
        if (any_op(kind_lines, "delete"))
            needed.push_back(remove);
    };
    need(lines.counterparties, "refdata::counterparties:write", "refdata::counterparties:delete");
    need(lines.identifiers,
         "refdata::counterparty_identifiers:write",
         "refdata::counterparty_identifiers:delete");
    need(
        lines.links, "refdata::party_counterparties:write", "refdata::party_counterparties:delete");
    return needed;
}

step_result recheck_step_result(const service::counterparty_preview& preview) {
    for (const auto& l : preview.lines) {
        if (!l.refusal.empty())
            return {.completed = false,
                    .error = "Line " + std::to_string(l.line_no) + ": " + l.refusal};
    }
    return {.completed = true, .error = {}};
}

step_result apply_step_result(const service::counterparty_apply& applied) {
    if (applied.applied)
        return {.completed = true, .error = {}};
    if (applied.transient)
        return {.completed = false,
                .error = "Could not apply now, and the apply may run again: " + applied.refusal};
    if (applied.failed_line > 0)
        return {.completed = false,
                .error = "Line " + std::to_string(applied.failed_line) + ": " + applied.refusal};
    return {.completed = false, .error = applied.refusal};
}

std::vector<counterparty_line_outcome> to_outcomes(const service::counterparty_preview& preview) {
    std::vector<counterparty_line_outcome> out;
    out.reserve(preview.lines.size());
    for (const auto& l : preview.lines) {
        out.push_back({.line_no = l.line_no,
                       .entity_type = l.entity_type,
                       .operation = l.operation,
                       .subject_id = l.subject_id,
                       .columns = l.columns,
                       .refusal = l.refusal,
                       .part_codes = l.part_codes});
    }
    return out;
}

std::vector<counterparty_change_delta>
to_deltas(const std::vector<service::counterparty_line_delta>& deltas) {
    std::vector<counterparty_change_delta> out;
    out.reserve(deltas.size());
    for (const auto& d : deltas) {
        counterparty_change_delta wire{.line_no = d.line_no,
                                       .entity_type = d.entity_type,
                                       .operation = d.operation,
                                       .subject_id = d.subject_id,
                                       .base_version = d.base_version,
                                       .applied = d.applied,
                                       .fields = {}};
        for (const auto& f : d.fields) {
            wire.fields.push_back(counterparty_field_delta{
                .name = f.name, .before = f.before, .after = f.after, .changed = f.changed});
        }
        out.push_back(std::move(wire));
    }
    return out;
}

}

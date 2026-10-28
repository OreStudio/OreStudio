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
#include "ores.reporting.core/results/canonical_log_reader.hpp"

#include <cstddef>
#include <string>
#include <vector>

namespace ores::reporting::results {
namespace {

constexpr std::string_view log_expected_header =
    "ordinal,severity,scope,message,subject,subject_id,cause,event_time,source";

std::vector<std::string> log_split_fields(std::string_view line) {
    std::vector<std::string> fields;
    std::string field;
    bool quoted = false;
    for (const auto c : line) {
        if (c == '"') {
            quoted = !quoted;
        } else if (c == ',' && !quoted) {
            fields.push_back(field);
            field.clear();
        } else {
            field.push_back(c);
        }
    }
    fields.push_back(field);
    return fields;
}

}

log_read_outcome read_canonical_log_csv(std::string_view csv) {
    log_read_outcome outcome;

    if (csv.substr(0, 3) == "\xEF\xBB\xBF")
        csv.remove_prefix(3);
    if (csv.empty()) {
        outcome.refusals.push_back("the log rows are empty.");
        return outcome;
    }

    const auto first_break = csv.find('\n');
    auto header = csv.substr(0, first_break);
    if (!header.empty() && header.back() == '\r')
        header.remove_suffix(1);
    if (header != log_expected_header) {
        outcome.refusals.push_back("the log rows state a header of '" + std::string(header) +
                                   "', not '" + std::string(log_expected_header) + "'.");
        return outcome;
    }

    std::size_t line_number = 1;
    std::size_t offset = first_break == std::string_view::npos ? csv.size() : first_break + 1;
    while (offset < csv.size()) {
        ++line_number;
        const auto line_break = csv.find('\n', offset);
        auto line = csv.substr(offset,
                               line_break == std::string_view::npos ? csv.size() - offset :
                                                                      line_break - offset);
        offset = line_break == std::string_view::npos ? csv.size() : line_break + 1;
        if (!line.empty() && line.back() == '\r')
            line.remove_suffix(1);
        if (line.find_first_not_of(" \t") == std::string_view::npos)
            continue;

        const auto fields = log_split_fields(line);
        if (fields.size() != 9) {
            outcome.refusals.push_back("line " + std::to_string(line_number) + " states " +
                                       std::to_string(fields.size()) + " fields, not nine.");
            continue;
        }
        if (fields[1].empty() || fields[3].empty()) {
            outcome.refusals.push_back("line " + std::to_string(line_number) +
                                       " states no severity or no message.");
            continue;
        }

        service::engine_log_error row;
        row.severity = fields[1];
        row.scope = fields[2];
        row.message = fields[3];
        row.subject = fields[4];
        row.subject_id = fields[5];
        row.cause = fields[6];
        row.event_time = fields[7];
        row.source = fields[8];
        outcome.rows.push_back(std::move(row));
    }

    return outcome;
}

}

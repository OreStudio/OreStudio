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
#include "ores.reporting.core/results/canonical_reader.hpp"

#include <charconv>
#include <cstddef>
#include <string>
#include <vector>

namespace ores::reporting::results {
namespace {

constexpr std::string_view expected_header =
    "subject_type,subject_id,measure,currency,counterparty_id,figure,source";

std::vector<std::string> split_fields(std::string_view line) {
    std::vector<std::string> fields;
    std::string field;
    bool quoted = false;
    for (const auto c : line) {
        if (c == '"') {
            quoted = !quoted;
        } else if (c == ',' && !quoted) {
            fields.push_back(field);
            field.clear();
        } else if (c != '\r') {
            field.push_back(c);
        }
    }
    fields.push_back(field);
    return fields;
}

bool figure_of(std::string_view text, double& figure) {
    if (text.empty())
        return false;
    const auto* first = text.data();
    const auto* last = text.data() + text.size();
    const auto parsed = std::from_chars(first, last, figure);
    return parsed.ec == std::errc() && parsed.ptr == last;
}

}

read_outcome read_canonical_csv(std::string_view csv) {
    read_outcome outcome;

    if (csv.substr(0, 3) == "\xEF\xBB\xBF")
        csv.remove_prefix(3);
    if (csv.empty()) {
        outcome.refusals.push_back("the canonical rows are empty.");
        return outcome;
    }

    const auto first_break = csv.find('\n');
    auto header_line = csv.substr(0, first_break);
    if (!header_line.empty() && header_line.back() == '\r')
        header_line.remove_suffix(1);
    if (header_line != expected_header) {
        outcome.refusals.push_back("the canonical rows state a header of '" +
                                   std::string(header_line) + "', not '" +
                                   std::string(expected_header) + "'.");
        return outcome;
    }

    std::size_t line_number = 1;
    std::size_t offset = first_break == std::string_view::npos ? csv.size() : first_break + 1;
    while (offset < csv.size()) {
        ++line_number;
        const auto line_break = csv.find('\n', offset);
        const auto line = csv.substr(offset,
                                     line_break == std::string_view::npos ? csv.size() - offset :
                                                                            line_break - offset);
        offset = line_break == std::string_view::npos ? csv.size() : line_break + 1;
        if (line.find_first_not_of("\r \t") == std::string_view::npos)
            continue;

        const auto fields = split_fields(line);
        if (fields.size() != 7) {
            outcome.refusals.push_back("line " + std::to_string(line_number) + " states " +
                                       std::to_string(fields.size()) + " fields, not seven.");
            continue;
        }

        canonical_row row{.subject_type = fields[0],
                          .subject_id = fields[1],
                          .measure = fields[2],
                          .currency = fields[3],
                          .counterparty_id = fields[4],
                          .figure = 0.0,
                          .source = fields[6]};
        if (row.subject_type.empty() || row.subject_id.empty() || row.measure.empty()) {
            outcome.refusals.push_back("line " + std::to_string(line_number) +
                                       " states no subject or no measure.");
            continue;
        }
        if (!figure_of(fields[5], row.figure)) {
            outcome.refusals.push_back("line " + std::to_string(line_number) + " states '" +
                                       fields[5] + "', which is not a figure.");
            continue;
        }
        outcome.rows.push_back(std::move(row));
    }

    return outcome;
}

}

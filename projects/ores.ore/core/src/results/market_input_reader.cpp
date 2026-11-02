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
#include "ores.ore.core/results/market_input_reader.hpp"
#include "output_csv.hpp"
#include <algorithm>
#include <array>
#include <string>

namespace ores::ore::results {

using namespace ores::ore::results::output_csv;

namespace {

/**
 * @brief The headings one family states its date, its subject and its value under.
 */
struct input_columns {
    std::string_view family;
    std::string_view date;
    std::string_view datum;
    std::string_view value;
    std::string_view payment_date;
};

constexpr std::array<input_columns, 3> columns_of_families = {{
    {"market_data_report", "datumDate", "datumId", "datumValue", ""},
    {"fixings_report", "fixingDate", "fixingId", "fixingValue", ""},
    {"dividends_report", "dividendExDate", "equityId", "dividendRate", "dividendPaymentDate"},
}};

const input_columns* columns_of(const output_family& family) {
    const auto it = std::ranges::find_if(
        columns_of_families, [&](const input_columns& one) { return one.family == family.code; });
    return it == columns_of_families.end() ? nullptr : &*it;
}

}

bool is_market_input_family(const output_family& family) {
    return columns_of(family) != nullptr;
}

market_input_read_outcome read_market_inputs(std::string_view csv, const output_family& family) {
    market_input_read_outcome outcome;

    const auto* columns = columns_of(family);
    if (columns == nullptr) {
        outcome.refusals.push_back(family.code + " is not a family of market inputs.");
        return outcome;
    }

    if (csv.substr(0, 3) == "\xEF\xBB\xBF")
        csv.remove_prefix(3);
    if (csv.empty()) {
        outcome.refusals.push_back(family.code + " is empty.");
        return outcome;
    }

    const auto first_break = csv.find('\n');
    auto header_line = csv.substr(0, first_break);
    if (!header_line.empty() && header_line.front() == '#')
        header_line.remove_prefix(1);
    if (!header_line.empty() && header_line.back() == '\r')
        header_line.remove_suffix(1);
    const auto headings = fields_of(header_line);

    for (const auto& required : family.required_columns) {
        if (std::ranges::find(headings, required) == headings.end()) {
            outcome.refusals.push_back(family.code + " states no " + required +
                                       " column; it states: " + joined(headings) + ".");
            return outcome;
        }
    }

    const auto at = [&headings](std::string_view name) -> std::size_t {
        return static_cast<std::size_t>(
            std::distance(headings.begin(), std::ranges::find(headings, name)));
    };
    const auto date_at = at(columns->date);
    const auto datum_at = at(columns->datum);
    const auto value_at = at(columns->value);
    const auto payment_at =
        columns->payment_date.empty() ? headings.size() : at(columns->payment_date);

    std::size_t line_number = 1;
    std::size_t offset = first_break == std::string_view::npos ? csv.size() : first_break + 1;
    while (offset < csv.size()) {
        ++line_number;
        const auto line_break = csv.find('\n', offset);
        auto line = csv.substr(offset,
                               line_break == std::string_view::npos ? csv.size() - offset :
                                                                      line_break - offset);
        offset = line_break == std::string_view::npos ? csv.size() : line_break + 1;
        if (line.find_first_not_of("\r \t") == std::string_view::npos)
            continue;
        if (line.back() == '\r')
            line.remove_suffix(1);

        const auto fields = fields_of(line);
        const auto field_at = [&fields](std::size_t index) -> std::string {
            if (index >= fields.size() || is_null(fields[index]))
                return {};
            return fields[index];
        };

        market_input_row row{.as_of = field_at(date_at),
                             .datum_id = field_at(datum_at),
                             .payment_date = field_at(payment_at),
                             .line = static_cast<int>(line_number)};
        if (row.as_of.empty() || row.datum_id.empty()) {
            outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                       " names no date or nothing to value.");
            continue;
        }
        const auto text = field_at(value_at);
        if (!figure_of(text, row.value)) {
            outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                       " states the value '" + text + "', which is not a figure.");
            continue;
        }
        outcome.rows.push_back(std::move(row));
    }

    return outcome;
}

}

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
#include "ores.ore.core/results/calibration_reader.hpp"
#include "output_csv.hpp"
#include <algorithm>
#include <string>

namespace ores::ore::results {

using namespace ores::ore::results::output_csv;

bool is_calibration_family(const output_family& family) {
    return family.code == "todaysmarketcalibration";
}

calibration_read_outcome read_calibration(std::string_view csv, const output_family& family) {
    calibration_read_outcome outcome;

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
    const auto type_at = at("MarketObjectType");
    const auto id_at = at("MarketObjectId");
    const auto name_at = at("ResultId");
    const auto key_1_at = at("ResultKey1");
    const auto key_2_at = at("ResultKey2");
    const auto key_3_at = at("ResultKey3");
    const auto result_type_at = at("ResultType");
    const auto value_at = at("ResultValue");

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

        calibration_row row{.market_object_type = field_at(type_at),
                            .market_object_id = field_at(id_at),
                            .result_name = field_at(name_at),
                            .result_key_1 = field_at(key_1_at),
                            .result_key_2 = field_at(key_2_at),
                            .result_key_3 = field_at(key_3_at),
                            .result_type = field_at(result_type_at),
                            .value = field_at(value_at),
                            .line = static_cast<int>(line_number)};

        if (row.market_object_type.empty() || row.market_object_id.empty()) {
            outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                       " names no market object.");
            continue;
        }
        if (row.result_type == "double" && !row.value.empty()) {
            double figure = 0.0;
            if (!figure_of(row.value, figure)) {
                outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                           " states the double '" + row.value +
                                           "', which is not a figure.");
                continue;
            }
            row.figure = figure;
        }
        outcome.rows.push_back(std::move(row));
    }

    return outcome;
}

}

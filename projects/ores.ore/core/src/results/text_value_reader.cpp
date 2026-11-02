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
#include "ores.ore.core/results/text_value_reader.hpp"
#include "output_csv.hpp"
#include <algorithm>
#include <array>
#include <string>

namespace ores::ore::results {

using namespace ores::ore::results::output_csv;

namespace {

/**
 * @brief The headings one family states its subject, name, type and value under.
 *
 * An empty name heading means the value's name is the value heading itself; a
 * document family states one value, the whole body after the heading.
 */
struct text_columns {
    std::string_view family;
    std::string_view subject;
    std::string_view name;
    std::string_view type;
    std::string_view value;
    bool document = false;
};

constexpr std::array<text_columns, 6> text_columns_of_families = {{
    {"additional_results", "TradeId", "ResultId", "ResultType", "ResultValue"},
    {"marketObjects", "", "MarketObjectType", "", "MarketObjectName"},
    {"riskFactors", "", "RiskFactorType", "", "RiskFactorName"},
    {"underlying_indices", "", "AssetType", "", "Indices"},
    {"swap_indices", "", "", "", "SwapIndices"},
    {"calibration", "", "", "", "CrossAssetModel", true},
}};

const text_columns* text_columns_of(const output_family& family) {
    const auto it = std::ranges::find_if(
        text_columns_of_families, [&](const text_columns& one) { return one.family == family.code; });
    return it == text_columns_of_families.end() ? nullptr : &*it;
}

std::string_view trimmed_text(std::string_view text) {
    const auto first = text.find_first_not_of("\r\n \t");
    if (first == std::string_view::npos)
        return {};
    const auto last = text.find_last_not_of("\r\n \t");
    return text.substr(first, last - first + 1);
}

}

bool is_text_value_family(const output_family& family) {
    return text_columns_of(family) != nullptr;
}

text_value_read_outcome read_text_values(std::string_view csv, const output_family& family) {
    text_value_read_outcome outcome;

    const auto* columns = text_columns_of(family);
    if (columns == nullptr) {
        outcome.refusals.push_back(family.code + " is not a family of text values.");
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

    const auto body = first_break == std::string_view::npos ? std::string_view() :
                                                              csv.substr(first_break + 1);
    if (columns->document) {
        const auto document = trimmed_text(body);
        // The engine writes the model only when it calibrated one, so a heading
        // with nothing under it is a file cut short, not an empty result.
        if (document.empty()) {
            outcome.refusals.push_back(family.code + " states no " + std::string(columns->value) +
                                       ".");
            return outcome;
        }
        outcome.rows.push_back(text_value_row{.name = std::string(columns->value),
                                                  .value_type = "xml",
                                                  .value = std::string(document),
                                                  .line = 2});
        return outcome;
    }

    const auto at = [&headings](std::string_view name) -> std::size_t {
        if (name.empty())
            return headings.size();
        return static_cast<std::size_t>(
            std::distance(headings.begin(), std::ranges::find(headings, name)));
    };
    const auto subject_at = at(columns->subject);
    const auto name_at = at(columns->name);
    const auto type_at = at(columns->type);
    const auto value_at = at(columns->value);

    std::size_t line_number = 1;
    std::size_t offset = 0;
    while (offset < body.size()) {
        ++line_number;
        const auto line_break = body.find('\n', offset);
        auto line = body.substr(offset,
                                line_break == std::string_view::npos ? body.size() - offset :
                                                                       line_break - offset);
        offset = line_break == std::string_view::npos ? body.size() : line_break + 1;
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

        text_value_row row{.subject = field_at(subject_at),
                           .name = columns->name.empty() ? std::string(columns->value) :
                                                           field_at(name_at),
                           .value_type = columns->type.empty() ? "text" : field_at(type_at),
                           .value = field_at(value_at),
                           .line = static_cast<int>(line_number)};
        if (!row.subject.empty())
            row.subject_type = "trade";
        if (row.name.empty()) {
            outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                       " names no value.");
            continue;
        }
        if (row.value_type == "double" && !row.value.empty()) {
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

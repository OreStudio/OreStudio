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
#include "ores.ore.core/results/cashflow_reader.hpp"
#include "output_csv.hpp"
#include <algorithm>
#include <array>
#include <string>

namespace ores::ore::results {
namespace {

using namespace ores::ore::results::output_csv;

// The kind of record a key names, as the corpus spells it. A cashflow's subject
// is the trade or the market object the payment belongs to; the two families
// that price a portfolio and the one that builds a curve name it differently.
std::string cashflow_subject_type_of(std::string_view key) {
    if (key == "TradeId" || key == "TradeID")
        return "trade";
    if (key == "MarketObjectId")
        return "market_object";
    return {};
}

// A stressed family states each figure three times. The suffix says which of
// the three a column is, and the stem is the figure a reader compares.
constexpr std::array<std::pair<std::string_view, std::string_view>, 3> parts = {
    {{"_Base", "base"}, {"_Scen", "scen"}, {"_Diff", "diff"}}};

std::pair<std::string, std::string> measure_and_part(std::string_view heading) {
    for (const auto& [suffix, part] : parts) {
        if (heading.size() > suffix.size() && heading.ends_with(suffix))
            return {std::string(heading.substr(0, heading.size() - suffix.size())),
                    std::string(part)};
    }
    return {std::string(heading), {}};
}

}

bool is_cashflow_family(const output_family& family) {
    const auto names_number = [&family](std::string_view key) {
        return std::ranges::find(family.key_columns, key) != family.key_columns.end();
    };
    return names_number("CashflowNo") && names_number("LegNo");
}

cashflow_read_outcome read_cashflows(std::string_view csv, const output_family& family) {
    cashflow_read_outcome outcome;

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
        const auto found = std::ranges::find(headings, name);
        return found == headings.end() ?
                   headings.size() :
                   static_cast<std::size_t>(std::distance(headings.begin(), found));
    };

    const auto subject_at = at("TradeId") != headings.size() ? at("TradeId") : at("MarketObjectId");
    const auto subject_type = subject_at == headings.size() ?
                                  std::string() :
                                  cashflow_subject_type_of(headings[subject_at]);
    const auto scenario_at = at("ScenarioLabel");
    const auto date_at = at("PillarDate");
    const auto cashflow_at = at("CashflowNo");
    const auto leg_at = at("LegNo");
    const auto pay_date_at = at("PayDate");
    const auto flow_type_at = at("FlowType");
    const auto currency_at = at("Currency");

    // The names the row carries in their own columns. Every other qualifier
    // travels beside the figure, so a family gains no column in the table.
    const auto promoted = [](std::string_view name) {
        return name == "TradeId" || name == "TradeID" || name == "MarketObjectId" ||
               name == "ScenarioLabel" || name == "CashflowNo" || name == "LegNo" ||
               name == "PayDate" || name == "FlowType" || name == "Currency" ||
               name == "PillarDate" || name == "Date";
    };
    std::vector<std::pair<std::string, std::size_t>> qualifier_at;
    for (const auto& name : family.qualifier_columns) {
        const auto index = at(name);
        if (index != headings.size() && !promoted(name))
            qualifier_at.emplace_back(name, index);
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

        const auto fields = fields_of(line);
        const auto field_at = [&fields](std::size_t index) -> std::string_view {
            return index < fields.size() ? std::string_view(fields[index]) : std::string_view();
        };

        cashflow_row base;
        base.source = family.code;
        if (subject_at != headings.size()) {
            base.subject_type = subject_type.empty() ? "row" : subject_type;
            base.subject = field_at(subject_at);
        }
        if (scenario_at != headings.size())
            base.scenario = field_at(scenario_at);
        if (date_at != headings.size())
            base.as_of = field_at(date_at);
        if (pay_date_at != headings.size())
            base.pay_date = field_at(pay_date_at);
        if (flow_type_at != headings.size())
            base.flow_type = field_at(flow_type_at);
        if (currency_at != headings.size())
            base.currency = field_at(currency_at);
        if (cashflow_at == headings.size() || is_null(field_at(cashflow_at))) {
            outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                       " states no CashflowNo, so its figures name no payment.");
            continue;
        }
        if (leg_at == headings.size() || is_null(field_at(leg_at))) {
            outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                       " states no LegNo, so its figures name no payment.");
            continue;
        }
        base.cashflow_no = field_at(cashflow_at);
        base.leg_no = field_at(leg_at);
        for (const auto& [name, index] : qualifier_at) {
            const auto value = field_at(index);
            if (!is_null(value))
                base.qualifiers.emplace_back(name, std::string(value));
        }
        // A key that is not one of the payment's own columns states context.
        for (const auto& key : family.key_columns) {
            if (promoted(key))
                continue;
            const auto value = field_at(at(key));
            if (!is_null(value))
                base.qualifiers.emplace_back(key, std::string(value));
        }

        for (const auto& heading : headings) {
            if (role_of(family, heading) != output_column_role::measure)
                continue;
            const auto text = field_at(at(heading));
            if (is_null(text))
                continue;
            double figure = 0.0;
            if (!figure_of(text, figure)) {
                outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                           " states " + heading + " as '" + std::string(text) +
                                           "', which is not a figure.");
                continue;
            }
            auto [measure, part] = measure_and_part(heading);
            auto row = base;
            row.measure = std::move(measure);
            row.part = std::move(part);
            row.value = figure;
            outcome.rows.push_back(std::move(row));
        }
    }

    return outcome;
}

cashflow_read_outcome read_cashflows_for_file(std::string_view csv, std::string_view file_name) {
    const auto* family = family_of_file(file_name);
    if (family == nullptr) {
        cashflow_read_outcome outcome;
        outcome.refusals.push_back("no family states the file " + std::string(file_name) + ".");
        return outcome;
    }
    return read_cashflows(csv, *family);
}

}

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
#include "ores.ore.core/results/metric_reader.hpp"
#include "output_csv.hpp"
#include <algorithm>
#include <string>

namespace ores::ore::results {
namespace {

using namespace ores::ore::results::output_csv;

std::string subject_type_of(std::string_view key) {
    if (key == "TradeId" || key == "TradeID")
        return "trade";
    if (key == "NettingSet" || key == "NettingSetId" || key == "NettingSetID")
        return "netting_set";
    if (key == "Portfolio" || key == "PortfolioId" || key == "PortfolioID")
        return "portfolio";
    if (key == "MarketObjectId")
        return "market_object";
    if (key == "PrincipalComponent")
        return "principal_component";
    return {};
}

// The order in which a row's keys are tried as the subject: a record key
// first, then any key that is not the observation itself. The corpus states a
// total whose trade is empty and whose netting set is not, so the subject is
// chosen for each row and not for the family.
bool same_letters(std::string_view left, std::string_view right) {
    if (left.size() != right.size())
        return false;
    for (std::size_t i = 0; i < left.size(); ++i) {
        const auto lower = [](char c) {
            return static_cast<char>(c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
        };
        if (lower(left[i]) != lower(right[i]))
            return false;
    }
    return true;
}

// The currency of one figure. A family states it once for the row, or once for
// each figure it states, as npv states NpvCurrency beside NPV. The heading that
// names the figure decides which, so no family is listed here.
std::string_view currency_of(const std::vector<std::string>& headings, std::string_view measure) {
    const auto exact = std::ranges::find(headings, "Currency");
    if (exact != headings.end())
        return *exact;
    for (const auto& heading : headings) {
        if (heading.size() <= std::string_view("Currency").size())
            continue;
        if (same_letters(heading.substr(0, heading.size() - 8), measure) &&
            same_letters(heading.substr(heading.size() - 8), "Currency"))
            return heading;
    }
    return {};
}

std::vector<std::size_t> subject_order(const output_family& family) {
    std::vector<std::size_t> order;
    for (std::size_t i = 0; i < family.key_columns.size(); ++i) {
        if (!subject_type_of(family.key_columns[i]).empty())
            order.push_back(i);
    }
    for (std::size_t i = 0; i < family.key_columns.size(); ++i) {
        const auto& key = family.key_columns[i];
        if (subject_type_of(key).empty() && key != "Date" && key != "Time")
            order.push_back(i);
    }
    return order;
}

}

metric_read_outcome read_metrics(std::string_view csv, const output_family& family) {
    metric_read_outcome outcome;

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

    // Every required column must be there, or the file is not this family's.
    for (const auto& required : family.required_columns) {
        if (std::ranges::find(headings, required) == headings.end()) {
            outcome.refusals.push_back(family.code + " states no " + required +
                                       " column; it states: " + joined(headings) + ".");
            return outcome;
        }
    }

    std::vector<std::size_t> key_at;
    for (const auto& key : family.key_columns) {
        const auto at = std::ranges::find(headings, key);
        key_at.push_back(static_cast<std::size_t>(std::distance(headings.begin(), at)));
    }

    // A heading the catalogue does not state is a measure when the family's
    // measures are open, and a refusal when they are closed.
    std::vector<std::string> measures = family.measure_columns;
    for (const auto& heading : headings) {
        if (role_of(family, heading) != output_column_role::none)
            continue;
        if (!family.measures_open) {
            outcome.refusals.push_back(family.code + " states no " + heading +
                                       " column, and its measures are closed.");
            return outcome;
        }
        measures.push_back(heading);
    }

    const auto subject_keys = subject_order(family);
    const auto date_at = std::ranges::find(headings, "Date");

    std::vector<std::pair<std::string, std::size_t>> qualifier_at;
    for (const auto& name : family.qualifier_columns) {
        const auto at = std::ranges::find(headings, name);
        if (at != headings.end())
            qualifier_at.emplace_back(
                name, static_cast<std::size_t>(std::distance(headings.begin(), at)));
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

        metric base;
        base.source = family.code;
        std::size_t subject_key = family.key_columns.size();
        for (const auto index : subject_keys) {
            if (is_null(field_at(key_at[index])))
                continue;
            subject_key = index;
            const auto type = subject_type_of(family.key_columns[index]);
            base.subject_type = type.empty() ? "row" : type;
            base.subject = field_at(key_at[index]);
            break;
        }
        if (date_at != headings.end())
            base.as_of =
                field_at(static_cast<std::size_t>(std::distance(headings.begin(), date_at)));

        // The scenario and the factor are stated by name, because a screen
        // filters on them; everything else travels as one qualifier.
        const auto lift = [&base](std::string_view name, std::string_view value) {
            if (is_null(value))
                return;
            if (name == "Scenario" || name == "ScenarioLabel")
                base.scenario = value;
            else if (name == "Factor")
                base.factor = value;
            else
                base.qualifiers.emplace_back(std::string(name), std::string(value));
        };
        for (const auto& [name, at] : qualifier_at)
            lift(name, field_at(at));
        // A key that is neither the subject nor the observation date states
        // context, so it travels with the metric.
        for (std::size_t i = 0; i < family.key_columns.size(); ++i) {
            if (i == subject_key || family.key_columns[i] == "Date")
                continue;
            lift(family.key_columns[i], field_at(key_at[i]));
        }

        for (const auto& measure : measures) {
            const auto at = std::ranges::find(headings, measure);
            if (at == headings.end())
                continue;
            const auto text =
                field_at(static_cast<std::size_t>(std::distance(headings.begin(), at)));
            if (is_null(text))
                continue;
            double figure = 0.0;
            if (!figure_of(text, figure)) {
                outcome.refusals.push_back(family.code + " line " + std::to_string(line_number) +
                                           " states " + measure + " as '" + std::string(text) +
                                           "', which is not a figure.");
                continue;
            }
            auto metric = base;
            metric.measure = measure;
            metric.value = figure;
            // The currency is the row's or the figure's own, and the heading
            // that names it is chosen from the ones the file states.
            const auto currency_heading = currency_of(headings, measure);
            if (!currency_heading.empty()) {
                const auto currency = field_at(static_cast<std::size_t>(std::distance(
                    headings.begin(), std::ranges::find(headings, std::string(currency_heading)))));
                if (!is_null(currency))
                    metric.currency = currency;
            }
            outcome.metrics.push_back(std::move(metric));
        }
    }

    return outcome;
}

metric_read_outcome read_metrics_for_file(std::string_view csv, std::string_view file_name) {
    const auto* family = family_of_file(file_name);
    if (family == nullptr) {
        metric_read_outcome outcome;
        outcome.refusals.push_back("no family states the file " + std::string(file_name) + ".");
        return outcome;
    }
    return read_metrics(csv, *family);
}

}

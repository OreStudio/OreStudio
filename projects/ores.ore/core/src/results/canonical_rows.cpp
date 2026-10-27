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
#include "ores.ore.core/results/canonical_rows.hpp"

#include <charconv>
#include <cstddef>
#include <sstream>
#include <string>
#include <vector>

namespace ores::ore::results {
namespace {

constexpr std::string_view subject_type_trade = "trade";
constexpr std::string_view measure_npv = "npv";
constexpr std::string_view measure_npv_base = "npv_base";
constexpr std::string_view measure_notional = "notional";

/** The engine's headings, which the projection reads by name. */
constexpr std::string_view heading_trade_id = "TradeId";
constexpr std::string_view heading_npv = "NPV";
constexpr std::string_view heading_npv_currency = "NpvCurrency";
constexpr std::string_view heading_npv_base = "NPV(Base)";
constexpr std::string_view heading_notional = "Notional";
constexpr std::string_view heading_notional_currency = "NotionalCurrency";
constexpr std::string_view heading_counterparty = "CounterParty";

/**
 * @brief The fields of one line, commas inside quotes kept.
 */
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

marshal_outcome marshal_npv_csv(std::string_view csv, std::string_view source_file) {
    marshal_outcome outcome;

    if (csv.substr(0, 3) == "\xEF\xBB\xBF")
        csv.remove_prefix(3);
    if (csv.empty()) {
        outcome.refusals.push_back(std::string(source_file) + " is empty.");
        return outcome;
    }

    const auto first_break = csv.find('\n');
    auto header_line = csv.substr(0, first_break);
    if (!header_line.empty() && header_line.front() == '#')
        header_line.remove_prefix(1);
    const auto header = split_fields(header_line);

    const auto at = [&header](std::string_view name) -> std::size_t {
        for (std::size_t i = 0; i < header.size(); ++i)
            if (header[i] == name)
                return i;
        return header.size();
    };
    const auto trade_id_at = at(heading_trade_id);
    const auto npv_at = at(heading_npv);
    const auto npv_currency_at = at(heading_npv_currency);
    const auto npv_base_at = at(heading_npv_base);
    const auto notional_at = at(heading_notional);
    const auto notional_currency_at = at(heading_notional_currency);
    const auto counterparty_at = at(heading_counterparty);

    if (trade_id_at == header.size() || npv_at == header.size()) {
        std::string named;
        for (const auto& field : header) {
            if (!named.empty())
                named += ", ";
            named += field;
        }
        outcome.refusals.push_back(std::string(source_file) +
                                   " states no TradeId and no NPV column; it states: " + named +
                                   ".");
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
        const auto field_at = [&fields](std::size_t index) -> std::string_view {
            return index < fields.size() ? std::string_view(fields[index]) : std::string_view();
        };
        const auto trade_id = field_at(trade_id_at);
        if (trade_id.empty()) {
            outcome.refusals.push_back(std::string(source_file) + " line " +
                                       std::to_string(line_number) + " states no trade id.");
            continue;
        }

        const auto add = [&](std::string_view measure,
                             std::size_t figure_index,
                             std::size_t currency_index,
                             bool needs_currency) {
            const auto figure_text = field_at(figure_index);
            double figure = 0.0;
            if (!figure_of(figure_text, figure)) {
                outcome.refusals.push_back(
                    std::string(source_file) + " line " + std::to_string(line_number) + " states " +
                    std::string(measure) + " as '" + std::string(figure_text) +
                    "', which is not a figure.");
                return;
            }
            const auto currency = field_at(currency_index);
            if (needs_currency && currency.empty()) {
                outcome.refusals.push_back(std::string(source_file) + " line " +
                                           std::to_string(line_number) + " states " +
                                           std::string(measure) + " with no currency.");
                return;
            }
            const auto counterparty =
                counterparty_at != header.size() ? field_at(counterparty_at) : std::string_view();
            outcome.rows.push_back(canonical_row{.subject_type = std::string(subject_type_trade),
                                                 .subject_id = std::string(trade_id),
                                                 .measure = std::string(measure),
                                                 .currency = std::string(currency),
                                                 .counterparty_id = std::string(counterparty),
                                                 .figure = figure,
                                                 .source = std::string(source_file)});
        };

        add(measure_npv, npv_at, npv_currency_at, true);
        if (npv_base_at != header.size())
            add(measure_npv_base, npv_base_at, npv_currency_at, true);
        if (notional_at != header.size())
            add(measure_notional, notional_at, notional_currency_at, false);
    }

    return outcome;
}

std::string canonical_csv(const std::vector<canonical_row>& rows) {
    const auto figure_text = [](double figure) {
        char buffer[64];
        const auto written = std::to_chars(buffer, buffer + sizeof buffer, figure);
        return std::string(buffer, written.ptr);
    };
    const auto quoted = [](std::string_view field) {
        if (field.find_first_of(",\"\n") == std::string_view::npos)
            return std::string(field);
        std::string out{"\""};
        for (const auto c : field) {
            if (c == '"')
                out.push_back('"');
            out.push_back(c);
        }
        out.push_back('"');
        return out;
    };

    std::ostringstream out;
    out << "subject_type,subject_id,measure,currency,counterparty_id,figure,source\n";
    for (const auto& row : rows) {
        out << quoted(row.subject_type) << ',' << quoted(row.subject_id) << ','
            << quoted(row.measure) << ',' << quoted(row.currency) << ','
            << quoted(row.counterparty_id) << ',' << figure_text(row.figure) << ','
            << quoted(row.source) << '\n';
    }
    return out.str();
}

}

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
#include "ores.ore.core/results/child_row_reader.hpp"
#include <algorithm>
#include <charconv>
#include <string>

namespace ores::ore::results {
namespace {

using child_row_fields = std::vector<std::string>;

bool child_is_null(std::string_view text) {
    return text.empty() || text == "#N/A" || text == "#NA" || text == "N/A" || text == "null" ||
           text == "NULL" || text == "NaN" || text == "-nan";
}

/* The helpers below are prefixed because a unity build puts the sources of one
 * target in one translation unit, where file-local names collide. */
child_row_fields child_fields_of(std::string_view line) {
    child_row_fields out;
    std::string field;
    bool quoted = false;
    for (const auto c : line) {
        if (c == '"') {
            quoted = !quoted;
        } else if (c == ',' && !quoted) {
            out.push_back(field);
            field.clear();
        } else if (c != '\r') {
            field.push_back(c);
        }
    }
    out.push_back(field);
    return out;
}

struct child_table {
    child_row_fields headings;
    std::string_view body;
};

bool child_table_of(std::string_view csv,
                    std::string_view file_name,
                    child_table& out,
                    std::vector<std::string>& refusals) {
    if (csv.substr(0, 3) == "\xEF\xBB\xBF")
        csv.remove_prefix(3);
    if (csv.empty()) {
        refusals.push_back(std::string(file_name) + " is empty.");
        return false;
    }
    const auto first_break = csv.find('\n');
    auto header = csv.substr(0, first_break);
    if (!header.empty() && header.front() == '#')
        header.remove_prefix(1);
    if (!header.empty() && header.back() == '\r')
        header.remove_suffix(1);
    out.headings = child_fields_of(header);
    out.body =
        first_break == std::string_view::npos ? std::string_view() : csv.substr(first_break + 1);
    return true;
}

std::optional<std::size_t> child_at_or_null(const child_row_fields& headings,
                                            std::string_view name) {
    const auto it = std::ranges::find(headings, name);
    if (it == headings.end())
        return std::nullopt;
    return static_cast<std::size_t>(std::distance(headings.begin(), it));
}

const std::string& child_cell(const child_row_fields& row, std::optional<std::size_t> index) {
    static const std::string empty;
    if (!index || *index >= row.size())
        return empty;
    return row[*index];
}

std::optional<double> child_figure_of_row(const child_row_fields& row,
                                          std::optional<std::size_t> index,
                                          std::string_view heading,
                                          std::size_t line_number,
                                          std::string_view file_name,
                                          std::vector<std::string>& refusals) {
    const auto& text = child_cell(row, index);
    if (child_is_null(text))
        return std::nullopt;
    double value = 0.0;
    const auto parsed = std::from_chars(text.data(), text.data() + text.size(), value);
    if (parsed.ec != std::errc() || parsed.ptr != text.data() + text.size()) {
        refusals.push_back(std::string(file_name) + " line " + std::to_string(line_number) +
                           " states " + std::string(heading) + " as '" + text +
                           "', which is not a figure.");
        return std::nullopt;
    }
    return value;
}

bool child_stated(const std::string& value) {
    return !child_is_null(value);
}

}

child_read_outcome<flow_row> read_flows(std::string_view csv, std::string_view file_name) {
    child_read_outcome<flow_row> outcome;
    child_table t;
    if (!child_table_of(csv, file_name, t, outcome.refusals))
        return outcome;

    const auto trade = child_at_or_null(t.headings, "TradeId");
    const auto leg = child_at_or_null(t.headings, "LegNo");
    const auto cashflow = child_at_or_null(t.headings, "CashflowNo");
    const auto pay_date = child_at_or_null(t.headings, "PayDate");
    if (!trade || !leg || !cashflow || !pay_date) {
        outcome.refusals.push_back(std::string(file_name) +
                                   " states no TradeId, LegNo, CashflowNo and PayDate.");
        return outcome;
    }

    std::size_t line_number = 1;
    std::size_t offset = 0;
    while (offset < t.body.size()) {
        ++line_number;
        const auto line_break = t.body.find('\n', offset);
        const auto line = t.body.substr(
            offset,
            line_break == std::string_view::npos ? t.body.size() - offset : line_break - offset);
        offset = line_break == std::string_view::npos ? t.body.size() : line_break + 1;
        if (line.find_first_not_of("\r \t") == std::string_view::npos)
            continue;

        const auto row = child_fields_of(line);
        flow_row flow;
        flow.source = std::string(file_name);
        flow.trade_id = child_cell(row, trade);
        flow.leg_no = child_cell(row, leg);
        flow.cashflow_no = child_cell(row, cashflow);
        flow.pay_date = child_cell(row, pay_date);
        if (!child_stated(flow.trade_id) || !child_stated(flow.leg_no) ||
            !child_stated(flow.cashflow_no) || !child_stated(flow.pay_date)) {
            outcome.refusals.push_back(std::string(file_name) + " line " +
                                       std::to_string(line_number) +
                                       " states no trade, leg, cashflow number or pay date.");
            continue;
        }
        flow.flow_type = child_cell(row, child_at_or_null(t.headings, "FlowType"));
        flow.currency = child_cell(row, child_at_or_null(t.headings, "Currency"));
        flow.amount = child_figure_of_row(row,
                                          child_at_or_null(t.headings, "Amount"),
                                          "Amount",
                                          line_number,
                                          file_name,
                                          outcome.refusals);
        flow.coupon = child_figure_of_row(row,
                                          child_at_or_null(t.headings, "Coupon"),
                                          "Coupon",
                                          line_number,
                                          file_name,
                                          outcome.refusals);
        flow.accrual = child_figure_of_row(row,
                                           child_at_or_null(t.headings, "Accrual"),
                                           "Accrual",
                                           line_number,
                                           file_name,
                                           outcome.refusals);
        flow.accrued_amount = child_figure_of_row(row,
                                                  child_at_or_null(t.headings, "AccruedAmount"),
                                                  "AccruedAmount",
                                                  line_number,
                                                  file_name,
                                                  outcome.refusals);
        flow.notional = child_figure_of_row(row,
                                            child_at_or_null(t.headings, "Notional"),
                                            "Notional",
                                            line_number,
                                            file_name,
                                            outcome.refusals);
        flow.discount_factor = child_figure_of_row(row,
                                                   child_at_or_null(t.headings, "DiscountFactor"),
                                                   "DiscountFactor",
                                                   line_number,
                                                   file_name,
                                                   outcome.refusals);
        flow.present_value = child_figure_of_row(row,
                                                 child_at_or_null(t.headings, "PresentValue"),
                                                 "PresentValue",
                                                 line_number,
                                                 file_name,
                                                 outcome.refusals);
        flow.present_value_base =
            child_figure_of_row(row,
                                child_at_or_null(t.headings, "PresentValue(Base)"),
                                "PresentValue(Base)",
                                line_number,
                                file_name,
                                outcome.refusals);
        outcome.rows.push_back(std::move(flow));
    }
    return outcome;
}

child_read_outcome<stressed_flow_row> read_stressed_flows(std::string_view csv,
                                                          std::string_view file_name) {
    child_read_outcome<stressed_flow_row> outcome;
    child_table t;
    if (!child_table_of(csv, file_name, t, outcome.refusals))
        return outcome;

    // A stressed figure needs its base and its scenario, or there is nothing to
    // compare. The difference is optional: the corpus states the floor and cap
    // volatilities with a base and a scenario and no difference.
    for (const auto& heading : t.headings) {
        const auto base = heading.find("_Base");
        if (base == std::string::npos)
            continue;
        const auto stem = heading.substr(0, base);
        if (std::ranges::find(t.headings, stem + "_Scen") == t.headings.end()) {
            outcome.refusals.push_back(std::string(file_name) + " states " + heading + " without " +
                                       stem + "_Scen.");
            return outcome;
        }
    }

    const auto trade = child_at_or_null(t.headings, "TradeId");
    const auto scenario = child_at_or_null(t.headings, "ScenarioLabel");
    const auto leg = child_at_or_null(t.headings, "LegNo");
    const auto cashflow = child_at_or_null(t.headings, "CashflowNo");
    const auto pay_date = child_at_or_null(t.headings, "PayDate");
    if (!trade || !scenario || !leg || !cashflow || !pay_date) {
        outcome.refusals.push_back(std::string(file_name) +
                                   " states no TradeId, ScenarioLabel, LegNo, CashflowNo and "
                                   "PayDate.");
        return outcome;
    }

    std::size_t line_number = 1;
    std::size_t offset = 0;
    while (offset < t.body.size()) {
        ++line_number;
        const auto line_break = t.body.find('\n', offset);
        const auto line = t.body.substr(
            offset,
            line_break == std::string_view::npos ? t.body.size() - offset : line_break - offset);
        offset = line_break == std::string_view::npos ? t.body.size() : line_break + 1;
        if (line.find_first_not_of("\r \t") == std::string_view::npos)
            continue;

        const auto row = child_fields_of(line);
        stressed_flow_row flow;
        flow.source = std::string(file_name);
        flow.trade_id = child_cell(row, trade);
        flow.scenario_label = child_cell(row, scenario);
        flow.leg_no = child_cell(row, leg);
        flow.cashflow_no = child_cell(row, cashflow);
        flow.pay_date = child_cell(row, pay_date);
        if (!child_stated(flow.trade_id) || !child_stated(flow.scenario_label)) {
            outcome.refusals.push_back(std::string(file_name) + " line " +
                                       std::to_string(line_number) +
                                       " states no trade or scenario.");
            continue;
        }
        flow.flow_type = child_cell(row, child_at_or_null(t.headings, "FlowType"));
        flow.currency = child_cell(row, child_at_or_null(t.headings, "Currency"));
        flow.amount_base = child_figure_of_row(row,
                                               child_at_or_null(t.headings, "Amount_Base"),
                                               "Amount_Base",
                                               line_number,
                                               file_name,
                                               outcome.refusals);
        flow.amount_scenario = child_figure_of_row(row,
                                                   child_at_or_null(t.headings, "Amount_Scen"),
                                                   "Amount_Scen",
                                                   line_number,
                                                   file_name,
                                                   outcome.refusals);
        flow.amount_difference = child_figure_of_row(row,
                                                     child_at_or_null(t.headings, "Amount_Diff"),
                                                     "Amount_Diff",
                                                     line_number,
                                                     file_name,
                                                     outcome.refusals);
        flow.present_value_base =
            child_figure_of_row(row,
                                child_at_or_null(t.headings, "PresentValue_Base"),
                                "PresentValue_Base",
                                line_number,
                                file_name,
                                outcome.refusals);
        flow.present_value_scenario =
            child_figure_of_row(row,
                                child_at_or_null(t.headings, "PresentValue_Scen"),
                                "PresentValue_Scen",
                                line_number,
                                file_name,
                                outcome.refusals);
        flow.present_value_difference =
            child_figure_of_row(row,
                                child_at_or_null(t.headings, "PresentValue_Diff"),
                                "PresentValue_Diff",
                                line_number,
                                file_name,
                                outcome.refusals);
        outcome.rows.push_back(std::move(flow));
    }
    return outcome;
}

child_read_outcome<calibration_flow_row> read_calibration_flows(std::string_view csv,
                                                                std::string_view file_name) {
    child_read_outcome<calibration_flow_row> outcome;
    child_table t;
    if (!child_table_of(csv, file_name, t, outcome.refusals))
        return outcome;

    const auto object_id = child_at_or_null(t.headings, "MarketObjectId");
    const auto cashflow = child_at_or_null(t.headings, "CashflowNo");
    const auto pay_date = child_at_or_null(t.headings, "PayDate");
    if (!object_id || !cashflow || !pay_date) {
        outcome.refusals.push_back(std::string(file_name) +
                                   " states no MarketObjectId, CashflowNo and PayDate.");
        return outcome;
    }

    std::size_t line_number = 1;
    std::size_t offset = 0;
    while (offset < t.body.size()) {
        ++line_number;
        const auto line_break = t.body.find('\n', offset);
        const auto line = t.body.substr(
            offset,
            line_break == std::string_view::npos ? t.body.size() - offset : line_break - offset);
        offset = line_break == std::string_view::npos ? t.body.size() : line_break + 1;
        if (line.find_first_not_of("\r \t") == std::string_view::npos)
            continue;

        const auto row = child_fields_of(line);
        calibration_flow_row flow;
        flow.source = std::string(file_name);
        flow.market_object_type = child_cell(row, child_at_or_null(t.headings, "MarketObjectType"));
        flow.market_object_id = child_cell(row, object_id);
        flow.pillar_date = child_cell(row, child_at_or_null(t.headings, "PillarDate"));
        flow.rate_helper_type = child_cell(row, child_at_or_null(t.headings, "RateHelperType"));
        flow.cashflow_no = child_cell(row, cashflow);
        flow.leg_no = child_cell(row, child_at_or_null(t.headings, "LegNo"));
        flow.pay_date = child_cell(row, pay_date);
        flow.flow_type = child_cell(row, child_at_or_null(t.headings, "FlowType"));
        flow.currency = child_cell(row, child_at_or_null(t.headings, "Currency"));
        if (!child_stated(flow.market_object_id) || !child_stated(flow.cashflow_no) ||
            !child_stated(flow.pay_date)) {
            outcome.refusals.push_back(std::string(file_name) + " line " +
                                       std::to_string(line_number) +
                                       " states no market object, cashflow number or pay date.");
            continue;
        }
        flow.amount = child_figure_of_row(row,
                                          child_at_or_null(t.headings, "Amount"),
                                          "Amount",
                                          line_number,
                                          file_name,
                                          outcome.refusals);
        flow.coupon = child_figure_of_row(row,
                                          child_at_or_null(t.headings, "Coupon"),
                                          "Coupon",
                                          line_number,
                                          file_name,
                                          outcome.refusals);
        flow.accrued_amount = child_figure_of_row(row,
                                                  child_at_or_null(t.headings, "AccruedAmount"),
                                                  "AccruedAmount",
                                                  line_number,
                                                  file_name,
                                                  outcome.refusals);
        flow.notional = child_figure_of_row(row,
                                            child_at_or_null(t.headings, "Notional"),
                                            "Notional",
                                            line_number,
                                            file_name,
                                            outcome.refusals);
        flow.discount_factor = child_figure_of_row(row,
                                                   child_at_or_null(t.headings, "DiscountFactor"),
                                                   "DiscountFactor",
                                                   line_number,
                                                   file_name,
                                                   outcome.refusals);
        flow.present_value = child_figure_of_row(row,
                                                 child_at_or_null(t.headings, "PresentValue"),
                                                 "PresentValue",
                                                 line_number,
                                                 file_name,
                                                 outcome.refusals);
        outcome.rows.push_back(std::move(flow));
    }
    return outcome;
}

}

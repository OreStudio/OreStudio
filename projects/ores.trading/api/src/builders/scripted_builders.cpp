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
#include "ores.trading.api/builders/scripted_builders.hpp"
#include "refusals.hpp"
#include <algorithm>
#include <format>
#include <iterator>
#include <utility>

namespace ores::trading::builders {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

namespace {

/// The rows of one scripted trade, filled variable by variable.
class scripted_rows final {
public:
    explicit scripted_rows(std::string_view trade_type, std::string script_name) {
        out_.instrument.identity.trade_type_code = std::string(trade_type);
        out_.instrument.script_name = std::move(script_name);
        out_.instrument.description = "Generated scripted trade.";
    }

    void value(std::string name, std::string type, std::string text) {
        auto& p = parameter(std::move(name), std::move(type), "scalar");
        p.value = std::move(text);
    }

    void list(std::string name, std::string type, const std::vector<std::string>& values) {
        const auto sequence = parameter(std::move(name), std::move(type), "vector").sequence_number;
        int member = 0;
        for (const auto& text : values) {
            domain::scripted_parameter_value row;
            row.parameter_sequence_number = sequence;
            row.sequence_number = ++member;
            row.value = text;
            out_.parameter_values.push_back(std::move(row));
        }
    }

    void schedule(std::string name, const scripted_schedule& dates) {
        const auto sequence = parameter(std::move(name), "Event", "schedule").sequence_number;
        domain::instrument_schedule row;
        row.owner_role = "scripted";
        row.owner_number = sequence;
        row.schedule_role = "event";
        row.sequence_number = 1;
        row.schedule_kind = "rules";
        row.start_date = dates.start;
        row.end_date = dates.end;
        row.tenor = dates.tenor;
        row.calendar = dates.calendar;
        row.convention = dates.convention;
        row.term_convention = dates.term_convention;
        row.rule = dates.rule;
        out_.schedules.push_back(std::move(row));
    }

    void dates(std::string name, const scripted_dates& listed) {
        const auto sequence = parameter(std::move(name), "Event", "schedule").sequence_number;
        domain::instrument_schedule row;
        row.owner_role = "scripted";
        row.owner_number = sequence;
        row.schedule_role = "event";
        row.sequence_number = 1;
        row.schedule_kind = "dates";
        row.calendar = listed.calendar;
        row.convention = listed.convention;
        out_.schedules.push_back(std::move(row));
        int number = 0;
        for (const auto& date : listed.dates) {
            domain::instrument_schedule_date date_row;
            date_row.owner_role = "scripted";
            date_row.owner_number = sequence;
            date_row.schedule_role = "event";
            date_row.schedule_sequence_number = 1;
            date_row.sequence_number = ++number;
            date_row.schedule_date = date;
            out_.schedule_dates.push_back(std::move(date_row));
        }
    }

    domain::instrument_underlying& underlying(std::string role,
                                              const marketdata::datum::market_reference& address,
                                              std::string type = "Equity") {
        int sequence = 0;
        for (const auto& row : out_.instrument_underlyings)
            if (row.underlying_role == role)
                ++sequence;
        domain::instrument_underlying row;
        row.underlying_role = std::move(role);
        row.sequence_number = sequence + 1;
        row.underlying_type = std::move(type);
        row.name = address.uri();
        out_.instrument_underlyings.push_back(std::move(row));
        return out_.instrument_underlyings.back();
    }

    /// A European OptionData block exercised on the one date.
    void option(domain::instrument_option row, const ymd& exercise) {
        out_.options.push_back(std::move(row));
        domain::instrument_schedule schedule;
        schedule.owner_role = "option";
        schedule.owner_number = 1;
        schedule.schedule_role = "exercise_dates";
        schedule.sequence_number = 1;
        schedule.schedule_kind = "dates";
        out_.schedules.push_back(std::move(schedule));
        domain::instrument_schedule_date date;
        date.owner_role = "option";
        date.owner_number = 1;
        date.schedule_role = "exercise_dates";
        date.schedule_sequence_number = 1;
        date.sequence_number = 1;
        date.schedule_date = exercise;
        out_.schedule_dates.push_back(std::move(date));
    }

    /// One BarrierData block with its one level, under the role.
    void barrier(const std::string& role, const scripted_barrier& terms) {
        int sequence = 0;
        for (const auto& row : out_.barriers)
            if (row.barrier_role == role)
                ++sequence;
        domain::instrument_barrier row;
        row.barrier_role = role;
        row.sequence_number = sequence + 1;
        row.barrier_type = terms.type;
        out_.barriers.push_back(std::move(row));
        domain::instrument_barrier_level level;
        level.barrier_role = role;
        level.barrier_sequence_number = sequence + 1;
        level.sequence_number = 1;
        level.level = terms.level;
        out_.barrier_levels.push_back(std::move(level));
    }

    /// A schedule of the product's own, stated as a rule every @p tenor.
    void rules_schedule(std::string role, const ymd& start, const ymd& end, std::string tenor) {
        domain::instrument_schedule row;
        row.owner_role = "option";
        row.owner_number = 1;
        row.schedule_role = std::move(role);
        row.sequence_number = 1;
        row.schedule_kind = "rules";
        row.start_date = start;
        row.end_date = end;
        row.tenor = std::move(tenor);
        row.calendar = "TARGET";
        row.convention = "F";
        row.term_convention = "F";
        row.rule = "Forward";
        out_.schedules.push_back(std::move(row));
    }

    /// A schedule of the product's own, derived from another by a shift.
    void derived_schedule(std::string role,
                          std::string base,
                          std::string shift,
                          std::string calendar,
                          std::string convention) {
        domain::instrument_schedule row;
        row.owner_role = "option";
        row.owner_number = 1;
        row.schedule_role = std::move(role);
        row.sequence_number = 1;
        row.schedule_kind = "derived";
        row.base_schedule = std::move(base);
        row.shift = std::move(shift);
        row.calendar = std::move(calendar);
        row.convention = std::move(convention);
        out_.schedules.push_back(std::move(row));
    }

    /// A schedule of the product's own, stated as a list of dates.
    void dated_schedule(std::string role,
                        const std::vector<ymd>& dates,
                        std::optional<std::string> calendar = std::nullopt,
                        std::optional<std::string> convention = std::nullopt) {
        domain::instrument_schedule schedule;
        schedule.owner_role = "option";
        schedule.owner_number = 1;
        schedule.schedule_role = role;
        schedule.sequence_number = 1;
        schedule.schedule_kind = "dates";
        schedule.calendar = std::move(calendar);
        schedule.convention = std::move(convention);
        out_.schedules.push_back(std::move(schedule));
        int sequence = 0;
        for (const auto& date : dates) {
            domain::instrument_schedule_date row;
            row.owner_role = "option";
            row.owner_number = 1;
            row.schedule_role = role;
            row.schedule_sequence_number = 1;
            row.sequence_number = ++sequence;
            row.schedule_date = date;
            out_.schedule_dates.push_back(std::move(row));
        }
    }

    domain::scripted_instrument_data take() {
        return std::move(out_);
    }

private:
    domain::scripted_parameter& parameter(std::string name, std::string type, std::string shape) {
        domain::scripted_parameter row;
        row.sequence_number = static_cast<int>(out_.parameters.size()) + 1;
        row.name = std::move(name);
        row.declared_type = std::move(type);
        row.shape = std::move(shape);
        out_.parameters.push_back(std::move(row));
        return out_.parameters.back();
    }

    domain::scripted_instrument_data out_;
};


}

scripted_trade_builder::scripted_trade_builder(scripted_trade_terms terms)
    : terms_(std::move(terms)) {}

scripted_result scripted_trade_builder::build() const {
    const auto& script = terms_.script_name.get();
    if (script.empty())
        return std::unexpected(build_error{"script_name", "must name a library script"});
    if (terms_.inputs.empty())
        return std::unexpected(build_error{
            "inputs", "the library holds no inputs for " + script + ", so none can be checked"});

    // Every input of the script is stated, of the type the script reads it as
    // and in the shape it reads: one value, or a list or schedule of them.
    for (const auto& input : terms_.inputs) {
        const auto stated =
            std::ranges::find(terms_.variables, input.input_name, &scripted_variable::name);
        if (stated == terms_.variables.end())
            return std::unexpected(build_error{input.input_name,
                                               "the script " + script + " reads it as a " +
                                                   input.data_type +
                                                   " and the trade does not state it"});
        const bool listed = !std::holds_alternative<std::string>(stated->value);
        if (stated->type != input.data_type || listed != input.is_array)
            return std::unexpected(build_error{input.input_name,
                                               "the script " + script + " reads it as a " +
                                                   input.data_type +
                                                   (input.is_array ? " list" : "") + ", not a " +
                                                   stated->type + (listed ? " list" : "")});
    }

    // The script reads only its inputs, each once, so a variable it does not
    // read, such as a misspelt name, is refused rather than booked unread.
    for (auto it = terms_.variables.begin(); it != terms_.variables.end(); ++it) {
        if (std::ranges::find(terms_.inputs, it->name, &domain::script_input::input_name) ==
            terms_.inputs.end())
            return std::unexpected(
                build_error{it->name, "the script " + script + " reads no such input"});
        if (std::ranges::find(
                std::next(it), terms_.variables.end(), it->name, &scripted_variable::name) !=
            terms_.variables.end())
            return std::unexpected(build_error{it->name, "is stated more than once"});
    }

    scripted_rows out(trade_type, script);
    for (const auto& variable : terms_.variables) {
        if (const auto* text = std::get_if<std::string>(&variable.value))
            out.value(variable.name, variable.type, *text);
        else if (const auto* values = std::get_if<std::vector<std::string>>(&variable.value))
            out.list(variable.name, variable.type, *values);
        else if (const auto* schedule = std::get_if<scripted_schedule>(&variable.value))
            out.schedule(variable.name, *schedule);
        else
            out.dates(variable.name, std::get<scripted_dates>(variable.value));
    }
    return out.take();
}


}

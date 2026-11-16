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
        row.calendar = "TARGET";
        row.convention = "F";
        row.rule = "Forward";
        out_.schedules.push_back(std::move(row));
    }

    void underlying(std::string role, const marketdata::datum::market_reference& address) {
        int sequence = 0;
        for (const auto& row : out_.instrument_underlyings)
            if (row.underlying_role == role)
                ++sequence;
        domain::instrument_underlying row;
        row.underlying_role = std::move(role);
        row.sequence_number = sequence + 1;
        row.underlying_type = "Equity";
        row.name = address.uri();
        out_.instrument_underlyings.push_back(std::move(row));
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

std::string scripted_date(const ymd& date) {
    return std::format("{:%Y-%m-%d}", date);
}

std::string scripted_text(const decimal& value) {
    return value.to_string();
}

std::vector<std::string> scripted_text(const std::vector<decimal>& values) {
    std::vector<std::string> out;
    for (const auto& value : values)
        out.push_back(value.to_string());
    return out;
}

std::string_view digital_kind_of(digital_kind kind) {
    switch (kind) {
        case digital_kind::call:
            return "Call";
        case digital_kind::put:
            return "Put";
        case digital_kind::collar:
            return "Collar";
    }
    return "Call";
}

std::optional<build_error> positive_amount(std::string term, const decimal& value) {
    if (value <= decimal{})
        return build_error{std::move(term), "must be positive"};
    return std::nullopt;
}

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

    scripted_rows out(trade_type, script);
    for (const auto& variable : terms_.variables) {
        if (const auto* text = std::get_if<std::string>(&variable.value))
            out.value(variable.name, variable.type, *text);
        else if (const auto* values = std::get_if<std::vector<std::string>>(&variable.value))
            out.list(variable.name, variable.type, *values);
        else
            out.schedule(variable.name, std::get<scripted_schedule>(variable.value));
    }
    return out.take();
}

double_digital_option_builder::double_digital_option_builder(double_digital_option_terms terms)
    : terms_(std::move(terms)) {}

scripted_result double_digital_option_builder::build() const {
    if (auto refusal = positive_amount("binary_payout", terms_.binary_payout.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.settlement.get() < terms_.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});
    if (terms_.pay_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("Expiry", "text", scripted_date(terms_.expiry.get()));
    out.value("Settlement", "text", scripted_date(terms_.settlement.get()));
    out.value("BinaryPayout", "decimal", scripted_text(terms_.binary_payout.get()));
    out.value("BinaryLevel1", "decimal", scripted_text(terms_.binary_level_1.get()));
    out.value("BinaryLevel2", "decimal", scripted_text(terms_.binary_level_2.get()));
    // ORE reads Type1 and Type2 as the option kinds, and refuses an asset class there.
    out.value("Type1", "text", std::string(digital_kind_of(terms_.kind_1.get())));
    out.value("Type2", "text", std::string(digital_kind_of(terms_.kind_2.get())));
    out.value("Position", "enumeration", long_short_of(terms_.position.get()));
    out.value("PayCcy", "enumeration", terms_.pay_currency.get());
    out.underlying("underlying1", terms_.underlying_1.get());
    out.underlying("underlying2", terms_.underlying_2.get());
    return out.take();
}

performance_option_builder::performance_option_builder(performance_option_terms terms)
    : terms_(std::move(terms)) {}

scripted_result performance_option_builder::build() const {
    if (auto refusal = positive_amount("notional", terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.settlement_date.get() < terms_.valuation_date.get())
        return std::unexpected(
            build_error{"settlement_date", "must not be before the valuation date"});
    if (terms_.strike_prices.get().empty())
        return std::unexpected(build_error{"strike_prices", "must state at least one strike"});
    if (terms_.underlyings.get().empty())
        return std::unexpected(build_error{"underlyings", "must name at least one underlying"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("NotionalAmount", "decimal", scripted_text(terms_.notional.get()));
    out.value("ParticipationRate", "decimal", scripted_text(terms_.participation_rate.get()));
    out.value("ValuationDate", "text", scripted_date(terms_.valuation_date.get()));
    out.value("SettlementDate", "text", scripted_date(terms_.settlement_date.get()));
    out.list("StrikePrices", "decimal", scripted_text(terms_.strike_prices.get()));
    out.value("Strike", "decimal", scripted_text(terms_.strike.get()));
    out.value("Position", "enumeration", long_short_of(terms_.position.get()));
    out.value("PayCcy", "enumeration", terms_.pay_currency.get());
    for (const auto& underlying : terms_.underlyings.get())
        out.underlying("underlying", underlying);
    return out.take();
}

autocallable_builder::autocallable_builder(autocallable_terms terms)
    : terms_(std::move(terms)) {}

scripted_result autocallable_builder::build() const {
    if (auto refusal = positive_amount("notional", terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = positive_amount("trigger_level", terms_.trigger_level.get()))
        return std::unexpected(std::move(*refusal));
    const auto& fixings = terms_.fixing_dates.get();
    const auto& settlements = terms_.settlement_dates.get();
    if (auto refusal = refusals::period(fixings.start, fixings.end))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = refusals::period(settlements.start, settlements.end))
        return std::unexpected(std::move(*refusal));
    if (terms_.accumulation_factors.get().empty())
        return std::unexpected(
            build_error{"accumulation_factors", "must state one factor for each fixing"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("NotionalAmount", "decimal", scripted_text(terms_.notional.get()));
    out.value("DeterminationLevel", "decimal", scripted_text(terms_.determination_level.get()));
    out.value("TriggerLevel", "decimal", scripted_text(terms_.trigger_level.get()));
    out.underlying("underlying", terms_.underlying.get());
    out.value("Position", "enumeration", long_short_of(terms_.position.get()));
    out.value("PayCcy", "enumeration", terms_.pay_currency.get());
    out.schedule("FixingDates", fixings);
    out.schedule("SettlementDates", settlements);
    out.list("AccumulationFactors", "decimal", scripted_text(terms_.accumulation_factors.get()));
    out.value("Cap", "decimal", scripted_text(terms_.cap.get()));
    return out.take();
}

}

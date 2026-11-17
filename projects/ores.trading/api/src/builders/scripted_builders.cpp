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
        row.calendar = "TARGET";
        row.convention = "F";
        row.rule = "Forward";
        out_.schedules.push_back(std::move(row));
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

    // A class-read product names no library script: ORE reads the product's own
    // class, so the row keeps the trade type as its script name.
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

scripted_result build_basket_option(std::string_view trade_type,
                                    std::string_view underlying_type,
                                    bool rainbow,
                                    const basket_option_terms& terms) {
    if (terms.currency.get().empty())
        return std::unexpected(build_error{"currency", "must name a currency"});
    if (auto refusal = positive_amount("notional", terms.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (terms.strike.get() < decimal{})
        return std::unexpected(build_error{"strike", "must not be negative"});
    const auto& members = terms.members.get();
    if (members.size() < 2)
        return std::unexpected(build_error{"members", "a basket names at least two underlyings"});
    for (std::size_t i = 0; i < members.size(); ++i) {
        if (auto refusal = positive_amount("weight", members[i].weight))
            return std::unexpected(std::move(*refusal));
        for (std::size_t j = 0; j < i; ++j)
            if (members[j].underlying.uri() == members[i].underlying.uri())
                return std::unexpected(build_error{"members", "names an underlying twice"});
    }
    const auto& payoff = terms.payoff_type.get();
    static constexpr std::string_view rainbow_payoffs[] = {
        "BestOfAssetOrCash", "WorstOfAssetOrCash", "MaxRainbow", "MinRainbow"};
    const bool known =
        rainbow ? std::ranges::find(rainbow_payoffs, payoff) != std::ranges::end(rainbow_payoffs) :
                  payoff == "Vanilla";
    if (!known)
        return std::unexpected(build_error{
            "payoff_type",
            rainbow ? "must be BestOfAssetOrCash, WorstOfAssetOrCash, MaxRainbow or MinRainbow" :
                      "must be Vanilla"});
    if (terms.settlement.get() < terms.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("Currency", "enumeration", terms.currency.get());
    out.value("Notional", "decimal", scripted_text(terms.notional.get()));
    out.value("Strike", "decimal", scripted_text(terms.strike.get()));
    out.value("Settlement", "text", scripted_date(terms.settlement.get()));
    for (const auto& member : members) {
        auto& row = out.underlying("underlying", member.underlying, std::string(underlying_type));
        row.currency = member.currency;
        row.weight = member.weight.to_double();
    }
    domain::instrument_option option;
    option.long_short = long_short_of(terms.position.get());
    option.payoff_type = payoff;
    // An asset-or-cash payoff pays the best or worst member, so it has no call or put.
    if (payoff == "Vanilla" || payoff == "MaxRainbow" || payoff == "MinRainbow")
        option.option_type = option_type_of(terms.kind.get());
    out.option(std::move(option), terms.expiry.get());
    return out.take();
}

namespace {

/// Refuses a list of dates that is empty, out of order, on or before @p after, or after @p until.
std::optional<build_error> dates_within(std::string term,
                                        const std::vector<ymd>& dates,
                                        std::optional<ymd> after,
                                        const ymd& until) {
    if (dates.empty())
        return build_error{std::move(term), "must name at least one date"};
    for (std::size_t i = 0; i < dates.size(); ++i) {
        if ((after && dates[i] <= *after) || dates[i] > until)
            return build_error{std::move(term), "must fall after the start and by the expiry"};
        if (i > 0 && dates[i] <= dates[i - 1])
            return build_error{std::move(term), "must be in increasing order"};
    }
    return std::nullopt;
}

}

scripted_result build_best_entry_option(std::string_view trade_type,
                                        std::string_view underlying_type,
                                        const best_entry_option_terms& terms) {
    if (terms.currency.get().empty())
        return std::unexpected(build_error{"currency", "must name a currency"});
    for (const auto& [term, value] : {std::pair{"notional", &terms.notional.get()},
                                      std::pair{"multiplier", &terms.multiplier.get()},
                                      std::pair{"strike", &terms.strike.get()},
                                      std::pair{"trigger_level", &terms.trigger_level.get()}})
        if (auto refusal = positive_amount(term, *value))
            return std::unexpected(std::move(*refusal));
    for (const auto& [term, value] : {std::pair{"cap", &terms.cap.get()},
                                      std::pair{"reset_minimum", &terms.reset_minimum.get()},
                                      std::pair{"premium", &terms.premium.get()}})
        if (*value < decimal{})
            return std::unexpected(build_error{term, "must not be negative"});
    if (auto refusal = dates_within("strike_observation_dates",
                                    terms.strike_observation_dates.get(),
                                    terms.strike_date.get(),
                                    terms.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms.settlement.get() < terms.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("LongShort", "enumeration", long_short_of(terms.position.get()));
    out.value("Currency", "enumeration", terms.currency.get());
    out.value("Notional", "decimal", scripted_text(terms.notional.get()));
    out.value("Multiplier", "decimal", scripted_text(terms.multiplier.get()));
    out.value("Cap", "decimal", scripted_text(terms.cap.get()));
    out.value("Strike", "decimal", scripted_text(terms.strike.get()));
    out.value("TriggerLevel", "decimal", scripted_text(terms.trigger_level.get()));
    out.value("ResetMinimum", "decimal", scripted_text(terms.reset_minimum.get()));
    out.value("Premium", "decimal", scripted_text(terms.premium.get()));
    out.value("ExpiryDate", "text", scripted_date(terms.expiry.get()));
    out.value("StrikeDate", "text", scripted_date(terms.strike_date.get()));
    out.value("PremiumDate", "text", scripted_date(terms.premium_date.get()));
    out.value("SettlementDate", "text", scripted_date(terms.settlement.get()));
    out.underlying("underlying", terms.underlying.get(), std::string(underlying_type));
    out.dated_schedule("strikeobservationdates", terms.strike_observation_dates.get());
    return out.take();
}

scripted_result build_strike_resettable_option(std::string_view trade_type,
                                               std::string_view underlying_type,
                                               const strike_resettable_option_terms& terms) {
    if (terms.currency.get().empty())
        return std::unexpected(build_error{"currency", "must name a currency"});
    for (const auto& [term, value] : {std::pair{"quantity", &terms.quantity.get()},
                                      std::pair{"strike", &terms.strike.get()},
                                      std::pair{"reset_strike", &terms.reset_strike.get()},
                                      std::pair{"trigger_price", &terms.trigger_price.get()}})
        if (auto refusal = positive_amount(term, *value))
            return std::unexpected(std::move(*refusal));
    if (terms.premium.get() < decimal{})
        return std::unexpected(build_error{"premium", "must not be negative"});
    const auto& dates = terms.observation_dates.get();
    // The element states no start date, so an observation may fall at any time before expiry.
    if (auto refusal = dates_within("observation_dates", dates, std::nullopt, terms.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms.settlement.get() < terms.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("LongShort", "enumeration", long_short_of(terms.position.get()));
    out.value("OptionType", "enumeration", option_type_of(terms.kind.get()));
    out.value("Currency", "enumeration", terms.currency.get());
    out.value("Strike", "decimal", scripted_text(terms.strike.get()));
    out.value("ResetStrike", "decimal", scripted_text(terms.reset_strike.get()));
    out.value("Quantity", "decimal", scripted_text(terms.quantity.get()));
    out.value("TriggerType", "enumeration", trigger_type_of(terms.trigger.get()));
    out.value("TriggerPrice", "decimal", scripted_text(terms.trigger_price.get()));
    out.value("ExpiryDate", "text", scripted_date(terms.expiry.get()));
    out.value("SettlementDate", "text", scripted_date(terms.settlement.get()));
    out.value("Premium", "decimal", scripted_text(terms.premium.get()));
    out.value("PremiumDate", "text", scripted_date(terms.premium_date.get()));
    out.underlying("underlying", terms.underlying.get(), std::string(underlying_type));
    out.dated_schedule("observationdates", dates);
    return out.take();
}

namespace {

/// Refuses a barrier whose type ORE does not read or whose level is not positive.
std::optional<build_error> barrier_refusal(const scripted_barrier& barrier) {
    static constexpr std::string_view types[] = {"UpAndOut", "UpAndIn", "DownAndOut", "DownAndIn"};
    if (std::ranges::find(types, barrier.type) == std::ranges::end(types))
        return build_error{"barrier", "must be UpAndOut, UpAndIn, DownAndOut or DownAndIn"};
    return positive_amount("barrier", barrier.level);
}

}

scripted_result build_window_barrier_option(std::string_view trade_type,
                                            std::string_view underlying_type,
                                            const window_barrier_option_terms& terms) {
    if (terms.currency.get().empty())
        return std::unexpected(build_error{"currency", "must name a currency"});
    if (auto refusal = positive_amount("fixing_amount", terms.fixing_amount.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = positive_amount("strike", terms.strike.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = barrier_refusal(terms.barrier.get()))
        return std::unexpected(std::move(*refusal));
    if (!(terms.window_start.get() < terms.window_end.get()) ||
        terms.expiry.get() < terms.window_end.get())
        return std::unexpected(
            build_error{"window_end", "must be after the window start and by the expiry"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("Currency", "enumeration", terms.currency.get());
    out.value("FixingAmount", "decimal", scripted_text(terms.fixing_amount.get()));
    out.value("Strike", "text", scripted_text(terms.strike.get()));
    out.value("StartDate", "text", scripted_date(terms.window_start.get()));
    out.value("EndDate", "text", scripted_date(terms.window_end.get()));
    out.underlying("underlying", terms.underlying.get(), std::string(underlying_type));
    domain::instrument_option option;
    option.long_short = long_short_of(terms.position.get());
    option.option_type = option_type_of(terms.kind.get());
    out.option(std::move(option), terms.expiry.get());
    out.barrier("barrier", terms.barrier.get());
    return out.take();
}

equity_generic_barrier_option_builder::equity_generic_barrier_option_builder(
    generic_barrier_option_terms terms)
    : terms_(std::move(terms)) {}

scripted_result equity_generic_barrier_option_builder::build() const {
    const auto& terms = terms_;
    if (terms.pay_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});
    if (auto refusal = positive_amount("quantity", terms.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = positive_amount("strike", terms.strike.get()))
        return std::unexpected(std::move(*refusal));
    const auto& barriers = terms.barriers.get();
    if (barriers.empty())
        return std::unexpected(build_error{"barriers", "must name at least one barrier"});
    for (const auto& barrier : barriers)
        if (auto refusal = barrier_refusal(barrier))
            return std::unexpected(std::move(*refusal));
    if (terms.kiko_type) {
        static constexpr std::string_view kikos[] = {"KoAlways", "KoBeforeKi", "KoAfterKi"};
        if (std::ranges::find(kikos, *terms.kiko_type) == std::ranges::end(kikos))
            return std::unexpected(
                build_error{"kiko_type", "must be KoAlways, KoBeforeKi or KoAfterKi"});
    }
    if (!(terms.monitoring_start.get() < terms.expiry.get()))
        return std::unexpected(build_error{"monitoring_start", "must be before the expiry"});
    if (terms.settlement.get() < terms.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("PayCurrency", "enumeration", terms.pay_currency.get());
    out.value("Quantity", "decimal", scripted_text(terms.quantity.get()));
    out.value("Strike", "decimal", scripted_text(terms.strike.get()));
    out.value("SettlementDate", "text", scripted_date(terms.settlement.get()));
    if (terms.kiko_type)
        out.value("Barriers.KikoType", "enumeration", *terms.kiko_type);
    out.underlying("underlying", terms.underlying.get(), "Equity");
    domain::instrument_option option;
    option.long_short = long_short_of(terms.position.get());
    option.payoff_type = "Vanilla";
    option.option_type = option_type_of(terms.kind.get());
    out.option(std::move(option), terms.expiry.get());
    out.rules_schedule("barrier_schedule", terms.monitoring_start.get(), terms.expiry.get(), "1D");
    for (const auto& barrier : barriers)
        out.barrier("barrier", barrier);
    return out.take();
}

scripted_result build_basket_variance_swap(std::string_view trade_type,
                                           std::string_view underlying_type,
                                           const basket_variance_swap_terms& terms) {
    if (terms.currency.get().empty())
        return std::unexpected(build_error{"currency", "must name a currency"});
    if (auto refusal = positive_amount("notional", terms.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = positive_amount("strike", terms.strike.get()))
        return std::unexpected(std::move(*refusal));
    const auto& members = terms.members.get();
    if (members.size() < 2)
        return std::unexpected(build_error{"members", "a basket names at least two underlyings"});
    for (std::size_t i = 0; i < members.size(); ++i) {
        if (auto refusal = positive_amount("weight", members[i].weight))
            return std::unexpected(std::move(*refusal));
        for (std::size_t j = 0; j < i; ++j)
            if (members[j].underlying.uri() == members[i].underlying.uri())
                return std::unexpected(build_error{"members", "names an underlying twice"});
    }
    if (!(terms.start.get() < terms.end.get()))
        return std::unexpected(build_error{"end", "must be after the start"});
    if (terms.settlement.get() < terms.end.get())
        return std::unexpected(build_error{"settlement", "must not be before the end"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("LongShort", "enumeration", long_short_of(terms.position.get()));
    out.value("Strike", "decimal", scripted_text(terms.strike.get()));
    out.value("Notional", "decimal", scripted_text(terms.notional.get()));
    out.value("SquaredPayoff", "text", terms.squared_payoff.get() ? "true" : "false");
    out.value("SettlementDate", "text", scripted_date(terms.settlement.get()));
    out.value("Currency", "enumeration", terms.currency.get());
    for (const auto& member : members) {
        auto& row = out.underlying("underlying", member.underlying, std::string(underlying_type));
        row.currency = member.currency;
        row.weight = member.weight.to_double();
    }
    out.rules_schedule("valuationschedule", terms.start.get(), terms.end.get(), "1D");
    return out.take();
}

scripted_result
build_forward_volatility_agreement(std::string_view trade_type,
                                   std::string_view underlying_type,
                                   const forward_volatility_agreement_terms& terms) {
    if (terms.pay_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});
    if (auto refusal = positive_amount("quantity", terms.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = positive_amount("implied_vol_strike", terms.implied_vol_strike.get()))
        return std::unexpected(std::move(*refusal));
    if (!(terms.fva_date.get() < terms.option_expiry.get()))
        return std::unexpected(build_error{"option_expiry", "must be after the FVA date"});
    if (terms.settlement.get() < terms.option_expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the option expiry"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("FvaDate", "text", scripted_date(terms.fva_date.get()));
    out.value("OptionExpiry", "text", scripted_date(terms.option_expiry.get()));
    out.value("PremiumDate", "text", scripted_date(terms.premium_date.get()));
    out.value("LongShort", "enumeration", long_short_of(terms.position.get()));
    out.value("ImpliedVolStrike", "decimal", scripted_text(terms.implied_vol_strike.get()));
    out.value("Quantity", "decimal", scripted_text(terms.quantity.get()));
    out.value("PayCcy", "enumeration", terms.pay_currency.get());
    out.value("SettlementDate", "text", scripted_date(terms.settlement.get()));
    out.underlying("underlying", terms.underlying.get(), std::string(underlying_type));
    return out.take();
}

european_option_barrier_builder::european_option_barrier_builder(
    european_option_barrier_terms terms)
    : terms_(std::move(terms)) {}

scripted_result european_option_barrier_builder::build() const {
    const auto& terms = terms_;
    if (terms.pay_currency.get().empty() || terms.premium_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});
    if (auto refusal = positive_amount("quantity", terms.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = positive_amount("strike", terms.strike.get()))
        return std::unexpected(std::move(*refusal));
    if (terms.premium_amount.get() < decimal{})
        return std::unexpected(build_error{"premium_amount", "must not be negative"});
    if (auto refusal = barrier_refusal(terms.barrier.get()))
        return std::unexpected(std::move(*refusal));
    if (terms.settlement.get() < terms.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});

    const std::string underlying_type =
        terms.family.get() == scripted_family::equity ? "Equity" : "FX";
    scripted_rows out(trade_type, std::string(trade_type));
    out.value("Quantity", "decimal", scripted_text(terms.quantity.get()));
    out.value("PutCall", "text", option_type_of(terms.kind.get()));
    out.value("LongShort", "enumeration", long_short_of(terms.position.get()));
    out.value("Strike", "decimal", scripted_text(terms.strike.get()));
    out.value("PremiumAmount", "decimal", scripted_text(terms.premium_amount.get()));
    out.value("PremiumCurrency", "enumeration", terms.premium_currency.get());
    out.value("PremiumDate", "text", scripted_date(terms.premium_date.get()));
    out.value("OptionExpiry", "text", scripted_date(terms.expiry.get()));
    out.value("BarrierLevel", "decimal", scripted_text(terms.barrier.get().level));
    out.value("BarrierType", "text", terms.barrier.get().type);
    out.value("BarrierStyle", "text", "European");
    out.value("SettlementDate", "text", scripted_date(terms.settlement.get()));
    out.value("PayCcy", "enumeration", terms.pay_currency.get());
    out.underlying("optionunderlying", terms.option_underlying.get(), underlying_type);
    out.underlying("barrierunderlying", terms.barrier_underlying.get(), underlying_type);
    return out.take();
}

fx_worst_of_basket_swap_builder::fx_worst_of_basket_swap_builder(
    fx_worst_of_basket_swap_terms terms)
    : terms_(std::move(terms)) {}

scripted_result fx_worst_of_basket_swap_builder::build() const {
    const auto& terms = terms_;
    if (terms.currency.get().empty())
        return std::unexpected(build_error{"currency", "must name a currency"});
    if (terms.floating_index.get().empty())
        return std::unexpected(build_error{"floating_index", "must name an index"});
    if (auto refusal = positive_amount("quantity", terms.quantity.get()))
        return std::unexpected(std::move(*refusal));
    const auto& members = terms.members.get();
    if (members.size() < 2)
        return std::unexpected(build_error{"members", "a basket names at least two pairs"});
    for (std::size_t i = 0; i < members.size(); ++i) {
        if (auto refusal = positive_amount("initial_price", members[i].initial_price))
            return std::unexpected(std::move(*refusal));
        for (std::size_t j = 0; j < i; ++j)
            if (members[j].underlying.uri() == members[i].underlying.uri())
                return std::unexpected(build_error{"members", "names a pair twice"});
    }
    const auto& periods = terms.period_dates.get();
    if (periods.size() < 2)
        return std::unexpected(build_error{"period_dates", "must bound at least one period"});
    for (std::size_t i = 1; i < periods.size(); ++i)
        if (!(periods[i - 1] < periods[i]))
            return std::unexpected(build_error{"period_dates", "must be in increasing order"});
    const auto count = periods.size() - 1;
    const auto& pays = terms.pay_dates.get();
    if (pays.size() != count)
        return std::unexpected(build_error{"pay_dates", "must state one date a period"});
    for (std::size_t i = 0; i < count; ++i)
        if (pays[i] < periods[i + 1])
            return std::unexpected(build_error{"pay_dates", "must not be before the period ends"});
    if (terms.knock_out_levels.get().size() != count ||
        terms.fixed_trigger_levels.get().size() != count)
        return std::unexpected(
            build_error{"knock_out_levels", "the level lists must state one level a period"});

    scripted_rows out(trade_type, std::string(trade_type));
    out.value("LongShort", "enumeration", long_short_of(terms.position.get()));
    out.value("Currency", "enumeration", terms.currency.get());
    out.value("Quantity", "decimal", scripted_text(terms.quantity.get()));
    out.value("InitialFixedRate", "decimal", scripted_text(terms.initial_fixed_rate.get()));
    std::vector<decimal> initial_prices;
    for (const auto& member : members)
        initial_prices.push_back(member.initial_price);
    out.list("InitialPrices", "decimal", scripted_text(initial_prices));
    out.list("KnockOutLevels", "decimal", scripted_text(terms.knock_out_levels.get()));
    out.list("FixedTriggerLevels", "decimal", scripted_text(terms.fixed_trigger_levels.get()));
    out.value("FixedRate", "decimal", scripted_text(terms.fixed_rate.get()));
    out.value("BermudanKnockIn", "text", "false");
    out.value("KnockInLevel", "decimal", scripted_text(terms.knock_in_level.get()));
    out.value("AccumulatingFixedCoupons", "text", "false");
    out.value("AccruingFixedCoupons", "text", "false");
    out.value("FloatingIndex", "text", terms.floating_index.get());
    out.value("FloatingSpread", "decimal", scripted_text(terms.floating_spread.get()));
    out.value("FloatingDayCountFraction", "text", terms.floating_day_count.get());
    out.value("Strike", "decimal", scripted_text(terms.strike.get()));
    for (const auto& member : members)
        out.underlying("underlying", member.underlying, "FX");
    out.dated_schedule("floatingperiodschedule", periods, "TARGET", "MF");
    out.dated_schedule("floatingpaydates", pays, "TARGET", "MF");
    out.derived_schedule("floatingfixingschedule", "FloatingPeriodSchedule", "-2D", "TARGET", "P");
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
        return std::unexpected(build_error{
            "accumulation_factors",
            "must state at least one factor; ORE reads one for each fixing, which a rules "
            "schedule states only once the calendar rolls it, so the caller matches the count"});

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

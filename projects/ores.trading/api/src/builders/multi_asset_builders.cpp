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
#include "ores.trading.api/builders/multi_asset_builders.hpp"
#include "refusals.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

std::optional<build_error> multi_asset_positive(std::string term, const decimal& value) {
    if (value <= decimal{})
        return build_error{std::move(term), "must be positive"};
    return std::nullopt;
}

std::string_view digital_kind_text(digital_kind kind) {
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

void add_underlying(domain::multi_asset_instrument_data& out,
                    std::string_view role,
                    const marketdata::datum::market_reference& address,
                    std::string_view type) {
    int sequence = 0;
    for (const auto& row : out.instrument_underlyings)
        if (row.underlying_role == role)
            ++sequence;
    domain::instrument_underlying row{};
    row.underlying_role = std::string(role);
    row.sequence_number = sequence + 1;
    row.underlying_type = std::string(type);
    row.name = address.uri();
    out.instrument_underlyings.push_back(std::move(row));
}

void add_values(domain::multi_asset_instrument_data& out,
                std::string_view role,
                const std::vector<decimal>& values) {
    int sequence = 0;
    for (const auto& value : values) {
        domain::instrument_value row{};
        row.value_role = std::string(role);
        row.sequence_number = ++sequence;
        row.value = value;
        out.values.push_back(std::move(row));
    }
}

/// A schedule stated as a rule: one date a tenor from the start to the end.
void add_rules_schedule(domain::multi_asset_instrument_data& out,
                        std::string_view role,
                        const scripted_schedule& dates) {
    domain::instrument_schedule row{};
    row.owner_role = "option";
    row.owner_number = 1;
    row.schedule_role = std::string(role);
    row.sequence_number = 1;
    row.schedule_kind = "rules";
    row.start_date = dates.start;
    row.end_date = dates.end;
    row.tenor = dates.tenor;
    row.calendar = "TARGET";
    row.convention = "F";
    row.rule = "Forward";
    out.schedules.push_back(std::move(row));
}

}

double_digital_option_builder::double_digital_option_builder(double_digital_option_terms terms)
    : terms_(std::move(terms)) {}

multi_asset_result double_digital_option_builder::build() const {
    if (auto refusal = multi_asset_positive("binary_payout", terms_.binary_payout.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.settlement.get() < terms_.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});
    if (terms_.pay_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});

    domain::multi_asset_instrument_data out{};
    domain::double_digital_option_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.expiry = terms_.expiry.get();
    row.settlement = terms_.settlement.get();
    row.binary_payout = terms_.binary_payout.get();
    row.binary_level_1 = terms_.binary_level_1.get();
    row.binary_level_2 = terms_.binary_level_2.get();
    // ORE reads Type1 and Type2 as the option kinds, and refuses an asset class there.
    row.type_1 = std::string(digital_kind_text(terms_.kind_1.get()));
    row.type_2 = std::string(digital_kind_text(terms_.kind_2.get()));
    row.long_short = long_short_of(terms_.position.get());
    row.pay_currency = terms_.pay_currency.get();
    out.double_digital_options.push_back(std::move(row));
    add_underlying(out, "underlying1", terms_.underlying_1.get(), "Equity");
    add_underlying(out, "underlying2", terms_.underlying_2.get(), "Equity");
    return out;
}

performance_option_builder::performance_option_builder(performance_option_terms terms)
    : terms_(std::move(terms)) {}

multi_asset_result performance_option_builder::build() const {
    if (auto refusal = multi_asset_positive("notional", terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.settlement_date.get() < terms_.valuation_date.get())
        return std::unexpected(
            build_error{"settlement_date", "must not be before the valuation date"});
    if (terms_.strike_prices.get().empty())
        return std::unexpected(build_error{"strike_prices", "must state at least one strike"});
    if (terms_.underlyings.get().empty())
        return std::unexpected(build_error{"underlyings", "must name at least one underlying"});
    if (terms_.pay_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});

    domain::multi_asset_instrument_data out{};
    domain::performance_option_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.notional_amount = terms_.notional.get();
    row.participation_rate = terms_.participation_rate.get().to_double();
    row.valuation_date = terms_.valuation_date.get();
    row.settlement_date = terms_.settlement_date.get();
    row.strike = terms_.strike.get();
    row.long_short = long_short_of(terms_.position.get());
    row.pay_currency = terms_.pay_currency.get();
    out.performance_options.push_back(std::move(row));
    for (const auto& underlying : terms_.underlyings.get())
        add_underlying(out, "underlying", underlying, "Equity");
    add_values(out, "strike_price", terms_.strike_prices.get());
    return out;
}

european_option_barrier_builder::european_option_barrier_builder(
    european_option_barrier_terms terms)
    : terms_(std::move(terms)) {}

multi_asset_result european_option_barrier_builder::build() const {
    const auto& terms = terms_;
    if (terms.pay_currency.get().empty() || terms.premium_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});
    if (auto refusal = multi_asset_positive("quantity", terms.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = multi_asset_positive("strike", terms.strike.get()))
        return std::unexpected(std::move(*refusal));
    if (terms.premium_amount.get() < decimal{})
        return std::unexpected(build_error{"premium_amount", "must not be negative"});
    static constexpr std::string_view types[] = {"UpAndOut", "UpAndIn", "DownAndOut", "DownAndIn"};
    const auto& barrier = terms.barrier.get();
    if (std::ranges::find(types, barrier.type) == std::ranges::end(types))
        return std::unexpected(
            build_error{"barrier", "must be UpAndOut, UpAndIn, DownAndOut or DownAndIn"});
    if (auto refusal = multi_asset_positive("barrier", barrier.level))
        return std::unexpected(std::move(*refusal));
    if (terms.settlement.get() < terms.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});

    const std::string_view underlying_type =
        terms.family.get() == scripted_family::equity ? "Equity" : "FX";
    domain::multi_asset_instrument_data out{};
    domain::european_option_barrier_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.quantity = terms.quantity.get().to_double();
    row.option_type = option_type_of(terms.kind.get());
    row.long_short = long_short_of(terms.position.get());
    row.strike = terms.strike.get();
    row.premium_amount = terms.premium_amount.get();
    row.premium_currency = terms.premium_currency.get();
    row.premium_date = terms.premium_date.get();
    row.option_expiry = terms.expiry.get();
    row.barrier_level = barrier.level;
    row.barrier_type = barrier.type;
    row.barrier_style = "European";
    row.settlement_date = terms.settlement.get();
    row.pay_currency = terms.pay_currency.get();
    out.european_option_barriers.push_back(std::move(row));
    add_underlying(out, "optionunderlying", terms.option_underlying.get(), underlying_type);
    add_underlying(out, "barrierunderlying", terms.barrier_underlying.get(), underlying_type);
    return out;
}

autocallable_builder::autocallable_builder(autocallable_terms terms)
    : terms_(std::move(terms)) {}

multi_asset_result autocallable_builder::build() const {
    if (auto refusal = multi_asset_positive("notional", terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = multi_asset_positive("trigger_level", terms_.trigger_level.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.pay_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});
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

    domain::multi_asset_instrument_data out{};
    domain::autocallable_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.notional_amount = terms_.notional.get();
    row.determination_level = terms_.determination_level.get();
    row.trigger_level = terms_.trigger_level.get();
    row.long_short = long_short_of(terms_.position.get());
    row.pay_currency = terms_.pay_currency.get();
    row.cap = terms_.cap.get();
    out.autocallables.push_back(std::move(row));
    add_underlying(out, "underlying", terms_.underlying.get(), "Equity");
    add_rules_schedule(out, "fixing_schedule", fixings);
    add_rules_schedule(out, "settlement_schedule", settlements);
    add_values(out, "accumulation_factor", terms_.accumulation_factors.get());
    return out;
}

}

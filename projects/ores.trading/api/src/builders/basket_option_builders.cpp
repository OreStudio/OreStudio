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
#include "ores.trading.api/builders/basket_option_builders.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;

std::optional<build_error> refusal_of(bool rainbow, const basket_option_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must name a currency"};
    if (!(terms.notional.get() > decimal{}))
        return build_error{"notional", "must be positive"};
    if (terms.strike.get() < decimal{})
        return build_error{"strike", "must not be negative"};
    const auto& members = terms.members.get();
    if (members.size() < 2)
        return build_error{"members", "a basket names at least two underlyings"};
    for (std::size_t i = 0; i < members.size(); ++i) {
        if (!(members[i].weight > decimal{}))
            return build_error{"weight", "must be positive"};
        for (std::size_t j = 0; j < i; ++j)
            if (members[j].underlying.uri() == members[i].underlying.uri())
                return build_error{"members", "names an underlying twice"};
    }
    static constexpr std::string_view rainbow_payoffs[] = {
        "BestOfAssetOrCash", "WorstOfAssetOrCash", "MaxRainbow", "MinRainbow"};
    const auto& payoff = terms.payoff_type.get();
    const bool known =
        rainbow ? std::ranges::find(rainbow_payoffs, payoff) != std::ranges::end(rainbow_payoffs) :
                  payoff == "Vanilla";
    if (!known)
        return build_error{
            "payoff_type",
            rainbow ? "must be BestOfAssetOrCash, WorstOfAssetOrCash, MaxRainbow or MinRainbow" :
                      "must be Vanilla"};
    if (terms.settlement.get() < terms.expiry.get())
        return build_error{"settlement", "must not be before the expiry"};
    return std::nullopt;
}

template <typename Row>
Row row_of(std::string_view trade_type, const basket_option_terms& terms) {
    Row row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.currency = terms.currency.get();
    row.notional = terms.notional.get();
    row.strike = terms.strike.get();
    row.settlement_date = terms.settlement.get();
    return row;
}

/// The members, the OptionData block and its one exercise date, as the carrier's shared rows.
template <typename Data>
void add_shared_rows(Data& out,
                     std::string_view underlying_type,
                     const basket_option_terms& terms) {
    int sequence = 0;
    for (const auto& member : terms.members.get()) {
        domain::instrument_underlying row{};
        row.underlying_role = "underlying";
        row.sequence_number = ++sequence;
        row.underlying_type = std::string(underlying_type);
        row.name = member.underlying.uri();
        row.currency = member.currency;
        row.weight = member.weight.to_double();
        out.instrument_underlyings.push_back(std::move(row));
    }
    domain::instrument_option option{};
    option.long_short = long_short_of(terms.position.get());
    option.payoff_type = terms.payoff_type.get();
    // An asset-or-cash payoff pays the best or worst member, so it has no call or put.
    const auto& payoff = terms.payoff_type.get();
    if (payoff == "Vanilla" || payoff == "MaxRainbow" || payoff == "MinRainbow")
        option.option_type = option_type_of(terms.kind.get());
    out.options.push_back(std::move(option));
    domain::instrument_schedule schedule{};
    schedule.owner_role = "option";
    schedule.owner_number = 1;
    schedule.schedule_role = "exercise_dates";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "dates";
    out.schedules.push_back(std::move(schedule));
    domain::instrument_schedule_date date{};
    date.owner_role = "option";
    date.owner_number = 1;
    date.schedule_role = "exercise_dates";
    date.schedule_sequence_number = 1;
    date.sequence_number = 1;
    date.schedule_date = terms.expiry.get();
    out.schedule_dates.push_back(std::move(date));
}

}

equity_result build_equity_basket_option(std::string_view trade_type,
                                         bool rainbow,
                                         const basket_option_terms& terms) {
    if (auto refusal = refusal_of(rainbow, terms))
        return std::unexpected(std::move(*refusal));
    domain::equity_instrument_data out{};
    out.instrument = row_of<domain::equity_basket_option_instrument>(trade_type, terms);
    add_shared_rows(out, "Equity", terms);
    return out;
}

fx_result build_fx_basket_option(std::string_view trade_type,
                                 bool rainbow,
                                 const basket_option_terms& terms) {
    if (auto refusal = refusal_of(rainbow, terms))
        return std::unexpected(std::move(*refusal));
    domain::fx_instrument_data out{};
    out.basket_options.push_back(row_of<domain::fx_basket_option_instrument>(trade_type, terms));
    add_shared_rows(out, "FX", terms);
    return out;
}

}

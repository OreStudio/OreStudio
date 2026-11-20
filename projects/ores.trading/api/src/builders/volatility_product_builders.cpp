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
#include "ores.trading.api/builders/volatility_product_builders.hpp"
#include <optional>
#include <string>
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;

std::optional<build_error> volatility_product_refusal(const basket_variance_swap_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must name a currency"};
    if (!(terms.notional.get() > decimal{}))
        return build_error{"notional", "must be positive"};
    if (!(terms.strike.get() > decimal{}))
        return build_error{"strike", "must be positive"};
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
    if (!(terms.start.get() < terms.end.get()))
        return build_error{"end", "must be after the start"};
    if (terms.settlement.get() < terms.end.get())
        return build_error{"settlement", "must not be before the end"};
    return std::nullopt;
}

std::optional<build_error>
volatility_product_refusal(const forward_volatility_agreement_terms& terms) {
    if (terms.pay_currency.get().empty())
        return build_error{"pay_currency", "must name a currency"};
    if (!(terms.quantity.get() > decimal{}))
        return build_error{"quantity", "must be positive"};
    if (!(terms.implied_vol_strike.get() > decimal{}))
        return build_error{"implied_vol_strike", "must be positive"};
    if (!(terms.fva_date.get() < terms.option_expiry.get()))
        return build_error{"option_expiry", "must be after the FVA date"};
    if (terms.settlement.get() < terms.option_expiry.get())
        return build_error{"settlement", "must not be before the option expiry"};
    return std::nullopt;
}

template <typename Row>
Row variance_swap_row(std::string_view trade_type, const basket_variance_swap_terms& terms) {
    Row row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.long_short = long_short_of(terms.position.get());
    row.strike = terms.strike.get();
    row.notional = terms.notional.get();
    row.squared_payoff = terms.squared_payoff.get();
    row.settlement_date = terms.settlement.get();
    row.currency = terms.currency.get();
    return row;
}

template <typename Row>
Row agreement_row(std::string_view trade_type, const forward_volatility_agreement_terms& terms) {
    Row row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.fva_date = terms.fva_date.get();
    row.option_expiry = terms.option_expiry.get();
    row.premium_date = terms.premium_date.get();
    row.long_short = long_short_of(terms.position.get());
    row.implied_vol_strike = terms.implied_vol_strike.get().to_double();
    row.quantity = terms.quantity.get().to_double();
    row.pay_currency = terms.pay_currency.get();
    row.settlement_date = terms.settlement.get();
    return row;
}

/// The members as underlying rows, and the daily ValuationSchedule as a rules schedule row.
template <typename Data>
void add_basket_rows(Data& out,
                     std::string_view underlying_type,
                     const basket_variance_swap_terms& terms) {
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
    domain::instrument_schedule schedule{};
    schedule.owner_role = "option";
    schedule.owner_number = 1;
    schedule.schedule_role = "valuation_schedule";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "rules";
    schedule.start_date = terms.start.get();
    schedule.end_date = terms.end.get();
    schedule.tenor = "1D";
    schedule.calendar = "TARGET";
    schedule.convention = "F";
    schedule.term_convention = "F";
    schedule.rule = "Forward";
    out.schedules.push_back(std::move(schedule));
}

template <typename Data>
void add_underlying_row(Data& out,
                        std::string_view underlying_type,
                        const forward_volatility_agreement_terms& terms) {
    domain::instrument_underlying row{};
    row.underlying_role = "underlying";
    row.sequence_number = 1;
    row.underlying_type = std::string(underlying_type);
    row.name = terms.underlying.get().uri();
    out.instrument_underlyings.push_back(std::move(row));
}

}

equity_basket_variance_swap_builder::equity_basket_variance_swap_builder(
    basket_variance_swap_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_basket_variance_swap_builder::build() const {
    if (auto refusal = volatility_product_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::equity_instrument_data out{};
    out.instrument =
        variance_swap_row<domain::equity_basket_variance_swap_instrument>(trade_type, terms_);
    add_basket_rows(out, "Equity", terms_);
    return out;
}

fx_basket_variance_swap_builder::fx_basket_variance_swap_builder(basket_variance_swap_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_basket_variance_swap_builder::build() const {
    if (auto refusal = volatility_product_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::fx_instrument_data out{};
    out.basket_variance_swaps.push_back(
        variance_swap_row<domain::fx_basket_variance_swap_instrument>(trade_type, terms_));
    add_basket_rows(out, "FX", terms_);
    return out;
}

equity_forward_volatility_agreement_builder::equity_forward_volatility_agreement_builder(
    forward_volatility_agreement_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_forward_volatility_agreement_builder::build() const {
    if (auto refusal = volatility_product_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::equity_instrument_data out{};
    out.instrument =
        agreement_row<domain::equity_forward_volatility_agreement_instrument>(trade_type, terms_);
    add_underlying_row(out, "Equity", terms_);
    return out;
}

fx_forward_volatility_agreement_builder::fx_forward_volatility_agreement_builder(
    forward_volatility_agreement_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_forward_volatility_agreement_builder::build() const {
    if (auto refusal = volatility_product_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::fx_instrument_data out{};
    out.forward_volatility_agreements.push_back(
        agreement_row<domain::fx_forward_volatility_agreement_instrument>(trade_type, terms_));
    add_underlying_row(out, "FX", terms_);
    return out;
}

}

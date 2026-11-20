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
#include "ores.trading.api/builders/entry_reset_option_builders.hpp"
#include <optional>
#include <string>
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

std::optional<build_error> refusal_unless_positive(std::string term, const decimal& value) {
    if (value <= decimal{})
        return build_error{std::move(term), "must be positive"};
    return std::nullopt;
}

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

std::optional<build_error> entry_reset_refusal(const best_entry_option_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must name a currency"};
    for (const auto& [term, value] : {std::pair{"notional", &terms.notional.get()},
                                      std::pair{"multiplier", &terms.multiplier.get()},
                                      std::pair{"strike", &terms.strike.get()},
                                      std::pair{"trigger_level", &terms.trigger_level.get()}})
        if (auto refusal = refusal_unless_positive(term, *value))
            return refusal;
    for (const auto& [term, value] : {std::pair{"cap", &terms.cap.get()},
                                      std::pair{"reset_minimum", &terms.reset_minimum.get()},
                                      std::pair{"premium", &terms.premium.get()}})
        if (*value < decimal{})
            return build_error{term, "must not be negative"};
    if (auto refusal = dates_within("strike_observation_dates",
                                    terms.strike_observation_dates.get(),
                                    terms.strike_date.get(),
                                    terms.expiry.get()))
        return refusal;
    if (terms.settlement.get() < terms.expiry.get())
        return build_error{"settlement", "must not be before the expiry"};
    return std::nullopt;
}

std::optional<build_error> entry_reset_refusal(const strike_resettable_option_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must name a currency"};
    for (const auto& [term, value] : {std::pair{"quantity", &terms.quantity.get()},
                                      std::pair{"strike", &terms.strike.get()},
                                      std::pair{"reset_strike", &terms.reset_strike.get()},
                                      std::pair{"trigger_price", &terms.trigger_price.get()}})
        if (auto refusal = refusal_unless_positive(term, *value))
            return refusal;
    if (terms.premium.get() < decimal{})
        return build_error{"premium", "must not be negative"};
    // The element states no start date, so an observation may fall at any time before expiry.
    if (auto refusal = dates_within(
            "observation_dates", terms.observation_dates.get(), std::nullopt, terms.expiry.get()))
        return refusal;
    if (terms.settlement.get() < terms.expiry.get())
        return build_error{"settlement", "must not be before the expiry"};
    return std::nullopt;
}

template <typename Row>
Row best_entry_row(std::string_view trade_type, const best_entry_option_terms& terms) {
    Row row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.long_short = long_short_of(terms.position.get());
    row.currency = terms.currency.get();
    row.notional = terms.notional.get();
    row.multiplier = terms.multiplier.get();
    row.cap = terms.cap.get();
    row.strike = terms.strike.get();
    row.trigger_level = terms.trigger_level.get();
    row.reset_minimum = terms.reset_minimum.get();
    row.premium = terms.premium.get();
    row.expiry_date = terms.expiry.get();
    row.strike_date = terms.strike_date.get();
    row.premium_date = terms.premium_date.get();
    row.settlement_date = terms.settlement.get();
    return row;
}

template <typename Row>
Row strike_resettable_row(std::string_view trade_type,
                          const strike_resettable_option_terms& terms) {
    Row row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.long_short = long_short_of(terms.position.get());
    row.option_type = option_type_of(terms.kind.get());
    row.currency = terms.currency.get();
    row.strike = terms.strike.get();
    row.reset_strike = terms.reset_strike.get();
    row.quantity = terms.quantity.get().to_double();
    row.trigger_type = trigger_type_of(terms.trigger.get());
    row.trigger_price = terms.trigger_price.get();
    row.expiry_date = terms.expiry.get();
    row.settlement_date = terms.settlement.get();
    row.premium = terms.premium.get();
    row.premium_date = terms.premium_date.get();
    return row;
}

/// The underlying as an underlying row, and the observation dates as a dated schedule row.
template <typename Data>
void add_entry_reset_rows(Data& out,
                          std::string_view underlying_type,
                          const marketdata::datum::market_reference& underlying,
                          std::string_view schedule_role,
                          const std::vector<ymd>& dates) {
    domain::instrument_underlying row{};
    row.underlying_role = "underlying";
    row.sequence_number = 1;
    row.underlying_type = std::string(underlying_type);
    row.name = underlying.uri();
    out.instrument_underlyings.push_back(std::move(row));
    domain::instrument_schedule schedule{};
    schedule.owner_role = "option";
    schedule.owner_number = 1;
    schedule.schedule_role = std::string(schedule_role);
    schedule.sequence_number = 1;
    schedule.schedule_kind = "dates";
    out.schedules.push_back(std::move(schedule));
    int sequence = 0;
    for (const auto& value : dates) {
        domain::instrument_schedule_date date{};
        date.owner_role = "option";
        date.owner_number = 1;
        date.schedule_role = std::string(schedule_role);
        date.schedule_sequence_number = 1;
        date.sequence_number = ++sequence;
        date.schedule_date = value;
        out.schedule_dates.push_back(std::move(date));
    }
}

}

equity_best_entry_option_builder::equity_best_entry_option_builder(best_entry_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_best_entry_option_builder::build() const {
    if (auto refusal = entry_reset_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::equity_instrument_data out{};
    out.instrument =
        best_entry_row<domain::equity_best_entry_option_instrument>(trade_type, terms_);
    add_entry_reset_rows(out,
                         "Equity",
                         terms_.underlying.get(),
                         "strike_observation_schedule",
                         terms_.strike_observation_dates.get());
    return out;
}

fx_best_entry_option_builder::fx_best_entry_option_builder(best_entry_option_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_best_entry_option_builder::build() const {
    if (auto refusal = entry_reset_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::fx_instrument_data out{};
    out.best_entry_options.push_back(
        best_entry_row<domain::fx_best_entry_option_instrument>(trade_type, terms_));
    add_entry_reset_rows(out,
                         "FX",
                         terms_.underlying.get(),
                         "strike_observation_schedule",
                         terms_.strike_observation_dates.get());
    return out;
}

equity_strike_resettable_option_builder::equity_strike_resettable_option_builder(
    strike_resettable_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_strike_resettable_option_builder::build() const {
    if (auto refusal = entry_reset_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::equity_instrument_data out{};
    out.instrument = strike_resettable_row<domain::equity_strike_resettable_option_instrument>(
        trade_type, terms_);
    add_entry_reset_rows(out,
                         "Equity",
                         terms_.underlying.get(),
                         "observation_schedule",
                         terms_.observation_dates.get());
    return out;
}

fx_strike_resettable_option_builder::fx_strike_resettable_option_builder(
    strike_resettable_option_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_strike_resettable_option_builder::build() const {
    if (auto refusal = entry_reset_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::fx_instrument_data out{};
    out.strike_resettable_options.push_back(
        strike_resettable_row<domain::fx_strike_resettable_option_instrument>(trade_type, terms_));
    add_entry_reset_rows(
        out, "FX", terms_.underlying.get(), "observation_schedule", terms_.observation_dates.get());
    return out;
}

}

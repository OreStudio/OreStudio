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
#include "ores.trading.api/builders/barrier_product_builders.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

std::optional<build_error> barrier_product_positive(std::string term, const decimal& value) {
    if (value <= decimal{})
        return build_error{std::move(term), "must be positive"};
    return std::nullopt;
}

/// Refuses a barrier whose type ORE does not read or whose level is not positive.
std::optional<build_error> barrier_product_barrier_refusal(const scripted_barrier& barrier) {
    static constexpr std::string_view types[] = {"UpAndOut", "UpAndIn", "DownAndOut", "DownAndIn"};
    if (std::ranges::find(types, barrier.type) == std::ranges::end(types))
        return build_error{"barrier", "must be UpAndOut, UpAndIn, DownAndOut or DownAndIn"};
    return barrier_product_positive("barrier", barrier.level);
}

std::optional<build_error> refusal_of(const window_barrier_option_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must name a currency"};
    if (auto refusal = barrier_product_positive("fixing_amount", terms.fixing_amount.get()))
        return refusal;
    if (auto refusal = barrier_product_positive("strike", terms.strike.get()))
        return refusal;
    if (auto refusal = barrier_product_barrier_refusal(terms.barrier.get()))
        return refusal;
    if (!(terms.window_start.get() < terms.window_end.get()) ||
        terms.expiry.get() < terms.window_end.get())
        return build_error{"window_end", "must be after the window start and by the expiry"};
    return std::nullopt;
}

std::optional<build_error> refusal_of(const generic_barrier_option_terms& terms) {
    if (terms.pay_currency.get().empty())
        return build_error{"pay_currency", "must name a currency"};
    if (auto refusal = barrier_product_positive("quantity", terms.quantity.get()))
        return refusal;
    if (auto refusal = barrier_product_positive("strike", terms.strike.get()))
        return refusal;
    const auto& barriers = terms.barriers.get();
    if (barriers.empty())
        return build_error{"barriers", "must name at least one barrier"};
    for (const auto& barrier : barriers)
        if (auto refusal = barrier_product_barrier_refusal(barrier))
            return refusal;
    if (terms.kiko_type) {
        static constexpr std::string_view kikos[] = {"KoAlways", "KoBeforeKi", "KoAfterKi"};
        if (std::ranges::find(kikos, *terms.kiko_type) == std::ranges::end(kikos))
            return build_error{"kiko_type", "must be KoAlways, KoBeforeKi or KoAfterKi"};
    }
    if (!(terms.monitoring_start.get() < terms.expiry.get()))
        return build_error{"monitoring_start", "must be before the expiry"};
    if (terms.settlement.get() < terms.expiry.get())
        return build_error{"settlement", "must not be before the expiry"};
    return std::nullopt;
}

/// A European OptionData block exercised on the one date.
template <typename Data>
void add_option(Data& out,
                option_position position,
                option_kind kind,
                const ymd& exercise,
                std::optional<std::string> payoff_type) {
    domain::instrument_option option{};
    option.long_short = long_short_of(position);
    option.option_type = option_type_of(kind);
    option.payoff_type = std::move(payoff_type);
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
    date.schedule_date = exercise;
    out.schedule_dates.push_back(std::move(date));
}

template <typename Data>
void add_underlying(Data& out,
                    std::string_view type,
                    const marketdata::datum::market_reference& underlying) {
    domain::instrument_underlying row{};
    row.underlying_role = "underlying";
    row.sequence_number = 1;
    row.underlying_type = std::string(type);
    row.name = underlying.uri();
    out.instrument_underlyings.push_back(std::move(row));
}

/// One BarrierData block with its one level, under the role.
template <typename Data>
void add_barrier(Data& out, const scripted_barrier& terms) {
    const int sequence = static_cast<int>(out.barriers.size()) + 1;
    domain::instrument_barrier row{};
    row.barrier_role = "barrier";
    row.sequence_number = sequence;
    row.barrier_type = terms.type;
    out.barriers.push_back(std::move(row));
    domain::instrument_barrier_level level{};
    level.barrier_role = "barrier";
    level.barrier_sequence_number = sequence;
    level.sequence_number = 1;
    level.level = terms.level;
    out.barrier_levels.push_back(std::move(level));
}

template <typename Row>
Row window_row(std::string_view trade_type, const window_barrier_option_terms& terms) {
    Row row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.currency = terms.currency.get();
    row.fixing_amount = terms.fixing_amount.get();
    row.strike = terms.strike.get();
    row.start_date = terms.window_start.get();
    row.end_date = terms.window_end.get();
    return row;
}

template <typename Data>
void add_window_rows(Data& out, std::string_view type, const window_barrier_option_terms& terms) {
    add_underlying(out, type, terms.underlying.get());
    add_option(out, terms.position.get(), terms.kind.get(), terms.expiry.get(), std::nullopt);
    add_barrier(out, terms.barrier.get());
}

}

equity_window_barrier_option_builder::equity_window_barrier_option_builder(
    window_barrier_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_window_barrier_option_builder::build() const {
    if (auto refusal = refusal_of(terms_))
        return std::unexpected(std::move(*refusal));
    domain::equity_instrument_data out{};
    out.instrument =
        window_row<domain::equity_window_barrier_option_instrument>(trade_type, terms_);
    add_window_rows(out, "Equity", terms_);
    return out;
}

fx_window_barrier_option_builder::fx_window_barrier_option_builder(
    window_barrier_option_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_window_barrier_option_builder::build() const {
    if (auto refusal = refusal_of(terms_))
        return std::unexpected(std::move(*refusal));
    domain::fx_instrument_data out{};
    out.window_barrier_options.push_back(
        window_row<domain::fx_window_barrier_option_instrument>(trade_type, terms_));
    add_window_rows(out, "FX", terms_);
    return out;
}

equity_generic_barrier_option_builder::equity_generic_barrier_option_builder(
    generic_barrier_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_generic_barrier_option_builder::build() const {
    if (auto refusal = refusal_of(terms_))
        return std::unexpected(std::move(*refusal));
    domain::equity_instrument_data out{};
    domain::equity_generic_barrier_option_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.pay_currency = terms_.pay_currency.get();
    row.quantity = terms_.quantity.get().to_double();
    row.strike = terms_.strike.get();
    row.settlement_date = terms_.settlement.get();
    row.kiko_type = terms_.kiko_type;
    out.instrument = std::move(row);
    add_underlying(out, "Equity", terms_.underlying.get());
    add_option(out, terms_.position.get(), terms_.kind.get(), terms_.expiry.get(), "Vanilla");
    domain::instrument_schedule schedule{};
    schedule.owner_role = "option";
    schedule.owner_number = 1;
    schedule.schedule_role = "barrier_schedule";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "rules";
    schedule.start_date = terms_.monitoring_start.get();
    schedule.end_date = terms_.expiry.get();
    schedule.tenor = "1D";
    schedule.calendar = "TARGET";
    schedule.convention = "F";
    schedule.term_convention = "F";
    schedule.rule = "Forward";
    out.schedules.push_back(std::move(schedule));
    for (const auto& barrier : terms_.barriers.get())
        add_barrier(out, barrier);
    return out;
}

}

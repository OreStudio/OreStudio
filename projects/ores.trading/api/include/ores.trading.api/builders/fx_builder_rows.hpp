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
#ifndef ORES_TRADING_API_BUILDERS_FX_BUILDER_ROWS_HPP
#define ORES_TRADING_API_BUILDERS_FX_BUILDER_ROWS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <algorithm>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::builders {

/// One currency bought against another.
struct fx_exchange final {
    required<std::string> bought_currency;
    required<utility::decimal::decimal> bought_amount;
    required<std::string> sold_currency;
    required<utility::decimal::decimal> sold_amount;
};

/// A barrier: its ORE type and the levels it triggers at.
struct fx_barrier final {
    std::string type;
    std::vector<utility::decimal::decimal> levels;
};

}

/**
 * @brief The rows and checks the FX builders share.
 *
 * The barrier and digital builders are class templates defined in their
 * header, so what they share is inline here rather than private to a source
 * file.
 */
namespace ores::trading::builders::fx_detail {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

inline std::optional<build_error> exchange_refusal(const fx_exchange& exchange) {
    if (exchange.bought_currency.get().empty() || exchange.sold_currency.get().empty())
        return build_error{"exchange", "must name both currencies"};
    if (exchange.bought_currency.get() == exchange.sold_currency.get())
        return build_error{"exchange", "must name two different currencies"};
    if (exchange.bought_amount.get() <= decimal{} || exchange.sold_amount.get() <= decimal{})
        return build_error{"exchange", "must state positive amounts"};
    return std::nullopt;
}

template <typename Row>
inline Row exchange_row(std::string_view trade_type, const fx_exchange& exchange) {
    Row row;
    row.identity.trade_type_code = std::string(trade_type);
    row.bought_currency = exchange.bought_currency.get();
    row.bought_amount = exchange.bought_amount.get();
    row.sold_currency = exchange.sold_currency.get();
    row.sold_amount = exchange.sold_amount.get();
    return row;
}

inline domain::instrument_option option_row(option_position position,
                                            std::optional<std::string> style,
                                            std::optional<std::string> payoff_type = {},
                                            std::optional<std::string> pay_off_at_expiry = {}) {
    domain::instrument_option row;
    row.long_short = long_short_of(position);
    row.style = std::move(style);
    if (row.style)
        row.settlement = "Cash";
    row.payoff_type = std::move(payoff_type);
    row.pay_off_at_expiry = std::move(pay_off_at_expiry);
    return row;
}

inline void add_barriers(domain::fx_instrument_data& out, const std::vector<fx_barrier>& barriers) {
    int number = 0;
    for (const auto& barrier : barriers) {
        domain::instrument_barrier row;
        row.barrier_role = "barrier";
        row.sequence_number = ++number;
        row.barrier_type = barrier.type;
        out.barriers.push_back(std::move(row));
        int level_number = 0;
        for (const auto& value : barrier.levels) {
            domain::instrument_barrier_level level;
            level.barrier_role = "barrier";
            level.barrier_sequence_number = number;
            level.sequence_number = ++level_number;
            level.level = value;
            out.barrier_levels.push_back(std::move(level));
        }
    }
}

/// The pair an FX product observes, as the Underlying block states it.
inline void add_underlying(domain::fx_instrument_data& out,
                           const marketdata::datum::market_reference& fx_index) {
    domain::instrument_underlying row;
    row.underlying_role = "underlying";
    row.sequence_number = 1;
    row.underlying_type = "FX";
    row.name = fx_index.uri();
    out.instrument_underlyings.push_back(std::move(row));
}

/*
 * A schedule kept as the product's own, from @p start to @p end every @p tenor.
 * Every generated FX schedule observes on the TARGET calendar, rolling forward,
 * as ORE's samples do.
 */
inline void add_schedule(
    domain::fx_instrument_data& out, std::string role, std::string tenor, ymd start, ymd end) {
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
    row.rule = "Forward";
    out.schedules.push_back(std::move(row));
}

/// Fixings below the strike are taken twice, those above it once.
inline std::vector<domain::range_bound> strike_leverage_bounds(decimal strike) {
    domain::range_bound below;
    below.sequence_number = 1;
    below.range_to = strike;
    below.leverage = 2.0;
    domain::range_bound above;
    above.sequence_number = 2;
    above.range_from = strike;
    above.leverage = 1.0;
    return {below, above};
}

inline bool is_single_type(std::string_view type) {
    return type == "UpAndIn" || type == "UpAndOut" || type == "DownAndIn" || type == "DownAndOut";
}

inline bool is_double_type(std::string_view type) {
    return type == "KnockIn" || type == "KnockOut";
}

inline bool knocks_in(std::string_view type) {
    return type.ends_with("In");
}

/// One barrier of the type set @p fits and of @p levels levels.
inline std::optional<build_error> barrier_refusal(const std::vector<fx_barrier>& barriers,
                                                  bool (*fits)(std::string_view),
                                                  std::size_t levels,
                                                  std::string_view shape) {
    if (barriers.size() != 1 || !fits(barriers.front().type) ||
        barriers.front().levels.size() != levels)
        return build_error{"barriers", "must be " + std::string(shape)};
    for (const auto& level : barriers.front().levels)
        if (level <= decimal{})
            return build_error{"barriers", "must state positive levels"};
    return std::nullopt;
}

/// The barrier columns of a row: the type of the first barrier and its levels.
template <typename Row>
inline void set_barrier_columns(Row& row, const std::vector<fx_barrier>& barriers) {
    if (barriers.empty())
        return;
    row.barrier_type = barriers.front().type;
    std::vector<decimal> levels;
    for (const auto& barrier : barriers)
        levels.insert(levels.end(), barrier.levels.begin(), barrier.levels.end());
    std::ranges::sort(levels);
    if (levels.size() == 1) {
        if (barriers.front().type.starts_with("Up"))
            row.upper_barrier = levels.front();
        else
            row.lower_barrier = levels.front();
        return;
    }
    row.lower_barrier = levels.front();
    row.upper_barrier = levels.back();
}

}

#endif

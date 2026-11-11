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
#ifndef ORES_TRADING_API_BUILDERS_EQUITY_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_EQUITY_BUILDERS_HPP

#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

/**
 * @file equity_builders.hpp
 * @brief The equity products the generator books.
 *
 * Every product names its underlying by an oresmd equity address, which the
 * ORE projection writes as ORE names it. An option settles in cash and
 * exercises European; every schedule observes on the TARGET calendar, rolling
 * following. A caller that needs another choice extends the terms.
 */
namespace ores::trading::builders {

using equity_result = std::expected<domain::equity_instrument_data, build_error>;

/// An option on a quantity of one equity, struck at a price.
struct equity_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<option_kind> kind;
    required<utility::decimal::decimal> strike;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityOption";
    explicit equity_option_builder(equity_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_option_terms terms_;
};

/// A cliquet: the option restrikes at each reset from the start to the expiry.
struct equity_cliquet_option_terms final {
    equity_option_terms option;
    required<std::chrono::year_month_day> start;
    /// How often it resets, as a schedule tenor such as 3M.
    required<std::string> reset_tenor;
};

class ORES_TRADING_API_EXPORT equity_cliquet_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityCliquetOption";
    explicit equity_cliquet_option_builder(equity_cliquet_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_cliquet_option_terms terms_;
};

/// An Asian option on the arithmetic average of monthly prices from the start to the expiry.
struct equity_asian_option_terms final {
    equity_option_terms option;
    required<std::chrono::year_month_day> averaging_start;
};

class ORES_TRADING_API_EXPORT equity_asian_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityAsianOption";
    explicit equity_asian_option_builder(equity_asian_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_asian_option_terms terms_;
};

/// The equity barrier products, each a European option with its barrier.
enum class equity_barrier_product { barrier, european_barrier, double_barrier };

/**
 * @brief An option that knocks in or out at its barrier.
 *
 * A single barrier states one level of an UpAnd or a DownAnd type; a double
 * barrier states two levels of a KnockIn or KnockOut type.
 */
struct equity_barrier_option_terms final {
    equity_option_terms option;
    required<std::string> barrier_type;
    required<std::vector<utility::decimal::decimal>> levels;
};

/// The checks and rows a barrier option shares with the other equity builders.
namespace equity_detail {

[[nodiscard]] ORES_TRADING_API_EXPORT std::optional<build_error>
option_refusal(const equity_option_terms& terms);

[[nodiscard]] ORES_TRADING_API_EXPORT domain::instrument_option
option_row(option_position position,
           std::optional<std::string> style,
           std::optional<std::string> payoff_type = {},
           std::optional<std::string> pay_off_at_expiry = {});

[[nodiscard]] ORES_TRADING_API_EXPORT std::optional<build_error>
barrier_refusal(std::string_view type,
                const std::vector<utility::decimal::decimal>& levels,
                bool double_barrier);

}

template <equity_barrier_product Product>
class equity_barrier_option_builder final {
public:
    static constexpr std::string_view trade_type = [] {
        switch (Product) {
            case equity_barrier_product::barrier:
                return std::string_view("EquityBarrierOption");
            case equity_barrier_product::european_barrier:
                return std::string_view("EquityEuropeanBarrierOption");
            default:
                return std::string_view("EquityDoubleBarrierOption");
        }
    }();

    explicit equity_barrier_option_builder(equity_barrier_option_terms terms)
        : terms_(std::move(terms)) {}

    [[nodiscard]] equity_result build() const {
        const auto& option = terms_.option;
        if (auto refusal = equity_detail::option_refusal(option))
            return std::unexpected(std::move(*refusal));
        const auto& levels = terms_.levels.get();
        constexpr bool double_barrier = Product == equity_barrier_product::double_barrier;
        if (auto refusal =
                equity_detail::barrier_refusal(terms_.barrier_type.get(), levels, double_barrier))
            return std::unexpected(std::move(*refusal));

        domain::equity_barrier_option_instrument row{};
        row.identity.trade_type_code = std::string(trade_type);
        row.underlying_name = option.underlying.get().uri();
        row.currency = option.currency.get();
        row.notional = option.quantity.get();
        row.option_type = option_type_of(option.kind.get());
        row.strike = option.strike.get();
        row.expiry_date = option.expiry.get();
        row.exercise_type = "European";
        row.long_short = long_short_of(option.position.get());
        row.lower_barrier = levels.front();
        row.lower_barrier_type = terms_.barrier_type.get();
        if constexpr (double_barrier) {
            row.upper_barrier = levels.back();
            row.upper_barrier_type = terms_.barrier_type.get();
        }
        row.rebate = utility::decimal::decimal{};

        domain::equity_instrument_data out;
        out.instrument = std::move(row);
        out.options.push_back(equity_detail::option_row(option.position.get(), "European"));
        return out;
    }

private:
    equity_barrier_option_terms terms_;
};

/// A digital: pays a fixed amount when the equity ends beyond the strike.
struct equity_digital_option_terms final {
    equity_option_terms option;
    required<utility::decimal::decimal> payout;
};

class ORES_TRADING_API_EXPORT equity_digital_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityDigitalOption";
    explicit equity_digital_option_builder(equity_digital_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_digital_option_terms terms_;
};

/// A touch: pays a fixed amount when the equity touches its barrier before the expiry.
struct equity_touch_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> payout;
    /// One of ORE's touch types, such as UpAndIn for a one touch above the spot.
    required<std::string> barrier_type;
    required<utility::decimal::decimal> level;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_touch_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityTouchOption";
    explicit equity_touch_option_builder(equity_touch_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_touch_option_terms terms_;
};

struct equity_forward_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<double> quantity;
    required<utility::decimal::decimal> forward_price;
    required<std::chrono::year_month_day> maturity;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_forward_builder final {
public:
    static constexpr std::string_view trade_type = "EquityForward";
    explicit equity_forward_builder(equity_forward_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_forward_terms terms_;
};

/**
 * @brief An equity's price return against the floating leg of a swap, both
 * on the floating index's schedule.
 *
 * The floating leg takes its terms from the swap convention of the index; the
 * equity leg pays on the same schedule.
 */
struct equity_swap_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<double> quantity;
    vanilla_swap_terms funding;
};

class ORES_TRADING_API_EXPORT equity_swap_builder final {
public:
    static constexpr std::string_view trade_type = "EquitySwap";
    equity_swap_builder(const marketdata::conventions::convention_lookup& conventions,
                        equity_swap_terms terms);
    equity_swap_builder(marketdata::conventions::convention_lookup&&, equity_swap_terms) = delete;
    [[nodiscard]] equity_result build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    equity_swap_terms terms_;
};

/// One equity of a basket and the price its performance is measured from.
struct equity_basket_member final {
    marketdata::datum::market_reference underlying;
    utility::decimal::decimal initial_price;
};

/**
 * @brief A swap on the worst performer of a basket of distinct equities,
 * observed on its dates against a knock out at par and a trigger at three
 * quarters.
 */
struct equity_worst_of_basket_swap_terms final {
    required<std::vector<equity_basket_member>> basket;
    /// The end of each accrual period, ascending, after the start and no later than maturity.
    required<std::vector<std::chrono::year_month_day>> observation_dates;
    /// The index the funding leg floats on, in the basket's currency.
    required<marketdata::datum::market_reference> funding_index;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_worst_of_basket_swap_builder final {
public:
    static constexpr std::string_view trade_type = "EquityWorstOfBasketSwap";
    explicit equity_worst_of_basket_swap_builder(equity_worst_of_basket_swap_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_worst_of_basket_swap_terms terms_;
};

struct equity_variance_swap_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    /// The strike as a variance, such as 0.04 for a twenty per cent volatility.
    required<double> variance_strike;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_variance_swap_builder final {
public:
    static constexpr std::string_view trade_type = "EquityVarianceSwap";
    explicit equity_variance_swap_builder(equity_variance_swap_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_variance_swap_terms terms_;
};

/**
 * @brief Monthly fixings of an equity at a strike, doubled below it, until the
 * equity rises through the knock out level.
 */
struct equity_accumulator_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> fixing_amount;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> knock_out;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_accumulator_builder final {
public:
    static constexpr std::string_view trade_type = "EquityAccumulator";
    explicit equity_accumulator_builder(equity_accumulator_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_accumulator_terms terms_;
};

/**
 * @brief Monthly fixings of an equity at a strike, doubled below it, until the
 * cumulated gains reach the target.
 */
struct equity_tarf_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> fixing_amount;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> target_amount;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_tarf_builder final {
public:
    static constexpr std::string_view trade_type = "EquityTaRF";
    explicit equity_tarf_builder(equity_tarf_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_tarf_terms terms_;
};

/// A holding of an equity, or of an equity option, at a price.
struct equity_position_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<double> quantity;
    required<utility::decimal::decimal> price;
};

class ORES_TRADING_API_EXPORT equity_position_builder final {
public:
    static constexpr std::string_view trade_type = "EquityPosition";
    explicit equity_position_builder(equity_position_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_position_terms terms_;
};

class ORES_TRADING_API_EXPORT equity_option_position_builder final {
public:
    static constexpr std::string_view trade_type = "EquityOptionPosition";
    explicit equity_option_position_builder(equity_position_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    equity_position_terms terms_;
};

}

#endif

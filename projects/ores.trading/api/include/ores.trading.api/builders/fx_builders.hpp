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
#ifndef ORES_TRADING_API_BUILDERS_FX_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_FX_BUILDERS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/fx_builder_rows.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file fx_builders.hpp
 * @brief The FX products the generator books.
 *
 * Every FX trade names its pair by an FX index address; the ORE projection
 * writes it as ORE spells an index or an underlying. No convention is read: an
 * FX product states its own dates and amounts.
 *
 * The builders fix what the generator does not draw: an option settles in cash
 * and a swap physically, an average forward is bought, and every schedule
 * observes on the TARGET calendar. A caller that needs another choice extends
 * the terms.
 */
namespace ores::trading::builders {

using fx_result = std::expected<domain::fx_instrument_data, build_error>;

struct fx_forward_terms final {
    fx_exchange exchange;
    required<std::chrono::year_month_day> value_date;
    required<settlement> settles;
};

class ORES_TRADING_API_EXPORT fx_forward_builder final {
public:
    static constexpr std::string_view trade_type = "FxForward";
    explicit fx_forward_builder(fx_forward_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_forward_terms terms_;
};

/// An exchange on the near date, reversed on the far date at the far amounts.
struct fx_swap_terms final {
    fx_exchange near_leg;
    required<std::chrono::year_month_day> near_date;
    required<std::chrono::year_month_day> far_date;
    /// What is bought back on the far date: the near leg's sold currency.
    required<utility::decimal::decimal> far_bought_amount;
    required<utility::decimal::decimal> far_sold_amount;
};

class ORES_TRADING_API_EXPORT fx_swap_builder final {
public:
    static constexpr std::string_view trade_type = "FxSwap";
    explicit fx_swap_builder(fx_swap_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_swap_terms terms_;
};

/// A European option to make the exchange on its expiry.
struct fx_option_terms final {
    fx_exchange exchange;
    required<option_kind> kind;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT fx_option_builder final {
public:
    static constexpr std::string_view trade_type = "FxOption";
    explicit fx_option_builder(fx_option_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_option_terms terms_;
};

/// The FX barrier products, each a European option with its own barriers.
enum class fx_barrier_product { barrier, double_barrier, european_barrier, kiko };

/**
 * @brief The terms of an FX barrier option.
 *
 * A single barrier product states one barrier of one level, of an UpAnd or a
 * DownAnd type. A double barrier states one KnockIn or KnockOut barrier of two
 * levels. A KIKO states a knock-in and a knock-out barrier of one level each.
 */
struct fx_barrier_option_terms final {
    fx_option_terms option;
    required<std::vector<fx_barrier>> barriers;
    required<marketdata::datum::market_reference> fx_index;
};

template <fx_barrier_product Product>
class fx_barrier_option_builder final {
public:
    static constexpr std::string_view trade_type = [] {
        switch (Product) {
            case fx_barrier_product::barrier:
                return std::string_view("FxBarrierOption");
            case fx_barrier_product::double_barrier:
                return std::string_view("FxDoubleBarrierOption");
            case fx_barrier_product::european_barrier:
                return std::string_view("FxEuropeanBarrierOption");
            default:
                return std::string_view("FxKIKOBarrierOption");
        }
    }();
    explicit fx_barrier_option_builder(fx_barrier_option_terms terms)
        : terms_(std::move(terms)) {}
    [[nodiscard]] fx_result build() const;

private:
    fx_barrier_option_terms terms_;
};

/**
 * @brief A vanilla option on a pair that knocks out at a barrier watched daily
 * from the monitoring start to the expiry.
 */
struct fx_generic_barrier_option_terms final {
    fx_option_terms option;
    required<fx_barrier> barrier;
    required<marketdata::datum::market_reference> fx_index;
    required<std::chrono::year_month_day> monitoring_start;
};

class ORES_TRADING_API_EXPORT fx_generic_barrier_option_builder final {
public:
    static constexpr std::string_view trade_type = "FxGenericBarrierOption";
    explicit fx_generic_barrier_option_builder(fx_generic_barrier_option_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_generic_barrier_option_terms terms_;
};

/// The FX products that pay a fixed amount: on expiry, or on a touch.
enum class fx_digital_product { digital, touch, double_touch, digital_barrier };

/**
 * @brief The terms of an FX digital or touch option.
 *
 * A digital pays when the pair ends beyond its strike, and a digital barrier
 * also needs its barrier. A touch pays when the pair touches its barrier, and a
 * double touch when it touches either of its two levels.
 */
struct fx_digital_option_terms final {
    required<std::string> foreign_currency;
    required<std::string> domestic_currency;
    required<utility::decimal::decimal> payoff_amount;
    required<option_kind> kind;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
    std::optional<utility::decimal::decimal> strike;
    std::vector<fx_barrier> barriers;
    required<marketdata::datum::market_reference> fx_index;
};

template <fx_digital_product Product>
class fx_digital_option_builder final {
public:
    static constexpr std::string_view trade_type = [] {
        switch (Product) {
            case fx_digital_product::digital:
                return std::string_view("FxDigitalOption");
            case fx_digital_product::touch:
                return std::string_view("FxTouchOption");
            case fx_digital_product::double_touch:
                return std::string_view("FxDoubleTouchOption");
            default:
                return std::string_view("FxDigitalBarrierOption");
        }
    }();
    explicit fx_digital_option_builder(fx_digital_option_terms terms)
        : terms_(std::move(terms)) {}
    [[nodiscard]] fx_result build() const;

private:
    fx_digital_option_terms terms_;
};

/// An exchange at the average of the pair's monthly fixings.
struct fx_average_forward_terms final {
    required<marketdata::datum::market_reference> fx_index;
    required<std::string> reference_currency;
    required<utility::decimal::decimal> reference_notional;
    required<std::string> settlement_currency;
    required<utility::decimal::decimal> settlement_notional;
    /// The first observation; the last is the payment date.
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> payment_date;
};

class ORES_TRADING_API_EXPORT fx_average_forward_builder final {
public:
    static constexpr std::string_view trade_type = "FxAverageForward";
    explicit fx_average_forward_builder(fx_average_forward_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_average_forward_terms terms_;
};

/**
 * @brief Monthly fixings at a strike, settled until the gains reach a target.
 *
 * Below the strike the fixing is doubled; the target is the cumulated gain at
 * which the trade ends.
 */
struct fx_tarf_terms final {
    required<marketdata::datum::market_reference> fx_index;
    required<std::string> currency;
    required<utility::decimal::decimal> fixing_amount;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> target_amount;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT fx_tarf_builder final {
public:
    static constexpr std::string_view trade_type = "FxTaRF";
    explicit fx_tarf_builder(fx_tarf_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_tarf_terms terms_;
};

struct fx_variance_swap_terms final {
    required<marketdata::datum::market_reference> fx_index;
    required<std::string> currency;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
    /// The strike as a volatility, such as 0.1 for ten per cent.
    required<double> strike;
    required<utility::decimal::decimal> notional;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT fx_variance_swap_builder final {
public:
    static constexpr std::string_view trade_type = "FxVarianceSwap";
    explicit fx_variance_swap_builder(fx_variance_swap_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_variance_swap_terms terms_;
};

/**
 * @brief Monthly fixings bought at a strike, doubled below it, until the pair
 * rises through the knock out level.
 */
struct fx_accumulator_terms final {
    required<marketdata::datum::market_reference> fx_index;
    required<std::string> currency;
    required<utility::decimal::decimal> fixing_amount;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> knock_out;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT fx_accumulator_builder final {
public:
    static constexpr std::string_view trade_type = "FxAccumulator";
    explicit fx_accumulator_builder(fx_accumulator_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_accumulator_terms terms_;
};


template <fx_barrier_product Product>
fx_result fx_barrier_option_builder<Product>::build() const {
    const auto& option = terms_.option;
    if (auto refusal = fx_detail::exchange_refusal(option.exchange))
        return std::unexpected(std::move(*refusal));
    const auto& barriers = terms_.barriers.get();
    if constexpr (Product == fx_barrier_product::double_barrier) {
        if (auto refusal =
                fx_detail::barrier_refusal(barriers,
                                           fx_detail::is_double_type,
                                           2,
                                           "one KnockIn or KnockOut barrier of two levels"))
            return std::unexpected(std::move(*refusal));
    } else if constexpr (Product == fx_barrier_product::kiko) {
        const bool shaped =
            barriers.size() == 2 && fx_detail::is_single_type(barriers[0].type) &&
            fx_detail::is_single_type(barriers[1].type) &&
            fx_detail::knocks_in(barriers[0].type) != fx_detail::knocks_in(barriers[1].type) &&
            barriers[0].levels.size() == 1 && barriers[1].levels.size() == 1;
        if (!shaped)
            return std::unexpected(build_error{
                "barriers", "must be a knock-in and a knock-out barrier of one level each"});
    } else {
        if (auto refusal = fx_detail::barrier_refusal(
                barriers, fx_detail::is_single_type, 1, "one UpAnd or DownAnd barrier"))
            return std::unexpected(std::move(*refusal));
    }

    auto row =
        fx_detail::exchange_row<domain::fx_barrier_option_instrument>(trade_type, option.exchange);
    row.option_type = option_type_of(option.kind.get());
    row.expiry_date = option.expiry.get();
    row.settlement = "Cash";
    fx_detail::set_barrier_columns(row, barriers);
    row.barrier_fixing_index = terms_.fx_index.get().uri();
    domain::fx_instrument_data out;
    out.options.push_back(fx_detail::option_row(option.position.get(), "European"));
    out.barrier_options.push_back(std::move(row));
    fx_detail::add_barriers(out, barriers);
    return out;
}

template <fx_digital_product Product>
fx_result fx_digital_option_builder<Product>::build() const {
    if (terms_.foreign_currency.get() == terms_.domestic_currency.get())
        return std::unexpected(build_error{"currencies", "must be two different currencies"});
    if (terms_.payoff_amount.get() <= utility::decimal::decimal{})
        return std::unexpected(build_error{"payoff_amount", "must be positive"});
    constexpr bool struck =
        Product == fx_digital_product::digital || Product == fx_digital_product::digital_barrier;
    if (struck && !(terms_.strike && *terms_.strike > utility::decimal::decimal{}))
        return std::unexpected(build_error{"strike", "must be positive"});
    if (!struck && terms_.strike)
        return std::unexpected(build_error{"strike", "a touch pays on its barrier, not a strike"});
    if constexpr (Product == fx_digital_product::digital) {
        if (!terms_.barriers.empty())
            return std::unexpected(build_error{"barriers", "must be none"});
    } else if constexpr (Product == fx_digital_product::double_touch) {
        if (auto refusal =
                fx_detail::barrier_refusal(terms_.barriers,
                                           fx_detail::is_double_type,
                                           2,
                                           "one KnockIn or KnockOut barrier of two levels"))
            return std::unexpected(std::move(*refusal));
    } else {
        if (auto refusal = fx_detail::barrier_refusal(
                terms_.barriers, fx_detail::is_single_type, 1, "one UpAnd or DownAnd barrier"))
            return std::unexpected(std::move(*refusal));
    }

    domain::fx_digital_option_instrument row;
    row.identity.trade_type_code = std::string(trade_type);
    row.foreign_currency = terms_.foreign_currency.get();
    row.domestic_currency = terms_.domestic_currency.get();
    // Paid in the domestic currency, as ORE's samples pay.
    row.payoff_currency = terms_.domestic_currency.get();
    row.payoff_amount = terms_.payoff_amount.get();
    row.option_type = option_type_of(terms_.kind.get());
    row.expiry_date = terms_.expiry.get();
    row.long_short = long_short_of(terms_.position.get());
    row.strike = terms_.strike;
    fx_detail::set_barrier_columns(row, terms_.barriers);
    if constexpr (Product == fx_digital_product::digital_barrier)
        row.barrier_fixing_index = terms_.fx_index.get().uri();

    domain::fx_instrument_data out;
    out.digital_options.push_back(std::move(row));
    // A touch pays when touched, so ORE states no exercise style for it.
    constexpr bool touch =
        Product == fx_digital_product::touch || Product == fx_digital_product::double_touch;
    out.options.push_back(
        touch ? fx_detail::option_row(terms_.position.get(), std::nullopt, {}, "true") :
                fx_detail::option_row(terms_.position.get(), "European"));
    fx_detail::add_barriers(out, terms_.barriers);
    return out;
}

}

#endif

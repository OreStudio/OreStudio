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
#ifndef ORES_TRADING_API_BUILDERS_RATE_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_RATE_BUILDERS_HPP

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

/**
 * @file rate_builders.hpp
 * @brief The interest rate products the generator books, each built from a
 * vanilla swap or its floating leg.
 *
 * Each builder keeps a reference to the conventions, so they must outlive it.
 */
namespace ores::trading::builders {

using conventions_ref = const marketdata::conventions::convention_lookup&;
using swap_result = std::expected<domain::swap_instrument_data, build_error>;

/**
 * @brief A fixed leg in one currency against a floating leg in another, with
 * the notionals exchanged at the start and at maturity.
 *
 * The fixed leg takes the conventions of the domestic swap, the floating leg
 * those of the foreign index.
 */
struct cross_currency_swap_terms final {
    vanilla_swap_terms domestic;
    required<utility::decimal::decimal> foreign_notional;
    required<std::string> foreign_currency;
    required<marketdata::datum::market_reference> foreign_index;
    /// The id of the foreign swap convention, when the foreign index has more than one.
    std::optional<std::string> foreign_convention = std::nullopt;
};

class ORES_TRADING_API_EXPORT cross_currency_swap_builder final {
public:
    static constexpr std::string_view trade_type = "CrossCurrencySwap";
    cross_currency_swap_builder(conventions_ref conventions, cross_currency_swap_terms terms);
    cross_currency_swap_builder(marketdata::conventions::convention_lookup&&,
                                cross_currency_swap_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    cross_currency_swap_terms terms_;
};

/// A forward rate agreement on an Ibor index, struck at the swap's fixed rate.
struct forward_rate_agreement_terms final {
    vanilla_swap_terms period;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT forward_rate_agreement_builder final {
public:
    static constexpr std::string_view trade_type = "ForwardRateAgreement";
    forward_rate_agreement_builder(conventions_ref conventions, forward_rate_agreement_terms terms);
    forward_rate_agreement_builder(marketdata::conventions::convention_lookup&&,
                                   forward_rate_agreement_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    forward_rate_agreement_terms terms_;
};

enum class cap_floor_kind { cap, floor };

/// A cap or a floor on the floating leg of a swap, struck at the swap's fixed rate.
struct cap_floor_terms final {
    vanilla_swap_terms swap;
    required<cap_floor_kind> kind;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT cap_floor_builder final {
public:
    static constexpr std::string_view trade_type = "CapFloor";
    cap_floor_builder(conventions_ref conventions, cap_floor_terms terms);
    cap_floor_builder(marketdata::conventions::convention_lookup&&, cap_floor_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    cap_floor_terms terms_;
};

/// A European option to enter a swap that starts on or after the expiry.
struct swaption_terms final {
    vanilla_swap_terms swap;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
    required<settlement> settles;
};

class ORES_TRADING_API_EXPORT swaption_builder final {
public:
    static constexpr std::string_view trade_type = "Swaption";
    swaption_builder(conventions_ref conventions, swaption_terms terms);
    swaption_builder(marketdata::conventions::convention_lookup&&, swaption_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    swaption_terms terms_;
};

/// A swap one side may cancel once a year, from the first call date on.
struct callable_swap_terms final {
    vanilla_swap_terms swap;
    required<std::chrono::year_month_day> first_call;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT callable_swap_builder final {
public:
    static constexpr std::string_view trade_type = "CallableSwap";
    callable_swap_builder(conventions_ref conventions, callable_swap_terms terms);
    callable_swap_builder(marketdata::conventions::convention_lookup&&,
                          callable_swap_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    callable_swap_terms terms_;
};

/// A swap that ends when its floating index crosses a barrier.
struct knock_out_swap_terms final {
    vanilla_swap_terms swap;
    /// One of ORE's barrier types, such as UpAndOut.
    required<std::string> barrier_type;
    required<utility::decimal::decimal> barrier_level;
    required<std::chrono::year_month_day> barrier_start;
};

class ORES_TRADING_API_EXPORT knock_out_swap_builder final {
public:
    static constexpr std::string_view trade_type = "KnockOutSwap";
    knock_out_swap_builder(conventions_ref conventions, knock_out_swap_terms terms);
    knock_out_swap_builder(marketdata::conventions::convention_lookup&&,
                           knock_out_swap_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    knock_out_swap_terms terms_;
};

/// A swap whose notional one side may reduce on its schedule.
struct flexi_swap_terms final {
    vanilla_swap_terms swap;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT flexi_swap_builder final {
public:
    static constexpr std::string_view trade_type = "FlexiSwap";
    flexi_swap_builder(conventions_ref conventions, flexi_swap_terms terms);
    flexi_swap_builder(marketdata::conventions::convention_lookup&&, flexi_swap_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    flexi_swap_terms terms_;
};

/// A swap whose notional follows the balance of one tranche of a security.
struct balance_guaranteed_swap_terms final {
    vanilla_swap_terms swap;
    required<std::string> reference_security;
};

class ORES_TRADING_API_EXPORT balance_guaranteed_swap_builder final {
public:
    static constexpr std::string_view trade_type = "BalanceGuaranteedSwap";
    balance_guaranteed_swap_builder(conventions_ref conventions,
                                    balance_guaranteed_swap_terms terms);
    balance_guaranteed_swap_builder(marketdata::conventions::convention_lookup&&,
                                    balance_guaranteed_swap_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    balance_guaranteed_swap_terms terms_;
};

/**
 * @brief A zero coupon inflation swap: a compounded fixed rate against the
 * growth of an inflation index, both paid at maturity.
 *
 * The inflation swap convention of the index supplies the calendar, the
 * adjustment, the day count, the observation lag and the interpolation.
 */
struct inflation_swap_terms final {
    required<utility::decimal::decimal> notional;
    required<std::string> currency;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    required<utility::decimal::decimal> fixed_rate;
    required<marketdata::datum::market_reference> index;
    /// The index level the growth is measured from.
    required<double> base_cpi;
    required<side> fixed_side;
};

class ORES_TRADING_API_EXPORT inflation_swap_builder final {
public:
    static constexpr std::string_view trade_type = "InflationSwap";
    inflation_swap_builder(conventions_ref conventions, inflation_swap_terms terms);
    inflation_swap_builder(marketdata::conventions::convention_lookup&&,
                           inflation_swap_terms) = delete;
    [[nodiscard]] swap_result build() const;

private:
    conventions_ref conventions_;
    inflation_swap_terms terms_;
};

}

#endif

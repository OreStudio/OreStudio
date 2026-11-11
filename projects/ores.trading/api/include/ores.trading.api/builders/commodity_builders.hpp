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
#ifndef ORES_TRADING_API_BUILDERS_COMMODITY_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_COMMODITY_BUILDERS_HPP

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
#include <string>
#include <string_view>
#include <vector>

/**
 * @file commodity_builders.hpp
 * @brief The commodity products the generator books.
 *
 * Every product names its commodity by an oresmd commodity address, which the
 * ORE projection writes as ORE names it. A product is in the currency its
 * commodity is quoted in, because ORE prices no quanto commodity cash flow.
 * An option settles in cash and exercises European. A leg prices on the future
 * settlement price and pays five days after its period on the NYSE calendar,
 * as ORE's commodity examples do. A caller that needs another choice extends
 * the terms.
 */
namespace ores::trading::builders {

using commodity_result = std::expected<domain::commodity_instrument_data, build_error>;

/// A quantity of one commodity bought or sold forward at a price.
struct commodity_forward_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<utility::decimal::decimal> price;
    required<std::chrono::year_month_day> maturity;
    required<option_position> position;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_forward_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityForward";
    explicit commodity_forward_builder(commodity_forward_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_forward_terms terms_;
};

/// An option on a quantity of one commodity, struck at a price.
struct commodity_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<option_kind> kind;
    required<utility::decimal::decimal> strike;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityOption";
    explicit commodity_option_builder(commodity_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_option_terms terms_;
};

/// An option on the commodity's arithmetic average price from a start to the expiry.
struct commodity_average_price_option_terms final {
    commodity_option_terms option;
    required<std::chrono::year_month_day> averaging_start;
};

class ORES_TRADING_API_EXPORT commodity_average_price_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityAveragePriceOption";
    explicit commodity_average_price_option_builder(commodity_average_price_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_average_price_option_terms terms_;
};

/// A strip of options, one per period of a floating leg, all at one strike.
struct commodity_option_strip_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<option_kind> kind;
    required<utility::decimal::decimal> strike;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    /// How often the strip rolls, as a schedule tenor such as 1M.
    required<std::string> roll_tenor;
    required<option_position> position;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_option_strip_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityOptionStrip";
    explicit commodity_option_strip_builder(commodity_option_strip_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_option_strip_terms terms_;
};

/// A monthly fixed-for-floating commodity swap on one quantity a period.
struct commodity_swap_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<utility::decimal::decimal> price;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    /// Whether the fixed price is paid or received.
    required<side> fixed_side;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_swap_builder final {
public:
    static constexpr std::string_view trade_type = "CommoditySwap";
    explicit commodity_swap_builder(commodity_swap_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_swap_terms terms_;
};

/// The right to enter a commodity swap on its expiry.
struct commodity_swaption_terms final {
    commodity_swap_terms swap;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT commodity_swaption_builder final {
public:
    static constexpr std::string_view trade_type = "CommoditySwaption";
    explicit commodity_swaption_builder(commodity_swaption_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_swaption_terms terms_;
};

/// A swap of the commodity's realised variance against a strike.
struct commodity_variance_swap_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    /// The strike as a variance, such as 0.04 for a twenty per cent volatility.
    required<double> variance_strike;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_variance_swap_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityVarianceSwap";
    explicit commodity_variance_swap_builder(commodity_variance_swap_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_variance_swap_terms terms_;
};

/// One commodity of a basket and its weight.
struct commodity_basket_member final {
    marketdata::datum::market_reference underlying;
    double weight = 0.0;
};

/// A vanilla option on a weighted basket of commodities.
struct commodity_basket_option_terms final {
    required<std::vector<commodity_basket_member>> basket;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    required<option_kind> kind;
    required<utility::decimal::decimal> strike;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_basket_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityBasketOption";
    explicit commodity_basket_option_builder(commodity_basket_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_basket_option_terms terms_;
};

}

#endif

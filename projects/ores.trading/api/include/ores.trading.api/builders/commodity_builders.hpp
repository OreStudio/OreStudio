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
};

class ORES_TRADING_API_EXPORT commodity_basket_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityBasketOption";
    explicit commodity_basket_option_builder(commodity_basket_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_basket_option_terms terms_;
};

/// An option that pays a fixed cash amount when the commodity ends beyond its strike.
struct commodity_digital_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    /// The cash the option pays when it ends in the money.
    required<utility::decimal::decimal> payoff;
    required<option_kind> kind;
    required<utility::decimal::decimal> strike;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT commodity_digital_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityDigitalOption";
    explicit commodity_digital_option_builder(commodity_digital_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_digital_option_terms terms_;
};

/// A digital option on the commodity's arithmetic average price from a start to the expiry.
struct commodity_digital_average_price_option_terms final {
    commodity_digital_option_terms option;
    required<std::chrono::year_month_day> averaging_start;
};

class ORES_TRADING_API_EXPORT commodity_digital_average_price_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityDigitalAveragePriceOption";
    explicit commodity_digital_average_price_option_builder(
        commodity_digital_average_price_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_digital_average_price_option_terms terms_;
};

/// A holding of a quantity of one commodity.
struct commodity_position_terms final {
    required<marketdata::datum::market_reference> underlying;
    /**
     * The currency the commodity is quoted in, which ORE values the position
     * in. ORE's position states no currency, so a position read from ORE has
     * none.
     */
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_position_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityPosition";
    explicit commodity_position_builder(commodity_position_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_position_terms terms_;
};

/**
 * @brief A strip of monthly options on the spread between two commodities'
 * prices, long the first and short the second.
 *
 * ORE requires both commodities to be quoted in the option's currency.
 */
struct commodity_spread_option_terms final {
    required<marketdata::datum::market_reference> long_underlying;
    required<marketdata::datum::market_reference> short_underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<option_kind> kind;
    /// The spread each option is struck at, which may be zero or negative.
    required<utility::decimal::decimal> spread_strike;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    required<option_position> position;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_spread_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommoditySpreadOption";
    explicit commodity_spread_option_builder(commodity_spread_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_spread_option_terms terms_;
};

/// Power bought or sold forward for a span of one delivery day.
struct intraday_power_forward_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<utility::decimal::decimal> price;
    required<std::chrono::year_month_day> maturity;
    required<std::chrono::year_month_day> delivery_date;
    /// The second of the delivery day the delivery starts at.
    required<int> delivery_start;
    /// The second of the delivery day the delivery ends at.
    required<int> delivery_end;
    required<option_position> position;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT intraday_power_forward_builder final {
public:
    static constexpr std::string_view trade_type = "IntradayPowerForward";
    explicit intraday_power_forward_builder(intraday_power_forward_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    intraday_power_forward_terms terms_;
};

/*
 * The scripted products on one commodity. ORE prices each with a script over
 * the commodity's observed prices, so each states the dates it observes as a
 * monthly schedule from its start to its expiry.
 */

/// An option on the commodity's arithmetic average price, observed monthly.
struct commodity_asian_option_terms final {
    commodity_option_terms option;
    required<std::chrono::year_month_day> averaging_start;
};

class ORES_TRADING_API_EXPORT commodity_asian_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityAsianOption";
    explicit commodity_asian_option_builder(commodity_asian_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_asian_option_terms terms_;
};

/**
 * @brief Buys a fixing amount of the commodity at the strike each month, twice
 * the amount when the price is below the strike, until the price rises to the
 * knock out.
 */
struct commodity_accumulator_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> fixing_amount;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> knock_out;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
    /// The unit the fixing amount counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_accumulator_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityAccumulator";
    explicit commodity_accumulator_builder(commodity_accumulator_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_accumulator_terms terms_;
};

/**
 * @brief Buys a fixing amount of the commodity at the strike each month, twice
 * the amount when the price is below the strike, until the gains reach the
 * target amount.
 */
struct commodity_tarf_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> fixing_amount;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> target_amount;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
    /// The unit the fixing amount counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_tarf_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityTaRF";
    explicit commodity_tarf_builder(commodity_tarf_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_tarf_terms terms_;
};

/// The way a barrier's level is crossed and what crossing it does.
enum class barrier_kind { up_and_in, up_and_out, down_and_in, down_and_out };

/**
 * @brief An option with one barrier on the commodity's price, monitored monthly
 * from a start to the expiry.
 */
struct commodity_generic_barrier_option_terms final {
    commodity_option_terms option;
    required<barrier_kind> barrier;
    required<utility::decimal::decimal> barrier_level;
    required<std::chrono::year_month_day> monitoring_start;
};

class ORES_TRADING_API_EXPORT commodity_generic_barrier_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityGenericBarrierOption";
    explicit commodity_generic_barrier_option_builder(commodity_generic_barrier_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_generic_barrier_option_terms terms_;
};

/// Whether a trigger fires when the price rises to it or falls to it.
enum class trigger_direction { up, down };

/**
 * @brief An option whose strike resets when the commodity's price reaches the
 * trigger on an observation date, observed monthly from a start to the expiry.
 */
struct commodity_strike_resettable_option_terms final {
    commodity_option_terms option;
    required<utility::decimal::decimal> reset_strike;
    required<trigger_direction> trigger;
    required<utility::decimal::decimal> trigger_price;
    required<std::chrono::year_month_day> observation_start;
    required<std::chrono::year_month_day> settlement;
    required<utility::decimal::decimal> premium;
    required<std::chrono::year_month_day> premium_date;
};

class ORES_TRADING_API_EXPORT commodity_strike_resettable_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityStrikeResettableOption";
    explicit commodity_strike_resettable_option_builder(
        commodity_strike_resettable_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_strike_resettable_option_terms terms_;
};

/**
 * @brief Agrees on the FVA date to buy or sell a straddle that expires later,
 * struck at the implied volatility.
 */
struct commodity_forward_volatility_agreement_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<std::chrono::year_month_day> fva_date;
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> settlement;
    required<std::chrono::year_month_day> premium_date;
    /// The volatility struck at, such as 0.3 for thirty per cent.
    required<double> implied_vol_strike;
    required<option_position> position;
    /// The unit the quantity counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_forward_volatility_agreement_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityForwardVolatilityAgreement";
    explicit commodity_forward_volatility_agreement_builder(
        commodity_forward_volatility_agreement_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_forward_volatility_agreement_terms terms_;
};

/*
 * The scripted products on a basket of commodities. A basket holds at least
 * two distinct commodities, each with a positive weight.
 */

/// What a rainbow option pays on its basket.
enum class rainbow_payoff {
    best_of_asset_or_cash,
    worst_of_asset_or_cash,
    max_rainbow,
    min_rainbow
};

/// An option on the best or worst performer of a basket of commodities.
struct commodity_rainbow_option_terms final {
    required<std::vector<commodity_basket_member>> basket;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    required<option_kind> kind;
    required<rainbow_payoff> payoff;
    required<utility::decimal::decimal> strike;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT commodity_rainbow_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityRainbowOption";
    explicit commodity_rainbow_option_builder(commodity_rainbow_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_rainbow_option_terms terms_;
};

/// A swap of a basket of commodities' realised variance, observed monthly, for a strike.
struct commodity_basket_variance_swap_terms final {
    required<std::vector<commodity_basket_member>> basket;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    /// The strike as a variance, such as 0.04 for a twenty per cent volatility.
    required<double> variance_strike;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    required<std::chrono::year_month_day> settlement;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT commodity_basket_variance_swap_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityBasketVarianceSwap";
    explicit commodity_basket_variance_swap_builder(commodity_basket_variance_swap_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_basket_variance_swap_terms terms_;
};

/**
 * @brief An option whose strike resets to a lower price observed monthly from
 * the strike date, when the price falls to the trigger level.
 *
 * The trigger level and the reset minimum are fractions of the initial price.
 */
struct commodity_best_entry_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> multiplier;
    required<utility::decimal::decimal> cap;
    required<utility::decimal::decimal> trigger_level;
    required<utility::decimal::decimal> reset_minimum;
    required<utility::decimal::decimal> premium;
    required<std::chrono::year_month_day> premium_date;
    required<std::chrono::year_month_day> strike_date;
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> settlement;
    required<option_position> position;
    /// The unit the notional counts, as the catalogue states it; empty when unstated.
    std::string unit;
};

class ORES_TRADING_API_EXPORT commodity_best_entry_option_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityBestEntryOption";
    explicit commodity_best_entry_option_builder(commodity_best_entry_option_terms terms);
    [[nodiscard]] commodity_result build() const;

private:
    commodity_best_entry_option_terms terms_;
};

/// A commodity in a worst-of basket, with the price its performance is measured from.
struct commodity_worst_of_member final {
    marketdata::datum::market_reference underlying;
    utility::decimal::decimal initial_price;
};

/**
 * @brief A swap that pays a floating funding leg against fixed coupons on the
 * worst performer of a basket of commodities, observed on its dates against a
 * knock out at par and a trigger at three quarters.
 */
struct commodity_worst_of_basket_swap_terms final {
    required<std::vector<commodity_worst_of_member>> basket;
    /// The end of each accrual period, ascending, after the start and no later than maturity.
    required<std::vector<std::chrono::year_month_day>> observation_dates;
    /// The index the funding leg floats on, in the basket's currency.
    required<marketdata::datum::market_reference> funding_index;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    /// The coupon rate paid while the basket stays above the trigger.
    required<utility::decimal::decimal> fixed_rate;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT commodity_worst_of_basket_swap_builder final {
public:
    static constexpr std::string_view trade_type = "CommodityWorstOfBasketSwap";
    commodity_worst_of_basket_swap_builder(
        const marketdata::conventions::convention_lookup& conventions,
        commodity_worst_of_basket_swap_terms terms);
    commodity_worst_of_basket_swap_builder(marketdata::conventions::convention_lookup&&,
                                           commodity_worst_of_basket_swap_terms) = delete;
    [[nodiscard]] commodity_result build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    commodity_worst_of_basket_swap_terms terms_;
};

}

#endif

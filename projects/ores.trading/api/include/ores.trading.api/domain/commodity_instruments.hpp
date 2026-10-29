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
#ifndef ORES_TRADING_API_DOMAIN_COMMODITY_INSTRUMENTS_HPP
#define ORES_TRADING_API_DOMAIN_COMMODITY_INSTRUMENTS_HPP

#include "ores.trading.api/domain/commodity_basket_constituent.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <array>
#include <chrono>
#include <cmath>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

/**
 * @brief The commodity family's named constructors.
 *
 * Twenty-five codes route to one table whose columns are almost all nullable
 * and whose checks tie no column to a trade type, so the table accepts a
 * forward that also states a barrier. Each constructor here states the columns
 * its product's shape carries and leaves the rest alone, so the mixture cannot
 * be assembled in the first place.
 *
 * Where two products state exactly the same columns they share a terms struct
 * and still get their own constructor, because the trade type is the only
 * thing that distinguishes them in storage. That is a limitation of the table,
 * not of this header.
 */
namespace ores::trading::domain {

/**
 * @brief The exact decimal the family's money fields carry.
 */
using decimal = ores::utility::decimal::decimal;

/**
 * @brief Refuses to construct a product from facts that do not state one.
 */
class commodity_product_error final : public std::invalid_argument {
public:
    using std::invalid_argument::invalid_argument;
};

namespace commodity_terms {

inline constexpr auto option_types = std::to_array<std::string_view>({"Call", "Put"});

inline constexpr auto exercise_types = std::to_array<std::string_view>({"European", "American"});

inline constexpr auto average_types = std::to_array<std::string_view>({"Arithmetic", "Geometric"});

inline constexpr auto barrier_types =
    std::to_array<std::string_view>({"UpAndIn", "UpAndOut", "DownAndIn", "DownAndOut"});

[[nodiscard]] inline bool states_one_of(std::string_view value,
                                        std::span<const std::string_view> allowed) {
    for (const auto candidate : allowed) {
        if (value == candidate)
            return true;
    }
    return false;
}

[[noreturn]] inline void refuse(std::string_view product, std::string_view fact) {
    throw commodity_product_error("A " + std::string(product) + " states " + std::string(fact) +
                                  ".");
}

inline void require_text(std::string_view product, std::string_view fact,
                         const std::string& value) {
    if (value.empty())
        refuse(product, fact);
}

inline void require_one_of(std::string_view product, std::string_view fact,
                           const std::string& value, std::span<const std::string_view> allowed) {
    if (!states_one_of(value, allowed))
        refuse(product, fact);
}

inline void require_positive(std::string_view product, std::string_view fact, double value) {
    if (!(value > 0.0) || !std::isfinite(value))
        refuse(product, fact);
}

inline void require_amount(std::string_view product, std::string_view fact, const decimal& value) {
    if (value < decimal{})
        refuse(product, fact);
}

inline void require_present(std::string_view product, std::string_view fact,
                            const std::optional<decimal>& value) {
    if (!value.has_value())
        refuse(product, fact);
    require_amount(product, fact, *value);
}

inline void require_date(std::string_view product, std::string_view fact,
                         const std::optional<std::chrono::year_month_day>& value) {
    if (!value.has_value() || *value == std::chrono::year_month_day{})
        refuse(product, fact);
}

}

/**
 * @brief The columns every commodity product states.
 */
struct commodity_common_terms final {
    std::string commodity_code;
    std::string currency;
    double quantity = 0.0;
    std::string unit;
    std::optional<std::chrono::year_month_day> start_date;
    std::optional<std::chrono::year_month_day> maturity_date;
};

/**
 * @brief A forward's or a position's own facts.
 */
struct commodity_forward_terms final {
    commodity_common_terms common;
    std::optional<decimal> fixed_price;
};

/**
 * @brief An option's own facts, as the table states them.
 */
struct commodity_option_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal strike_price;
    std::string exercise_type;
};

/**
 * @brief An average price option's own facts.
 */
struct commodity_average_price_option_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal strike_price;
    std::string average_type;
    std::optional<std::chrono::year_month_day> averaging_start_date;
    std::optional<std::chrono::year_month_day> averaging_end_date;
};

/**
 * @brief The facts one member of a commodity basket states.
 */
struct commodity_basket_member_terms final {
    std::string underlying_code;
    std::optional<double> weight;
};

/**
 * @brief An Asian option's own facts.
 *
 * The average is what makes it Asian: the window and the average type are
 * facts of the product, not optional fields a caller may leave out.
 */
struct commodity_asian_option_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal strike_price;
    std::string average_type;
    std::optional<std::chrono::year_month_day> averaging_start_date;
    std::optional<std::chrono::year_month_day> averaging_end_date;
};

/**
 * @brief A basket option's own facts.
 *
 * A basket option, a rainbow option and a best entry option are one shape in
 * this family: they state members and the option's own terms, and nothing the
 * family has tells them apart but the trade type.
 */
struct commodity_basket_option_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal strike_price;
    std::string exercise_type;
    std::vector<commodity_basket_member_terms> basket;
};

/**
 * @brief An option strip's own facts.
 */
struct commodity_option_strip_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal strike_price;
    std::string average_type;
    std::optional<std::chrono::year_month_day> averaging_start_date;
    std::optional<std::chrono::year_month_day> averaging_end_date;
    std::string strip_frequency_code;
};

/**
 * @brief A barrier option's own facts.
 */
struct commodity_barrier_option_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal strike_price;
    std::string barrier_type;
    decimal lower_barrier;
    std::optional<decimal> upper_barrier;
};

/**
 * @brief An accumulator's or a target redemption forward's own facts.
 *
 * The table states the same columns for both, so they share these terms and
 * differ only by the constructor that stamps the trade type.
 */
struct commodity_accumulation_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal accumulation_amount;
    std::optional<decimal> knock_out_barrier;
};

/**
 * @brief A spread option's own facts.
 */
struct commodity_spread_option_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal strike_price;
    std::string spread_commodity_code;
    decimal spread_amount;
};

/**
 * @brief A variance swap's own facts.
 */
struct commodity_variance_swap_terms final {
    commodity_common_terms common;
    double variance_strike = 0.0;
};

/**
 * @brief A swap's own facts.
 */
struct commodity_swap_terms final {
    commodity_common_terms common;
    std::optional<decimal> fixed_price;
};

/**
 * @brief A worst-of basket swap's own facts.
 */
struct commodity_worst_of_basket_swap_terms final {
    commodity_common_terms common;
    std::optional<decimal> fixed_price;
    std::vector<commodity_basket_member_terms> basket;
};

/**
 * @brief A commodity product and the rows its children state.
 */
struct commodity_basket_product final {
    commodity_instrument row;
    std::vector<commodity_basket_constituent> constituents;
};

/**
 * @brief A swaption's own facts.
 */
struct commodity_swaption_terms final {
    commodity_common_terms common;
    std::string option_type;
    decimal strike_price;
    std::optional<std::chrono::year_month_day> swaption_expiry_date;
};

namespace commodity_builders {

inline commodity_instrument
common_row(std::string_view product, const std::string& trade_type_code,
           const commodity_common_terms& common) {
    using namespace commodity_terms;
    require_text(product, "a commodity code", common.commodity_code);
    require_text(product, "a currency", common.currency);
    require_positive(product, "a positive quantity", common.quantity);
    require_text(product, "a unit", common.unit);
    require_date(product, "a start date", common.start_date);
    require_date(product, "a maturity date", common.maturity_date);

    commodity_instrument row;
    row.identity.trade_type_code = trade_type_code;
    row.commodity_code = common.commodity_code;
    row.currency = common.currency;
    row.quantity = common.quantity;
    row.unit = common.unit;
    row.start_date = common.start_date;
    row.maturity_date = common.maturity_date;
    return row;
}

/**
 * @brief Numbers the basket's members in the order they were given.
 *
 * A basket of fewer than two members is not a basket.
 */
inline std::vector<commodity_basket_constituent>
basket_rows(std::string_view product, const std::vector<commodity_basket_member_terms>& members) {
    using namespace commodity_terms;
    if (members.size() < 2)
        refuse(product, "at least two members, because one member is not a basket");
    std::vector<commodity_basket_constituent> rows;
    rows.reserve(members.size());
    int ordinal = 1;
    for (const auto& member : members) {
        require_text(product, "a code for each of its members", member.underlying_code);
        if (member.weight.has_value() && !(*member.weight > 0.0 && std::isfinite(*member.weight)))
            refuse(product, "a positive weight where it states one");
        commodity_basket_constituent row;
        row.sequence_number = ordinal++;
        row.underlying_code = member.underlying_code;
        row.weight = member.weight;
        rows.push_back(row);
    }
    return rows;
}

inline commodity_instrument
option_row(std::string_view product, const std::string& trade_type_code,
           const commodity_common_terms& common, const std::string& option_type,
           const decimal& strike_price, const std::string& exercise_type) {
    using namespace commodity_terms;
    auto row = common_row(product, trade_type_code, common);
    require_one_of(product, "a Call or Put option type", option_type, option_types);
    require_amount(product, "a non-negative strike price", strike_price);
    require_one_of(product, "a European or American exercise type", exercise_type, exercise_types);
    row.option_type = option_type;
    row.strike_price = strike_price;
    row.exercise_type = exercise_type;
    return row;
}

}

[[nodiscard]] inline commodity_instrument
construct_commodity_forward(const commodity_forward_terms& terms) {
    using namespace commodity_terms;
    auto row = commodity_builders::common_row("commodity forward", "CommodityForward",
                                              terms.common);
    if (terms.fixed_price.has_value())
        require_amount("commodity forward", "a non-negative price", *terms.fixed_price);
    row.fixed_price = terms.fixed_price;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_intraday_power_forward(const commodity_forward_terms& terms) {
    auto row = commodity_builders::common_row("intraday power forward", "IntradayPowerForward",
                                              terms.common);
    row.fixed_price = terms.fixed_price;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_forward_volatility_agreement(const commodity_forward_terms& terms) {
    auto row = commodity_builders::common_row("commodity forward volatility agreement",
                                              "CommodityForwardVolatilityAgreement", terms.common);
    row.fixed_price = terms.fixed_price;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_position(const commodity_forward_terms& terms) {
    auto row = commodity_builders::common_row("commodity position", "CommodityPosition",
                                              terms.common);
    row.fixed_price = terms.fixed_price;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_option(const commodity_option_terms& terms) {
    return commodity_builders::option_row("commodity option", "CommodityOption", terms.common,
                                          terms.option_type, terms.strike_price,
                                          terms.exercise_type);
}

[[nodiscard]] inline commodity_instrument
construct_commodity_digital_option(const commodity_option_terms& terms) {
    return commodity_builders::option_row("commodity digital option", "CommodityDigitalOption",
                                          terms.common, terms.option_type, terms.strike_price,
                                          terms.exercise_type);
}

[[nodiscard]] inline commodity_instrument
construct_commodity_asian_option(const commodity_asian_option_terms& terms) {
    using namespace commodity_terms;
    auto row = commodity_builders::common_row("commodity Asian option", "CommodityAsianOption",
                                              terms.common);
    require_one_of("commodity Asian option", "a Call or Put option type", terms.option_type,
                   option_types);
    require_amount("commodity Asian option", "a non-negative strike price", terms.strike_price);
    require_one_of("commodity Asian option", "an Arithmetic or Geometric average",
                   terms.average_type, average_types);
    require_date("commodity Asian option", "the start of its averaging",
                 terms.averaging_start_date);
    require_date("commodity Asian option", "the end of its averaging", terms.averaging_end_date);
    row.option_type = terms.option_type;
    row.strike_price = terms.strike_price;
    row.average_type = terms.average_type;
    row.averaging_start_date = terms.averaging_start_date;
    row.averaging_end_date = terms.averaging_end_date;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_strike_resettable_option(const commodity_option_terms& terms) {
    return commodity_builders::option_row("commodity strike resettable option",
                                          "CommodityStrikeResettableOption", terms.common,
                                          terms.option_type, terms.strike_price,
                                          terms.exercise_type);
}

namespace commodity_builders {

inline commodity_basket_product
basket_option_product(std::string_view product, const std::string& trade_type_code,
                      const commodity_basket_option_terms& terms) {
    commodity_basket_product built;
    built.row = option_row(product, trade_type_code, terms.common, terms.option_type,
                           terms.strike_price, terms.exercise_type);
    built.constituents = basket_rows(product, terms.basket);
    return built;
}

}

[[nodiscard]] inline commodity_basket_product
construct_commodity_basket_option(const commodity_basket_option_terms& terms) {
    return commodity_builders::basket_option_product("commodity basket option",
                                                     "CommodityBasketOption", terms);
}

[[nodiscard]] inline commodity_basket_product
construct_commodity_rainbow_option(const commodity_basket_option_terms& terms) {
    return commodity_builders::basket_option_product("commodity rainbow option",
                                                     "CommodityRainbowOption", terms);
}

[[nodiscard]] inline commodity_basket_product
construct_commodity_best_entry_option(const commodity_basket_option_terms& terms) {
    return commodity_builders::basket_option_product("commodity best entry option",
                                                     "CommodityBestEntryOption", terms);
}

namespace commodity_builders {

inline commodity_instrument
average_price_row(std::string_view product, const std::string& trade_type_code,
                  const commodity_average_price_option_terms& terms) {
    using namespace commodity_terms;
    auto row = common_row(product, trade_type_code, terms.common);
    require_one_of(product, "a Call or Put option type", terms.option_type, option_types);
    require_amount(product, "a non-negative strike price", terms.strike_price);
    require_one_of(product, "an Arithmetic or Geometric average", terms.average_type,
                   average_types);
    require_date(product, "the start of its averaging", terms.averaging_start_date);
    require_date(product, "the end of its averaging", terms.averaging_end_date);
    row.option_type = terms.option_type;
    row.strike_price = terms.strike_price;
    row.average_type = terms.average_type;
    row.averaging_start_date = terms.averaging_start_date;
    row.averaging_end_date = terms.averaging_end_date;
    return row;
}

}

[[nodiscard]] inline commodity_instrument
construct_commodity_average_price_option(const commodity_average_price_option_terms& terms) {
    return commodity_builders::average_price_row("commodity average price option",
                                                 "CommodityAveragePriceOption", terms);
}

[[nodiscard]] inline commodity_instrument
construct_commodity_digital_average_price_option(
    const commodity_average_price_option_terms& terms) {
    return commodity_builders::average_price_row("commodity digital average price option",
                                                 "CommodityDigitalAveragePriceOption", terms);
}

[[nodiscard]] inline commodity_instrument
construct_commodity_option_strip(const commodity_option_strip_terms& terms) {
    using namespace commodity_terms;
    commodity_average_price_option_terms shared{terms.common, terms.option_type,
                                                terms.strike_price, terms.average_type,
                                                terms.averaging_start_date,
                                                terms.averaging_end_date};
    auto row = commodity_builders::average_price_row("commodity option strip",
                                                     "CommodityOptionStrip", shared);
    require_text("commodity option strip", "the frequency its strip rolls at",
                 terms.strip_frequency_code);
    row.strip_frequency_code = terms.strip_frequency_code;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_window_barrier_option(const commodity_barrier_option_terms& terms) {
    using namespace commodity_terms;
    auto row = commodity_builders::common_row("commodity window barrier option",
                                              "CommodityWindowBarrierOption", terms.common);
    require_one_of("commodity window barrier option", "a Call or Put option type",
                   terms.option_type, option_types);
    require_amount("commodity window barrier option", "a non-negative strike price",
                   terms.strike_price);
    require_one_of("commodity window barrier option", "a barrier type from ORE's set",
                   terms.barrier_type, barrier_types);
    require_amount("commodity window barrier option", "a non-negative lower barrier",
                   terms.lower_barrier);
    row.option_type = terms.option_type;
    row.strike_price = terms.strike_price;
    row.barrier_type = terms.barrier_type;
    row.lower_barrier = terms.lower_barrier;
    row.upper_barrier = terms.upper_barrier;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_generic_barrier_option(const commodity_barrier_option_terms& terms) {
    using namespace commodity_terms;
    auto row = commodity_builders::common_row("commodity generic barrier option",
                                              "CommodityGenericBarrierOption", terms.common);
    require_one_of("commodity generic barrier option", "a Call or Put option type",
                   terms.option_type, option_types);
    require_amount("commodity generic barrier option", "a non-negative strike price",
                   terms.strike_price);
    require_one_of("commodity generic barrier option", "a barrier type from ORE's set",
                   terms.barrier_type, barrier_types);
    require_amount("commodity generic barrier option", "a non-negative lower barrier",
                   terms.lower_barrier);
    row.option_type = terms.option_type;
    row.strike_price = terms.strike_price;
    row.barrier_type = terms.barrier_type;
    row.lower_barrier = terms.lower_barrier;
    row.upper_barrier = terms.upper_barrier;
    return row;
}

namespace commodity_builders {

inline commodity_instrument
accumulation_row(std::string_view product, const std::string& trade_type_code,
                 const commodity_accumulation_terms& terms, bool with_target) {
    using namespace commodity_terms;
    auto row = common_row(product, trade_type_code, terms.common);
    require_one_of(product, "a Call or Put option type", terms.option_type, option_types);
    require_amount(product, "a non-negative accumulation amount", terms.accumulation_amount);
    if (terms.knock_out_barrier.has_value())
        require_amount(product, "a non-negative knock out barrier", *terms.knock_out_barrier);
    row.option_type = terms.option_type;
    row.accumulation_amount = terms.accumulation_amount;
    row.knock_out_barrier = terms.knock_out_barrier;
    (void)with_target;
    return row;
}

}

[[nodiscard]] inline commodity_instrument
construct_commodity_accumulator(const commodity_accumulation_terms& terms) {
    return commodity_builders::accumulation_row("commodity accumulator", "CommodityAccumulator",
                                                terms, false);
}

[[nodiscard]] inline commodity_instrument
construct_commodity_target_redemption_forward(const commodity_accumulation_terms& terms) {
    return commodity_builders::accumulation_row("commodity target redemption forward",
                                                "CommodityTaRF", terms, true);
}

[[nodiscard]] inline commodity_instrument
construct_commodity_spread_option(const commodity_spread_option_terms& terms) {
    using namespace commodity_terms;
    auto row = commodity_builders::common_row("commodity spread option", "CommoditySpreadOption",
                                              terms.common);
    require_one_of("commodity spread option", "a Call or Put option type", terms.option_type,
                   option_types);
    require_amount("commodity spread option", "a non-negative strike price", terms.strike_price);
    require_text("commodity spread option", "the second commodity it spreads against",
                 terms.spread_commodity_code);
    require_amount("commodity spread option", "a non-negative spread amount", terms.spread_amount);
    row.option_type = terms.option_type;
    row.strike_price = terms.strike_price;
    row.spread_commodity_code = terms.spread_commodity_code;
    row.spread_amount = terms.spread_amount;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_variance_swap(const commodity_variance_swap_terms& terms) {
    using namespace commodity_terms;
    auto row = commodity_builders::common_row("commodity variance swap", "CommodityVarianceSwap",
                                              terms.common);
    if (!(terms.variance_strike >= 0.0) || !std::isfinite(terms.variance_strike))
        refuse("commodity variance swap", "a finite, non-negative variance strike");
    row.variance_strike = terms.variance_strike;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_pairwise_variance_swap(const commodity_variance_swap_terms& terms) {
    auto row = commodity_builders::common_row("commodity pairwise variance swap",
                                              "CommodityPairwiseVarianceSwap", terms.common);
    row.variance_strike = terms.variance_strike;
    return row;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_basket_variance_swap(const commodity_variance_swap_terms& terms) {
    auto row = commodity_builders::common_row("commodity basket variance swap",
                                              "CommodityBasketVarianceSwap", terms.common);
    row.variance_strike = terms.variance_strike;
    return row;
}

[[nodiscard]] inline commodity_instrument construct_commodity_swap(const commodity_swap_terms& terms) {
    using namespace commodity_terms;
    auto row = commodity_builders::common_row("commodity swap", "CommoditySwap", terms.common);
    if (terms.fixed_price.has_value())
        require_amount("commodity swap", "a non-negative price", *terms.fixed_price);
    row.fixed_price = terms.fixed_price;
    return row;
}

[[nodiscard]] inline commodity_basket_product
construct_commodity_worst_of_basket_swap(const commodity_worst_of_basket_swap_terms& terms) {
    using namespace commodity_terms;
    commodity_basket_product built;
    built.row = commodity_builders::common_row("commodity worst-of basket swap",
                                               "CommodityWorstOfBasketSwap", terms.common);
    if (terms.fixed_price.has_value())
        require_amount("commodity worst-of basket swap", "a non-negative price",
                       *terms.fixed_price);
    built.row.fixed_price = terms.fixed_price;
    built.constituents = commodity_builders::basket_rows("commodity worst-of basket swap",
                                                         terms.basket);
    return built;
}

[[nodiscard]] inline commodity_instrument
construct_commodity_swaption(const commodity_swaption_terms& terms) {
    using namespace commodity_terms;
    auto row = commodity_builders::common_row("commodity swaption", "CommoditySwaption",
                                              terms.common);
    require_one_of("commodity swaption", "a Call or Put option type", terms.option_type,
                   option_types);
    require_amount("commodity swaption", "a non-negative strike price", terms.strike_price);
    require_date("commodity swaption", "the date it expires", terms.swaption_expiry_date);
    row.option_type = terms.option_type;
    row.strike_price = terms.strike_price;
    row.swaption_expiry_date = terms.swaption_expiry_date;
    return row;
}

}

#endif

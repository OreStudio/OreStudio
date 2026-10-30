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
#ifndef ORES_TRADING_API_DOMAIN_FX_INSTRUMENTS_HPP
#define ORES_TRADING_API_DOMAIN_FX_INSTRUMENTS_HPP

#include "ores.trading.api/domain/fx_accumulator_instrument.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
#include "ores.trading.api/domain/fx_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument.hpp"
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/range_bound.hpp"
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
 * @brief The fx family's named constructors.
 *
 * An fx row is the whole instrument: there is no fact table beside it, so the
 * table's own cross-column checks are the product's shape. They are mirrored
 * here, which is where they become refusals a caller cannot walk past: a
 * double barrier without two barriers, a digital without a strike, a taRF
 * without a target.
 */
namespace ores::trading::domain {

/**
 * @brief The exact decimal the family's amounts carry.
 */
using decimal = ores::utility::decimal::decimal;

/**
 * @brief Refuses to construct a product from facts that do not state one.
 */
class fx_product_error final : public std::invalid_argument {
public:
    using std::invalid_argument::invalid_argument;
};

namespace fx_terms {

inline constexpr auto directions = std::to_array<std::string_view>({"Long", "Short"});

inline constexpr auto option_types = std::to_array<std::string_view>({"Call", "Put"});

/// The barrier types the store fixes the upper barrier for.
inline constexpr auto upper_barrier_types =
    std::to_array<std::string_view>({"UpAndOut", "UpAndIn", "FixingCap"});

/// The barrier types the store fixes the lower barrier for.
inline constexpr auto lower_barrier_types =
    std::to_array<std::string_view>({"DownAndOut", "DownAndIn", "FixingFloor"});

[[nodiscard]] inline bool states_one_of(std::string_view value,
                                        std::span<const std::string_view> allowed) {
    for (const auto candidate : allowed) {
        if (value == candidate)
            return true;
    }
    return false;
}

[[noreturn]] inline void refuse(std::string_view product, std::string_view fact) {
    throw fx_product_error("An " + std::string(product) + " states " + std::string(fact) + ".");
}

inline void
require_text(std::string_view product, std::string_view fact, const std::string& value) {
    if (value.empty())
        refuse(product, fact);
}

inline void require_one_of(std::string_view product,
                           std::string_view fact,
                           const std::string& value,
                           std::span<const std::string_view> allowed) {
    if (!states_one_of(value, allowed))
        refuse(product, fact);
}

inline void require_money(std::string_view product, std::string_view fact, const decimal& value) {
    if (!(value > decimal{}))
        refuse(product, fact);
}

inline void
require_date(std::string_view product, std::string_view fact, std::chrono::year_month_day value) {
    if (value == std::chrono::year_month_day{})
        refuse(product, fact);
}

inline void require_positive_optional(std::string_view product,
                                      std::string_view fact,
                                      const std::optional<decimal>& value) {
    if (value.has_value() && !(*value > decimal{}))
        refuse(product, fact);
}

}

/**
 * @brief The two legs every fx product states: what is bought and what is sold.
 */
struct fx_legs_terms final {
    std::string bought_currency;
    decimal bought_amount;
    std::string sold_currency;
    decimal sold_amount;
};

/**
 * @brief An fx forward's or an fx swap's own facts.
 */
struct fx_forward_terms final {
    fx_legs_terms legs;
    std::chrono::year_month_day value_date;
    std::string settlement;
};

/**
 * @brief An fx vanilla option's own facts.
 */
struct fx_vanilla_option_terms final {
    fx_legs_terms legs;
    std::string option_type;
    std::chrono::year_month_day expiry_date;
    std::string exercise_style;
    std::string settlement;
};

/**
 * @brief An fx barrier option's own facts.
 *
 * Which barriers a product states is not a caller's choice: a double barrier
 * states both, and a single barrier states the one its type names.
 */
struct fx_barrier_option_terms final {
    fx_legs_terms legs;
    std::string option_type;
    std::chrono::year_month_day expiry_date;
    std::string settlement;
    std::string barrier_type;
    std::optional<decimal> lower_barrier;
    std::optional<decimal> upper_barrier;
    std::string underlying_code;
};

/**
 * @brief An fx digital or touch option's own facts.
 */
struct fx_digital_option_terms final {
    std::string foreign_currency;
    std::string domestic_currency;
    std::string payoff_currency;
    decimal payoff_amount;
    std::string option_type;
    std::chrono::year_month_day expiry_date;
    std::string long_short;
    std::optional<decimal> strike;
    std::string barrier_type;
    std::optional<decimal> lower_barrier;
    std::optional<decimal> upper_barrier;
};

/**
 * @brief An fx average forward's own facts.
 */
struct fx_asian_forward_terms final {
    std::string fx_index;
    std::string reference_currency;
    std::optional<decimal> reference_notional;
    std::string settlement_currency;
    std::optional<decimal> settlement_notional;
    std::optional<std::chrono::year_month_day> payment_date;
    std::string long_short;
    std::string currency;
    std::optional<decimal> fixing_amount;
    std::optional<decimal> strike;
};

/**
 * @brief An fx target redemption forward's own facts.
 *
 * The target is what the product exists to redeem against, so it is required
 * here and absent from the average forward.
 */
struct fx_tarf_terms final {
    std::string fx_index;
    std::string reference_currency;
    std::optional<decimal> reference_notional;
    std::string settlement_currency;
    std::optional<decimal> settlement_notional;
    std::optional<std::chrono::year_month_day> payment_date;
    std::string long_short;
    std::string currency;
    std::optional<decimal> fixing_amount;
    std::optional<decimal> strike;
    decimal target_amount;
};

/**
 * @brief An fx variance swap's own facts.
 */
struct fx_variance_swap_terms final {
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day end_date;
    std::string currency;
    std::string underlying_code;
    std::string long_short;
    double strike = 0.0;
    decimal notional;
    std::string moment_type;
};

/**
 * @brief An fx accumulator's own facts.
 */
struct fx_accumulator_terms final {
    std::string currency;
    decimal fixing_amount;
    std::string underlying_code;
    std::string long_short;
    std::chrono::year_month_day start_date;
    decimal strike;
    std::optional<decimal> knock_out_barrier;
};

/**
 * @brief An fx accumulator and the range bound it states.
 */
struct fx_accumulator_product final {
    fx_accumulator_instrument row;
    std::vector<range_bound> range_bounds;
};

namespace fx_builders {

inline void require_legs(std::string_view product, const fx_legs_terms& legs) {
    using namespace fx_terms;
    require_text(product, "a bought currency", legs.bought_currency);
    require_text(product, "a sold currency", legs.sold_currency);
    require_money(product, "a positive bought amount", legs.bought_amount);
    require_money(product, "a positive sold amount", legs.sold_amount);
}

template <typename Row>
inline Row legs_row(std::string_view product, const fx_legs_terms& legs) {
    require_legs(product, legs);
    Row row;
    row.bought_currency = legs.bought_currency;
    row.bought_amount = legs.bought_amount;
    row.sold_currency = legs.sold_currency;
    row.sold_amount = legs.sold_amount;
    return row;
}

/**
 * @brief The barrier rules the store states, as refusals.
 */
inline void require_barrier_shape(std::string_view product,
                                  bool double_barrier,
                                  const std::string& barrier_type,
                                  const std::optional<decimal>& lower,
                                  const std::optional<decimal>& upper) {
    using namespace fx_terms;
    require_positive_optional(product, "a positive lower barrier", lower);
    require_positive_optional(product, "a positive upper barrier", upper);
    if (!lower.has_value() && !upper.has_value())
        refuse(product, "a barrier, because a barrier product has one");
    if (double_barrier && !(lower.has_value() && upper.has_value()))
        refuse(product, "two barriers, because it is a double barrier");
    if (states_one_of(barrier_type, upper_barrier_types) && !upper.has_value())
        refuse(product, "an upper barrier for the barrier type it names");
    if (states_one_of(barrier_type, lower_barrier_types) && !lower.has_value())
        refuse(product, "a lower barrier for the barrier type it names");
}

}

[[nodiscard]] inline fx_forward_instrument construct_fx_forward(const fx_forward_terms& terms) {
    using namespace fx_terms;
    auto row = fx_builders::legs_row<fx_forward_instrument>("fx forward", terms.legs);
    require_date("fx forward", "a value date", terms.value_date);
    row.identity.trade_type_code = "FxForward";
    row.value_date = terms.value_date;
    row.settlement = terms.settlement;
    return row;
}

[[nodiscard]] inline fx_forward_instrument construct_fx_swap(const fx_forward_terms& terms) {
    using namespace fx_terms;
    auto row = fx_builders::legs_row<fx_forward_instrument>("fx swap", terms.legs);
    require_date("fx swap", "a value date", terms.value_date);
    if (terms.legs.bought_currency == terms.legs.sold_currency)
        refuse("fx swap", "two different currencies");
    row.identity.trade_type_code = "FxSwap";
    row.value_date = terms.value_date;
    row.settlement = terms.settlement;
    return row;
}

[[nodiscard]] inline fx_vanilla_option_instrument
construct_fx_option(const fx_vanilla_option_terms& terms) {
    using namespace fx_terms;
    auto row = fx_builders::legs_row<fx_vanilla_option_instrument>("fx option", terms.legs);
    require_one_of("fx option", "a Call or Put option type", terms.option_type, option_types);
    require_date("fx option", "an expiry date", terms.expiry_date);
    require_text("fx option", "an exercise style", terms.exercise_style);
    row.identity.trade_type_code = "FxOption";
    row.option_type = terms.option_type;
    row.expiry_date = terms.expiry_date;
    row.exercise_style = terms.exercise_style;
    row.settlement = terms.settlement;
    return row;
}

namespace fx_builders {

inline fx_barrier_option_instrument barrier_row(std::string_view product,
                                                const std::string& trade_type_code,
                                                bool double_barrier,
                                                const fx_barrier_option_terms& terms) {
    using namespace fx_terms;
    auto row = legs_row<fx_barrier_option_instrument>(product, terms.legs);
    require_one_of(product, "a Call or Put option type", terms.option_type, option_types);
    require_date(product, "an expiry date", terms.expiry_date);
    require_barrier_shape(
        product, double_barrier, terms.barrier_type, terms.lower_barrier, terms.upper_barrier);
    row.identity.trade_type_code = trade_type_code;
    row.option_type = terms.option_type;
    row.expiry_date = terms.expiry_date;
    row.settlement = terms.settlement;
    row.barrier_type = terms.barrier_type;
    row.lower_barrier = terms.lower_barrier;
    row.upper_barrier = terms.upper_barrier;
    row.underlying_code = terms.underlying_code;
    return row;
}

}

[[nodiscard]] inline fx_barrier_option_instrument
construct_fx_barrier_option(const fx_barrier_option_terms& terms) {
    return fx_builders::barrier_row("fx barrier option", "FxBarrierOption", false, terms);
}

[[nodiscard]] inline fx_barrier_option_instrument
construct_fx_double_barrier_option(const fx_barrier_option_terms& terms) {
    return fx_builders::barrier_row(
        "fx double barrier option", "FxDoubleBarrierOption", true, terms);
}

[[nodiscard]] inline fx_barrier_option_instrument
construct_fx_european_barrier_option(const fx_barrier_option_terms& terms) {
    return fx_builders::barrier_row(
        "fx European barrier option", "FxEuropeanBarrierOption", false, terms);
}

[[nodiscard]] inline fx_barrier_option_instrument
construct_fx_generic_barrier_option(const fx_barrier_option_terms& terms) {
    return fx_builders::barrier_row(
        "fx generic barrier option", "FxGenericBarrierOption", false, terms);
}

[[nodiscard]] inline fx_barrier_option_instrument
construct_fx_kiko_barrier_option(const fx_barrier_option_terms& terms) {
    return fx_builders::barrier_row("fx KIKO barrier option", "FxKIKOBarrierOption", false, terms);
}

namespace fx_builders {

inline fx_digital_option_instrument digital_row(std::string_view product,
                                                const std::string& trade_type_code,
                                                bool needs_strike,
                                                bool needs_two_barriers,
                                                const fx_digital_option_terms& terms) {
    using namespace fx_terms;
    require_text(product, "a foreign currency", terms.foreign_currency);
    require_text(product, "a domestic currency", terms.domestic_currency);
    require_text(product, "a payoff currency", terms.payoff_currency);
    require_money(product, "a positive payoff amount", terms.payoff_amount);
    require_date(product, "an expiry date", terms.expiry_date);
    require_one_of(product, "a Long or Short direction", terms.long_short, directions);
    if (needs_strike && !terms.strike.has_value())
        refuse(product, "a strike, because a digital pays on one");
    require_positive_optional(product, "a positive strike", terms.strike);
    require_positive_optional(product, "a positive lower barrier", terms.lower_barrier);
    require_positive_optional(product, "a positive upper barrier", terms.upper_barrier);
    if (needs_two_barriers) {
        if (!terms.lower_barrier.has_value() || !terms.upper_barrier.has_value())
            refuse(product, "two barriers, because it is touched twice");
    } else if (needs_strike == false && !terms.lower_barrier.has_value() &&
               !terms.upper_barrier.has_value()) {
        refuse(product, "a barrier, because a touch is touched");
    }

    fx_digital_option_instrument row;
    row.identity.trade_type_code = trade_type_code;
    row.foreign_currency = terms.foreign_currency;
    row.domestic_currency = terms.domestic_currency;
    row.payoff_currency = terms.payoff_currency;
    row.payoff_amount = terms.payoff_amount;
    row.option_type = terms.option_type;
    row.expiry_date = terms.expiry_date;
    row.long_short = terms.long_short;
    row.strike = terms.strike;
    row.barrier_type = terms.barrier_type;
    row.lower_barrier = terms.lower_barrier;
    row.upper_barrier = terms.upper_barrier;
    return row;
}

}

[[nodiscard]] inline fx_digital_option_instrument
construct_fx_digital_option(const fx_digital_option_terms& terms) {
    return fx_builders::digital_row("fx digital option", "FxDigitalOption", true, false, terms);
}

[[nodiscard]] inline fx_digital_option_instrument
construct_fx_touch_option(const fx_digital_option_terms& terms) {
    return fx_builders::digital_row("fx touch option", "FxTouchOption", false, false, terms);
}

[[nodiscard]] inline fx_digital_option_instrument
construct_fx_double_touch_option(const fx_digital_option_terms& terms) {
    return fx_builders::digital_row(
        "fx double touch option", "FxDoubleTouchOption", false, true, terms);
}

[[nodiscard]] inline fx_digital_option_instrument
construct_fx_digital_barrier_option(const fx_digital_option_terms& terms) {
    return fx_builders::digital_row(
        "fx digital barrier option", "FxDigitalBarrierOption", true, false, terms);
}

namespace fx_builders {

inline fx_asian_forward_instrument asian_row(std::string_view product,
                                             const std::string& trade_type_code,
                                             const fx_asian_forward_terms& terms) {
    using namespace fx_terms;
    require_text(product, "an fx index", terms.fx_index);
    require_text(product, "a reference currency", terms.reference_currency);
    require_text(product, "a settlement currency", terms.settlement_currency);
    require_text(product, "a currency", terms.currency);
    require_one_of(product, "a Long or Short direction", terms.long_short, directions);

    fx_asian_forward_instrument row;
    row.identity.trade_type_code = trade_type_code;
    row.fx_index = terms.fx_index;
    row.reference_currency = terms.reference_currency;
    row.reference_notional = terms.reference_notional;
    row.settlement_currency = terms.settlement_currency;
    row.settlement_notional = terms.settlement_notional;
    row.payment_date = terms.payment_date;
    row.long_short = terms.long_short;
    row.currency = terms.currency;
    row.fixing_amount = terms.fixing_amount;
    row.strike = terms.strike;
    return row;
}

}

[[nodiscard]] inline fx_asian_forward_instrument
construct_fx_average_forward(const fx_asian_forward_terms& terms) {
    return fx_builders::asian_row("fx average forward", "FxAverageForward", terms);
}

[[nodiscard]] inline fx_asian_forward_instrument
construct_fx_target_redemption_forward(const fx_tarf_terms& terms) {
    using namespace fx_terms;
    fx_asian_forward_terms shared{terms.fx_index,
                                  terms.reference_currency,
                                  terms.reference_notional,
                                  terms.settlement_currency,
                                  terms.settlement_notional,
                                  terms.payment_date,
                                  terms.long_short,
                                  terms.currency,
                                  terms.fixing_amount,
                                  terms.strike};
    auto row = fx_builders::asian_row("fx target redemption forward", "FxTaRF", shared);
    require_money("fx target redemption forward", "a positive target amount", terms.target_amount);
    row.target_amount = terms.target_amount;
    return row;
}

[[nodiscard]] inline fx_variance_swap_instrument
construct_fx_variance_swap(const fx_variance_swap_terms& terms) {
    using namespace fx_terms;
    require_date("fx variance swap", "a start date", terms.start_date);
    require_date("fx variance swap", "an end date", terms.end_date);
    if (!(terms.end_date > terms.start_date))
        refuse("fx variance swap", "an end date after its start");
    require_text("fx variance swap", "a currency", terms.currency);
    require_text("fx variance swap", "an underlying", terms.underlying_code);
    require_one_of("fx variance swap", "a Long or Short direction", terms.long_short, directions);
    require_money("fx variance swap", "a positive notional", terms.notional);
    if (!(terms.strike > 0.0) || !std::isfinite(terms.strike))
        refuse("fx variance swap", "a finite, positive strike");

    fx_variance_swap_instrument row;
    row.identity.trade_type_code = "FxVarianceSwap";
    row.start_date = terms.start_date;
    row.end_date = terms.end_date;
    row.currency = terms.currency;
    row.underlying_code = terms.underlying_code;
    row.long_short = terms.long_short;
    row.strike = terms.strike;
    row.notional = terms.notional;
    row.moment_type = terms.moment_type;
    return row;
}

[[nodiscard]] inline fx_accumulator_product
construct_fx_accumulator(const fx_accumulator_terms& terms) {
    using namespace fx_terms;
    require_text("fx accumulator", "a currency", terms.currency);
    require_money("fx accumulator", "a positive fixing amount", terms.fixing_amount);
    require_text("fx accumulator", "an underlying", terms.underlying_code);
    require_one_of("fx accumulator", "a Long or Short direction", terms.long_short, directions);
    require_date("fx accumulator", "a start date", terms.start_date);
    require_money("fx accumulator", "a positive strike", terms.strike);
    require_positive_optional(
        "fx accumulator", "a positive knock out barrier", terms.knock_out_barrier);

    fx_accumulator_product built;
    built.row.identity.trade_type_code = "FxAccumulator";
    built.row.currency = terms.currency;
    built.row.fixing_amount = terms.fixing_amount;
    built.row.underlying_code = terms.underlying_code;
    built.row.long_short = terms.long_short;
    built.row.start_date = terms.start_date;
    built.row.strike = terms.strike;
    built.row.knock_out_barrier = terms.knock_out_barrier;

    range_bound bound;
    bound.sequence_number = 1;
    bound.start_date = terms.start_date;
    bound.range_from = terms.strike;
    bound.range_to = terms.knock_out_barrier;
    bound.strike = terms.strike;
    built.range_bounds.push_back(bound);
    return built;
}

}

#endif

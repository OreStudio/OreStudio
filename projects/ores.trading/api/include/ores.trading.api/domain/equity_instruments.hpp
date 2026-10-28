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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_INSTRUMENTS_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_INSTRUMENTS_HPP

#include "ores.trading.api/domain/equity_accumulator_instrument.hpp"
#include "ores.trading.api/domain/equity_asian_option_instrument.hpp"
#include "ores.trading.api/domain/equity_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/equity_digital_option_instrument.hpp"
#include "ores.trading.api/domain/equity_forward_instrument.hpp"
#include "ores.trading.api/domain/equity_option_instrument.hpp"
#include "ores.trading.api/domain/equity_position_instrument.hpp"
#include "ores.trading.api/domain/equity_swap_instrument.hpp"
#include "ores.trading.api/domain/equity_variance_swap_instrument.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <array>
#include <chrono>
#include <cmath>
#include <initializer_list>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>

/**
 * @brief The equity family's named constructors.
 *
 * A product is constructed, never assembled. These functions take the facts a
 * product states and return the row the table accepts, so the rules the check
 * constraints state are refused before a write, and the trade type code is
 * fixed by the construction rather than passed alongside it. The SQL stays as
 * the backstop it should always have been.
 *
 * Two codes that share a table cannot borrow each other's columns: the digital
 * and the touch take different terms, the accumulator and the target
 * redemption forward differ by whether a target exists, and the barrier's
 * three codes differ by whether a second barrier exists.
 *
 * Child rows -- a swap's constituents and periods, an accumulator's range
 * bounds -- stay with the caller for now.
 */
namespace ores::trading::domain {

/**
 * @brief The exact decimal the family's money fields carry.
 */
using decimal = ores::utility::decimal::decimal;

/**
 * @brief Refuses to construct a product from facts that do not state one.
 */
class equity_product_error final : public std::invalid_argument {
public:
    using std::invalid_argument::invalid_argument;
};

namespace equity_terms {

inline constexpr auto option_types = std::to_array<std::string_view>({"Call", "Put"});

inline constexpr auto exercise_types =
    std::to_array<std::string_view>({"European", "Bermudan", "American"});

inline constexpr auto settlement_types = std::to_array<std::string_view>({"Cash", "Physical"});

inline constexpr auto direction_types = std::to_array<std::string_view>({"Long", "Short"});

inline constexpr auto barrier_types = std::to_array<std::string_view>(
    {"UpAndOut", "UpAndIn", "DownAndOut", "DownAndIn", "KnockIn", "KnockOut",
     "CumulatedProfitCap", "CumulatedProfitCapPoints", "FixingCap", "FixingFloor"});

inline constexpr auto return_types = std::to_array<std::string_view>({"Total", "Price"});

inline constexpr auto payoff_types = std::to_array<std::string_view>(
    {"Accumulator", "Asian", "AverageStrike", "Decumulator", "TargetExact", "TargetFull",
     "Vanilla"});

inline constexpr auto target_types = std::to_array<std::string_view>({"TargetFull", "TargetExact"});

[[nodiscard]] inline bool states_one_of(std::string_view value,
                                        std::span<const std::string_view> allowed) {
    for (const auto candidate : allowed) {
        if (value == candidate)
            return true;
    }
    return false;
}

/**
 * @brief Names the fact that is missing, so the throw says what to fix.
 */
[[noreturn]] inline void refuse(std::string_view product, std::string_view fact) {
    throw equity_product_error("An " + std::string(product) + " states " + std::string(fact) + ".");
}

inline void require_text(std::string_view product, std::string_view fact,
                         const std::string& value) {
    if (value.empty())
        refuse(product, fact);
}

inline void require_one_of(std::string_view product, std::string_view fact,
                           const std::string& value,
                           std::span<const std::string_view> allowed) {
    if (!states_one_of(value, allowed))
        refuse(product, fact);
}

inline void require_one_of_or_absent(std::string_view product, std::string_view fact,
                                     const std::string& value,
                                     std::span<const std::string_view> allowed) {
    if (!value.empty() && !states_one_of(value, allowed))
        refuse(product, fact);
}

inline void require_money(std::string_view product, std::string_view fact, const decimal& value) {
    if (!(value > decimal{}))
        refuse(product, fact);
}

inline void require_not_negative(std::string_view product, std::string_view fact,
                                 const decimal& value) {
    if (value < decimal{})
        refuse(product, fact);
}

inline void require_date(std::string_view product, std::string_view fact,
                         std::chrono::year_month_day value) {
    if (value == std::chrono::year_month_day{})
        refuse(product, fact);
}

inline void require_positive_quantity(std::string_view product, double value) {
    if (!(value > 0.0) || !std::isfinite(value))
        refuse(product, "a positive quantity");
}

inline void require_moment(std::string_view product, double value) {
    if (!(value >= 0.0) || !std::isfinite(value))
        refuse(product, "a finite, non-negative strike");
}

}

/**
 * @brief The facts an equity option states.
 */
struct equity_option_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    std::string option_type;
    decimal strike;
    std::chrono::year_month_day expiry_date;
    std::string exercise_type;
    std::string long_short;
    std::string settlement_type;
};

/**
 * @brief The facts an equity cliquet option states.
 *
 * The frequency has no default and no empty meaning: a cliquet observes its
 * own returns over periods, so a cliquet without a frequency is not a cliquet.
 */
struct equity_cliquet_option_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    std::string option_type;
    decimal strike;
    std::chrono::year_month_day expiry_date;
    std::string exercise_type;
    std::string long_short;
    std::string settlement_type;
    std::string frequency;
};

/**
 * @brief The facts an equity Asian option states.
 */
struct equity_asian_option_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    std::string option_type;
    decimal strike;
    std::chrono::year_month_day expiry_date;
    std::string exercise_type;
    std::string long_short;
    std::string average_type;
    std::chrono::year_month_day averaging_start_date;
    std::chrono::year_month_day averaging_end_date;
};

/**
 * @brief The facts a single-barrier equity option states.
 */
struct equity_barrier_option_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    std::string option_type;
    decimal strike;
    std::chrono::year_month_day expiry_date;
    std::string exercise_type;
    std::string long_short;
    decimal lower_barrier;
    std::string lower_barrier_type;
    std::optional<decimal> rebate;
};

/**
 * @brief The facts an equity double barrier option states.
 *
 * The second barrier is a fact of this product, not an optional one, so the
 * type states it rather than leaving a nullable field to remember.
 */
struct equity_double_barrier_option_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    std::string option_type;
    decimal strike;
    std::chrono::year_month_day expiry_date;
    std::string exercise_type;
    std::string long_short;
    decimal lower_barrier;
    std::string lower_barrier_type;
    decimal upper_barrier;
    std::string upper_barrier_type;
    std::optional<decimal> rebate;
};

/**
 * @brief The facts an equity digital option states.
 *
 * A digital states an option type and a strike and no barrier; the check
 * constraint refuses the mixture, and this type has nowhere to put a barrier.
 */
struct equity_digital_option_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    std::string option_type;
    decimal strike;
    std::chrono::year_month_day expiry_date;
    std::string long_short;
    std::optional<decimal> payout_amount;
};

/**
 * @brief The facts an equity touch option states.
 *
 * The mirror of the digital: a barrier and no strike.
 */
struct equity_touch_option_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    decimal barrier_level;
    std::string barrier_type;
    std::chrono::year_month_day expiry_date;
    std::string long_short;
    std::optional<decimal> payout_amount;
};

/**
 * @brief The facts an equity forward states.
 */
struct equity_forward_terms final {
    std::string underlying_name;
    std::string currency;
    double quantity = 0.0;
    std::optional<decimal> forward_price;
    std::chrono::year_month_day expiry_date;
    std::string long_short;
    std::string settlement_type;
};

/**
 * @brief The facts an equity swap states, other than its children.
 */
struct equity_swap_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    std::string return_type;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day maturity_date;
    std::string long_short;
    std::string payment_frequency_code;
};

/**
 * @brief The facts a worst-of basket swap states.
 *
 * The swap states no single underlying, because its constituents name the
 * members; the check constraint refuses one.
 */
struct equity_basket_swap_terms final {
    std::string currency;
    decimal notional;
    std::string return_type;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day maturity_date;
    std::string long_short;
    std::string payment_frequency_code;
};

/**
 * @brief The facts an equity variance swap states.
 */
struct equity_variance_swap_terms final {
    std::string underlying_name;
    std::string currency;
    decimal notional;
    double variance_strike = 0.0;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day maturity_date;
    std::string long_short;
};

/**
 * @brief The facts an equity accumulator states.
 *
 * An accumulator states no target: the check constraint refuses one, and the
 * type has nowhere to put it.
 */
struct equity_accumulator_terms final {
    std::string underlying_name;
    std::string currency;
    decimal strike;
    decimal fixing_amount;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day expiry_date;
    std::string fixing_frequency;
    std::string long_short;
    std::optional<decimal> knock_out_level;
    std::string payoff_type;
};

/**
 * @brief The facts an equity target redemption forward states.
 *
 * The target is required here and absent from the accumulator, which is the
 * difference the check constraint states.
 */
struct equity_tarf_terms final {
    std::string underlying_name;
    std::string currency;
    decimal strike;
    decimal fixing_amount;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day expiry_date;
    std::string fixing_frequency;
    std::string long_short;
    std::optional<decimal> knock_out_level;
    std::string payoff_type;
    decimal target_amount;
    std::string target_type;
};

/**
 * @brief The facts an equity position states.
 */
struct equity_position_terms final {
    std::string underlying_name;
    std::string currency;
    double quantity = 0.0;
    std::optional<decimal> price;
};

[[nodiscard]] inline equity_option_instrument
construct_equity_option(const equity_option_terms& terms) {
    using namespace equity_terms;
    require_text("equity option", "an underlying name", terms.underlying_name);
    require_text("equity option", "a currency", terms.currency);
    require_money("equity option", "a positive notional", terms.notional);
    require_one_of("equity option", "a Call or Put option type", terms.option_type,
                   option_types);
    require_not_negative("equity option", "a non-negative strike", terms.strike);
    require_date("equity option", "an expiry date", terms.expiry_date);
    require_one_of("equity option", "a European, Bermudan or American exercise type",
                   terms.exercise_type,
                   exercise_types);
    require_one_of("equity option", "a Long or Short direction", terms.long_short,
                   direction_types);
    require_one_of_or_absent("equity option",
                             "a Cash or Physical settlement type where it states one",
                             terms.settlement_type,
                             settlement_types);

    equity_option_instrument row;
    row.identity.trade_type_code = "EquityOption";
    row.underlying_name = terms.underlying_name;
    row.currency = terms.currency;
    row.notional = terms.notional;
    row.option_type = terms.option_type;
    row.strike = terms.strike;
    row.expiry_date = terms.expiry_date;
    row.exercise_type = terms.exercise_type;
    row.long_short = terms.long_short;
    row.settlement_type = terms.settlement_type;
    return row;
}

[[nodiscard]] inline equity_option_instrument
construct_equity_cliquet_option(const equity_cliquet_option_terms& terms) {
    using namespace equity_terms;
    require_text("equity cliquet option", "an underlying name", terms.underlying_name);
    require_text("equity cliquet option", "a currency", terms.currency);
    require_money("equity cliquet option", "a positive notional", terms.notional);
    require_one_of("equity cliquet option", "a Call or Put option type", terms.option_type,
                   option_types);
    require_not_negative("equity cliquet option", "a non-negative strike", terms.strike);
    require_date("equity cliquet option", "an expiry date", terms.expiry_date);
    require_one_of("equity cliquet option", "a European, Bermudan or American exercise type",
                   terms.exercise_type,
                   exercise_types);
    require_one_of("equity cliquet option", "a Long or Short direction", terms.long_short,
                   direction_types);
    require_one_of_or_absent("equity cliquet option",
                             "a Cash or Physical settlement type where it states one",
                             terms.settlement_type,
                             settlement_types);
    require_text("equity cliquet option", "a frequency, because that is how often it observes",
                 terms.frequency);

    equity_option_instrument row;
    row.identity.trade_type_code = "EquityCliquetOption";
    row.underlying_name = terms.underlying_name;
    row.currency = terms.currency;
    row.notional = terms.notional;
    row.option_type = terms.option_type;
    row.strike = terms.strike;
    row.expiry_date = terms.expiry_date;
    row.exercise_type = terms.exercise_type;
    row.long_short = terms.long_short;
    row.settlement_type = terms.settlement_type;
    row.cliquet_frequency = terms.frequency;
    return row;
}

[[nodiscard]] inline equity_asian_option_instrument
construct_equity_asian_option(const equity_asian_option_terms& terms) {
    using namespace equity_terms;
    require_text("equity Asian option", "an underlying name", terms.underlying_name);
    require_text("equity Asian option", "a currency", terms.currency);
    require_money("equity Asian option", "a positive notional", terms.notional);
    require_one_of("equity Asian option", "a Call or Put option type", terms.option_type,
                   option_types);
    require_not_negative("equity Asian option", "a non-negative strike", terms.strike);
    require_date("equity Asian option", "an expiry date", terms.expiry_date);
    require_one_of("equity Asian option", "a European, Bermudan or American exercise type",
                   terms.exercise_type,
                   exercise_types);
    require_one_of("equity Asian option", "a Long or Short direction", terms.long_short,
                   direction_types);
    require_text("equity Asian option", "the average it takes", terms.average_type);
    require_date("equity Asian option", "the start of its averaging", terms.averaging_start_date);
    require_date("equity Asian option", "the end of its averaging", terms.averaging_end_date);

    equity_asian_option_instrument row;
    row.identity.trade_type_code = "EquityAsianOption";
    row.underlying_name = terms.underlying_name;
    row.currency = terms.currency;
    row.notional = terms.notional;
    row.option_type = terms.option_type;
    row.strike = terms.strike;
    row.expiry_date = terms.expiry_date;
    row.exercise_type = terms.exercise_type;
    row.long_short = terms.long_short;
    row.average_type = terms.average_type;
    row.averaging_start_date = terms.averaging_start_date;
    row.averaging_end_date = terms.averaging_end_date;
    return row;
}

namespace equity_builders {

/**
 * @brief Builds the barrier row's shared facts.
 *
 * The barriers stay with the caller, so the single and double forms state
 * exactly the barriers their product has.
 */
inline equity_barrier_option_instrument
barrier_common(std::string_view product, const std::string& trade_type_code,
               const std::string& underlying_name, const std::string& currency,
               const decimal& notional, const std::string& option_type, const decimal& strike,
               std::chrono::year_month_day expiry_date, const std::string& exercise_type,
               const std::string& long_short) {
    using namespace equity_terms;
    require_text(product, "an underlying name", underlying_name);
    require_text(product, "a currency", currency);
    require_money(product, "a positive notional", notional);
    require_one_of(product, "a Call or Put option type", option_type,
                   option_types);
    require_not_negative(product, "a non-negative strike", strike);
    require_date(product, "an expiry date", expiry_date);
    require_one_of(product, "a European, Bermudan or American exercise type", exercise_type,
                   exercise_types);
    require_one_of(product, "a Long or Short direction", long_short,
                   direction_types);

    equity_barrier_option_instrument row;
    row.identity.trade_type_code = trade_type_code;
    row.underlying_name = underlying_name;
    row.currency = currency;
    row.notional = notional;
    row.option_type = option_type;
    row.strike = strike;
    row.expiry_date = expiry_date;
    row.exercise_type = exercise_type;
    row.long_short = long_short;
    return row;
}

}

[[nodiscard]] inline equity_barrier_option_instrument
construct_equity_barrier_option(const equity_barrier_option_terms& terms) {
    using namespace equity_terms;
    auto row = equity_builders::barrier_common(
        "equity barrier option", "EquityBarrierOption", terms.underlying_name, terms.currency,
        terms.notional, terms.option_type, terms.strike, terms.expiry_date, terms.exercise_type,
        terms.long_short);
    require_one_of("equity barrier option", "a barrier type from ORE's closed set",
                   terms.lower_barrier_type, barrier_types);
    require_not_negative("equity barrier option", "a non-negative barrier", terms.lower_barrier);
    row.lower_barrier = terms.lower_barrier;
    row.lower_barrier_type = terms.lower_barrier_type;
    row.rebate = terms.rebate;
    return row;
}

[[nodiscard]] inline equity_barrier_option_instrument
construct_equity_european_barrier_option(const equity_barrier_option_terms& terms) {
    using namespace equity_terms;
    auto row = equity_builders::barrier_common(
        "equity European barrier option", "EquityEuropeanBarrierOption", terms.underlying_name,
        terms.currency, terms.notional, terms.option_type, terms.strike, terms.expiry_date,
        terms.exercise_type, terms.long_short);
    require_one_of("equity European barrier option", "a barrier type from ORE's closed set",
                   terms.lower_barrier_type, barrier_types);
    require_not_negative("equity European barrier option", "a non-negative barrier",
                         terms.lower_barrier);
    row.lower_barrier = terms.lower_barrier;
    row.lower_barrier_type = terms.lower_barrier_type;
    row.rebate = terms.rebate;
    return row;
}

[[nodiscard]] inline equity_barrier_option_instrument
construct_equity_double_barrier_option(const equity_double_barrier_option_terms& terms) {
    using namespace equity_terms;
    auto row = equity_builders::barrier_common(
        "equity double barrier option", "EquityDoubleBarrierOption", terms.underlying_name,
        terms.currency, terms.notional, terms.option_type, terms.strike, terms.expiry_date,
        terms.exercise_type, terms.long_short);
    require_one_of("equity double barrier option", "a barrier type from ORE's closed set",
                   terms.lower_barrier_type, barrier_types);
    require_one_of("equity double barrier option", "a barrier type from ORE's closed set",
                   terms.upper_barrier_type, barrier_types);
    require_not_negative("equity double barrier option", "a non-negative barrier",
                         terms.lower_barrier);
    require_not_negative("equity double barrier option", "a non-negative barrier",
                         terms.upper_barrier);
    if (!(terms.upper_barrier > terms.lower_barrier))
        refuse("equity double barrier option", "an upper barrier above its lower one");
    row.lower_barrier = terms.lower_barrier;
    row.lower_barrier_type = terms.lower_barrier_type;
    row.upper_barrier = terms.upper_barrier;
    row.upper_barrier_type = terms.upper_barrier_type;
    row.rebate = terms.rebate;
    return row;
}

[[nodiscard]] inline equity_digital_option_instrument
construct_equity_digital_option(const equity_digital_option_terms& terms) {
    using namespace equity_terms;
    require_text("equity digital option", "an underlying name", terms.underlying_name);
    require_text("equity digital option", "a currency", terms.currency);
    require_money("equity digital option", "a positive notional", terms.notional);
    require_one_of("equity digital option", "a Call or Put option type", terms.option_type,
                   option_types);
    require_not_negative("equity digital option", "a non-negative strike", terms.strike);
    require_date("equity digital option", "an expiry date", terms.expiry_date);
    require_one_of("equity digital option", "a Long or Short direction", terms.long_short,
                   direction_types);

    equity_digital_option_instrument row;
    row.identity.trade_type_code = "EquityDigitalOption";
    row.underlying_name = terms.underlying_name;
    row.currency = terms.currency;
    row.notional = terms.notional;
    row.option_type = terms.option_type;
    row.strike = terms.strike;
    row.expiry_date = terms.expiry_date;
    row.long_short = terms.long_short;
    row.payout_amount = terms.payout_amount;
    return row;
}

[[nodiscard]] inline equity_digital_option_instrument
construct_equity_touch_option(const equity_touch_option_terms& terms) {
    using namespace equity_terms;
    require_text("equity touch option", "an underlying name", terms.underlying_name);
    require_text("equity touch option", "a currency", terms.currency);
    require_money("equity touch option", "a positive notional", terms.notional);
    require_one_of("equity touch option", "a barrier type from ORE's closed set",
                   terms.barrier_type, barrier_types);
    require_not_negative("equity touch option", "a non-negative barrier", terms.barrier_level);
    require_date("equity touch option", "an expiry date", terms.expiry_date);
    require_one_of("equity touch option", "a Long or Short direction", terms.long_short,
                   direction_types);

    equity_digital_option_instrument row;
    row.identity.trade_type_code = "EquityTouchOption";
    row.underlying_name = terms.underlying_name;
    row.currency = terms.currency;
    row.notional = terms.notional;
    row.barrier_level = terms.barrier_level;
    row.barrier_type = terms.barrier_type;
    row.expiry_date = terms.expiry_date;
    row.long_short = terms.long_short;
    row.payout_amount = terms.payout_amount;
    return row;
}

[[nodiscard]] inline equity_forward_instrument
construct_equity_forward(const equity_forward_terms& terms) {
    using namespace equity_terms;
    require_text("equity forward", "an underlying name", terms.underlying_name);
    require_text("equity forward", "a currency", terms.currency);
    require_positive_quantity("equity forward", terms.quantity);
    require_date("equity forward", "an expiry date", terms.expiry_date);
    require_one_of("equity forward", "a Long or Short direction", terms.long_short,
                   direction_types);
    require_one_of_or_absent("equity forward",
                             "a Cash or Physical settlement type where it states one",
                             terms.settlement_type,
                             settlement_types);

    equity_forward_instrument row;
    row.identity.trade_type_code = "EquityForward";
    row.underlying_name = terms.underlying_name;
    row.currency = terms.currency;
    row.quantity = terms.quantity;
    row.forward_price = terms.forward_price;
    row.expiry_date = terms.expiry_date;
    row.long_short = terms.long_short;
    row.settlement_type = terms.settlement_type;
    return row;
}

namespace equity_builders {

inline equity_swap_instrument
swap_common(std::string_view product, const std::string& trade_type_code,
            const std::string& currency, const decimal& notional, const std::string& return_type,
            std::chrono::year_month_day start_date, std::chrono::year_month_day maturity_date,
            const std::string& long_short, const std::string& payment_frequency_code) {
    using namespace equity_terms;
    require_text(product, "a currency", currency);
    require_money(product, "a positive notional", notional);
    require_one_of(product, "a Total or Price return", return_type, return_types);
    require_date(product, "a start date", start_date);
    require_date(product, "a maturity date", maturity_date);
    require_one_of(product, "a Long or Short direction", long_short,
                   direction_types);
    require_text(product, "a payment frequency", payment_frequency_code);

    equity_swap_instrument row;
    row.identity.trade_type_code = trade_type_code;
    row.currency = currency;
    row.notional = notional;
    row.return_type = return_type;
    row.start_date = start_date;
    row.maturity_date = maturity_date;
    row.long_short = long_short;
    row.payment_frequency_code = payment_frequency_code;
    return row;
}

}

[[nodiscard]] inline equity_swap_instrument construct_equity_swap(const equity_swap_terms& terms) {
    using namespace equity_terms;
    require_text("equity swap", "an underlying name", terms.underlying_name);
    auto row = equity_builders::swap_common("equity swap", "EquitySwap", terms.currency,
                                            terms.notional, terms.return_type, terms.start_date,
                                            terms.maturity_date, terms.long_short,
                                            terms.payment_frequency_code);
    row.underlying_name = terms.underlying_name;
    return row;
}

[[nodiscard]] inline equity_swap_instrument
construct_equity_worst_of_basket_swap(const equity_basket_swap_terms& terms) {
    return equity_builders::swap_common("equity worst-of basket swap", "EquityWorstOfBasketSwap",
                                        terms.currency, terms.notional, terms.return_type,
                                        terms.start_date, terms.maturity_date, terms.long_short,
                                        terms.payment_frequency_code);
}

[[nodiscard]] inline equity_variance_swap_instrument
construct_equity_variance_swap(const equity_variance_swap_terms& terms) {
    using namespace equity_terms;
    require_text("equity variance swap", "an underlying name", terms.underlying_name);
    require_text("equity variance swap", "a currency", terms.currency);
    require_money("equity variance swap", "a positive notional", terms.notional);
    require_moment("equity variance swap", terms.variance_strike);
    require_date("equity variance swap", "a start date", terms.start_date);
    require_date("equity variance swap", "a maturity date", terms.maturity_date);
    require_one_of("equity variance swap", "a Long or Short direction", terms.long_short,
                   direction_types);

    equity_variance_swap_instrument row;
    row.identity.trade_type_code = "EquityVarianceSwap";
    row.underlying_name = terms.underlying_name;
    row.currency = terms.currency;
    row.notional = terms.notional;
    row.variance_strike = terms.variance_strike;
    row.start_date = terms.start_date;
    row.maturity_date = terms.maturity_date;
    row.long_short = terms.long_short;
    return row;
}

namespace equity_builders {

inline equity_accumulator_instrument
accumulator_common(std::string_view product, const std::string& trade_type_code,
                   const std::string& underlying_name, const std::string& currency,
                   const decimal& strike, const decimal& fixing_amount,
                   std::chrono::year_month_day start_date,
                   std::chrono::year_month_day expiry_date, const std::string& fixing_frequency,
                   const std::string& long_short, const std::optional<decimal>& knock_out_level,
                   const std::string& payoff_type) {
    using namespace equity_terms;
    require_text(product, "an underlying name", underlying_name);
    require_text(product, "a currency", currency);
    require_not_negative(product, "a non-negative strike", strike);
    require_money(product, "a positive fixing amount", fixing_amount);
    require_date(product, "a start date", start_date);
    require_date(product, "an expiry date", expiry_date);
    require_text(product, "a fixing frequency", fixing_frequency);
    require_one_of(product, "a Long or Short direction", long_short,
                   direction_types);
    require_one_of(product, "a payoff the accumulator table holds", payoff_type, payoff_types);

    equity_accumulator_instrument row;
    row.identity.trade_type_code = trade_type_code;
    row.underlying_name = underlying_name;
    row.currency = currency;
    row.strike = strike;
    row.fixing_amount = fixing_amount;
    row.start_date = start_date;
    row.expiry_date = expiry_date;
    row.fixing_frequency = fixing_frequency;
    row.long_short = long_short;
    row.knock_out_level = knock_out_level;
    row.payoff_type = payoff_type;
    return row;
}

}

[[nodiscard]] inline equity_accumulator_instrument
construct_equity_accumulator(const equity_accumulator_terms& terms) {
    return equity_builders::accumulator_common(
        "equity accumulator", "EquityAccumulator", terms.underlying_name, terms.currency,
        terms.strike, terms.fixing_amount, terms.start_date, terms.expiry_date,
        terms.fixing_frequency, terms.long_short, terms.knock_out_level, terms.payoff_type);
}

[[nodiscard]] inline equity_accumulator_instrument
construct_equity_target_redemption_forward(const equity_tarf_terms& terms) {
    using namespace equity_terms;
    require_money("equity target redemption forward", "a positive target amount",
                  terms.target_amount);
    require_one_of("equity target redemption forward", "a target type the table holds",
                   terms.target_type, target_types);
    auto row = equity_builders::accumulator_common(
        "equity target redemption forward", "EquityTaRF", terms.underlying_name, terms.currency,
        terms.strike, terms.fixing_amount, terms.start_date, terms.expiry_date,
        terms.fixing_frequency, terms.long_short, terms.knock_out_level, terms.payoff_type);
    row.target_amount = terms.target_amount;
    row.target_type = terms.target_type;
    return row;
}

namespace equity_builders {

inline equity_position_instrument position_common(std::string_view product,
                                                  const std::string& trade_type_code,
                                                  const std::string& underlying_name,
                                                  const std::string& currency, double quantity,
                                                  const std::optional<decimal>& price) {
    using namespace equity_terms;
    require_text(product, "an underlying name", underlying_name);
    require_text(product, "a currency", currency);
    require_positive_quantity(product, quantity);

    equity_position_instrument row;
    row.identity.trade_type_code = trade_type_code;
    row.underlying_name = underlying_name;
    row.currency = currency;
    row.quantity = quantity;
    row.price = price;
    return row;
}

}

[[nodiscard]] inline equity_position_instrument
construct_equity_position(const equity_position_terms& terms) {
    return equity_builders::position_common("equity position", "EquityPosition",
                                            terms.underlying_name, terms.currency, terms.quantity,
                                            terms.price);
}

[[nodiscard]] inline equity_position_instrument
construct_equity_option_position(const equity_position_terms& terms) {
    return equity_builders::position_common("equity option position", "EquityOptionPosition",
                                            terms.underlying_name, terms.currency,
                                            terms.quantity, terms.price);
}

}

#endif

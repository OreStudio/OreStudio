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
#ifndef ORES_TRADING_API_DOMAIN_RATE_INSTRUMENTS_HPP
#define ORES_TRADING_API_DOMAIN_RATE_INSTRUMENTS_HPP

#include "ores.trading.api/domain/balance_guaranteed_swap_instrument.hpp"
#include "ores.trading.api/domain/callable_swap_instrument.hpp"
#include "ores.trading.api/domain/cap_floor_instrument.hpp"
#include "ores.trading.api/domain/flexi_swap_instrument.hpp"
#include "ores.trading.api/domain/fra_instrument.hpp"
#include "ores.trading.api/domain/inflation_swap_instrument.hpp"
#include "ores.trading.api/domain/instrument_strike.hpp"
#include "ores.trading.api/domain/knock_out_swap_instrument.hpp"
#include "ores.trading.api/domain/rate_instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_amount.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include "ores.trading.api/domain/swaption_instrument.hpp"
#include "ores.trading.api/domain/vanilla_swap_instrument.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <array>
#include <chrono>
#include <initializer_list>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

/**
 * @brief The rates family's named constructors.
 *
 * A rates product is a header, two legs, and one fact row that says which
 * product the legs belong to. The header states no trade type: the family's
 * routing puts every code in one table, so the fact row is the product's own
 * statement and each constructor here produces exactly one kind of it.
 *
 * A product is constructed whole or not at all. The legs and the fact row
 * cannot disagree about which product they describe, because they come out of
 * the same call.
 */
namespace ores::trading::domain {

/**
 * @brief The exact decimal the family's amounts and rates carry.
 */
using decimal = ores::utility::decimal::decimal;

/**
 * @brief Refuses to construct a product from facts that do not state one.
 */
class rate_product_error final : public std::invalid_argument {
public:
    using std::invalid_argument::invalid_argument;
};

namespace rate_terms {

inline constexpr auto leg_types = std::to_array<std::string_view>({"Fixed", "Floating"});

/*
 * The legs an inflation swap states: the fixed side every rates product has,
 * and the CPI side whose type names the index family the ORE document reads
 * the product's own fields from. A CPI leg is not a floating leg: it observes
 * a published index rather than a benchmark that resets on a schedule.
 */
inline constexpr auto inflation_leg_types = std::to_array<std::string_view>({"Fixed", "CPI"});

inline constexpr auto directions = std::to_array<std::string_view>({"Long", "Short"});

inline constexpr auto exercise_types =
    std::to_array<std::string_view>({"European", "Bermudan", "American"});

inline constexpr auto settlement_types = std::to_array<std::string_view>({"Cash", "Physical"});

inline constexpr auto barrier_types = std::to_array<std::string_view>({"UpAndOut",
                                                                       "UpAndIn",
                                                                       "DownAndOut",
                                                                       "DownAndIn",
                                                                       "KnockIn",
                                                                       "KnockOut",
                                                                       "CumulatedProfitCap",
                                                                       "CumulatedProfitCapPoints",
                                                                       "FixingCap",
                                                                       "FixingFloor"});

[[nodiscard]] inline bool states_one_of(std::string_view value,
                                        std::span<const std::string_view> allowed) {
    for (const auto candidate : allowed) {
        if (value == candidate)
            return true;
    }
    return false;
}

[[noreturn]] inline void refuse(std::string_view product, std::string_view fact) {
    /*
     * The product is named by its phrase rather than by an article: a family
     * holds products whose names begin with a vowel and products whose names do
     * not, and "The" is right for both.
     */
    throw rate_product_error("The " + std::string(product) + " states " + std::string(fact) +
                             ".");
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

inline void require_date(std::string_view product,
                         std::string_view fact,
                         const std::optional<std::chrono::year_month_day>& value) {
    if (!value.has_value() || *value == std::chrono::year_month_day{})
        refuse(product, fact);
}

inline void require_amount(std::string_view product, std::string_view fact, const decimal& value) {
    if (!(value > decimal{}))
        refuse(product, fact);
}

inline void
require_not_negative(std::string_view product, std::string_view fact, const decimal& value) {
    if (value < decimal{})
        refuse(product, fact);
}

}

/**
 * @brief The facts one leg of a rates product states.
 */
struct rate_leg_terms final {
    std::string leg_type_code;
    std::string day_count_fraction_code;
    std::string business_day_convention_code;
    std::string payment_frequency_code;
    /// The index a floating leg observes; empty on a fixed leg.
    std::string floating_index_code;
    std::string currency;
    /// The role the rate row plays: =fixed= or =spread=.
    std::string rate_role;
    decimal value;
    decimal notional;
    bool payer = true;
};

/**
 * @brief The facts every rates product states.
 */
struct rate_common_terms final {
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day maturity_date;
    rate_leg_terms fixed;
    rate_leg_terms floating;
};

/**
 * @brief A vanilla or cross-currency swap's own facts.
 */
struct rate_swap_terms final {
    rate_common_terms common;
    int settlement_lag = 2;
};

/**
 * @brief A forward rate agreement's own facts.
 */
struct rate_fra_terms final {
    rate_common_terms common;
    std::string long_short;
};

/**
 * @brief A cap or a floor's own facts.
 *
 * The strike is required: a cap without one pays nothing, and the schema
 * keeps the strike beside the leg rather than on it.
 */
struct rate_cap_floor_terms final {
    rate_common_terms common;
    decimal strike;
};

/**
 * @brief A swaption's own facts.
 */
struct rate_swaption_terms final {
    rate_common_terms common;
    std::chrono::year_month_day expiry_date;
    std::string long_short;
    std::string exercise_type = "European";
    std::string settlement_type = "Cash";
};

/**
 * @brief A callable swap's own facts.
 */
struct rate_callable_swap_terms final {
    rate_common_terms common;
};

/**
 * @brief A knock out swap's own facts.
 */
struct rate_knock_out_swap_terms final {
    rate_common_terms common;
    std::chrono::year_month_day barrier_start_date;
    std::string barrier_type;
};

/**
 * @brief A flexi swap's own facts.
 */
struct rate_flexi_swap_terms final {
    rate_common_terms common;
    std::string option_long_short;
};

/**
 * @brief A balance guaranteed swap's own facts.
 */
struct rate_balance_guaranteed_swap_terms final {
    rate_common_terms common;
    std::string reference_security;
};

/**
 * @brief An inflation swap's own facts.
 *
 * A zero coupon inflation swap pays a fixed rate against the change in an
 * inflation index over the trade's life. The nominal side is the family's
 * fixed leg; the CPI side is the leg that observes the index. The index, the
 * level it is compared from and the lag it is published at are the product's
 * own facts, and the ORE document states them on the CPI leg rather than on
 * the swap.
 */
struct rate_inflation_swap_terms final {
    rate_common_terms common;
    /// The published index the trade observes, as the tenant's curve is named.
    std::string inflation_index_code;
    /// The index level the trade is struck from. Absent when the document
    /// leaves it to the engine's own fixing.
    std::optional<double> base_cpi;
    /// The publication lag the index is observed at, e.g. =3M=.
    std::string lag_convention;
};

/**
 * @brief The one fact row that says which rate product this is.
 */
using rate_fact = std::variant<vanilla_swap_instrument,
                               inflation_swap_instrument,
                               fra_instrument,
                               cap_floor_instrument,
                               swaption_instrument,
                               callable_swap_instrument,
                               knock_out_swap_instrument,
                               flexi_swap_instrument,
                               balance_guaranteed_swap_instrument>;

/**
 * @brief A rates product and the rows it states.
 */
struct rate_product final {
    rate_instrument row;
    std::vector<swap_leg> legs;
    std::vector<swap_leg_amount> amounts;
    std::vector<swap_leg_rate> rates;
    std::vector<instrument_strike> strikes;
    rate_fact fact;
};

namespace rate_builders {

/**
 * @brief Builds the header and the two legs every rate product states.
 *
 * The legs are numbered 1 and 2 and each states one amount and one rate, which
 * is the shape every routed rate type reduces to: a fixed leg against a
 * floating one.
 */
inline rate_product common_product(std::string_view product, const rate_common_terms& terms) {
    using namespace rate_terms;
    require_date(product, "a start date", terms.start_date);
    require_date(product, "a maturity date", terms.maturity_date);

    rate_product built;
    built.row.start_date = terms.start_date;
    built.row.maturity_date = terms.maturity_date;
    built.row.description = "Generated interest rate trade.";

    const auto add_leg = [&](int number, const rate_leg_terms& leg) {
        require_one_of(product, "a Fixed or Floating leg type", leg.leg_type_code, leg_types);
        require_text(product, "a day count fraction on each leg", leg.day_count_fraction_code);
        require_text(
            product, "a business day convention on each leg", leg.business_day_convention_code);
        require_text(product, "a payment frequency on each leg", leg.payment_frequency_code);
        require_text(product, "a currency on each leg", leg.currency);
        require_text(product, "a rate role on each leg", leg.rate_role);
        require_amount(product, "a positive notional on each leg", leg.notional);
        require_not_negative(product, "a non-negative rate on each leg", leg.value);
        if (leg.leg_type_code == "Floating")
            require_text(product, "a floating index on its floating leg", leg.floating_index_code);

        swap_leg row;
        row.identity.leg_number = number;
        row.payer = leg.payer;
        row.leg_type_code = leg.leg_type_code;
        row.day_count_fraction_code = leg.day_count_fraction_code;
        row.business_day_convention_code = leg.business_day_convention_code;
        row.payment_frequency_code = leg.payment_frequency_code;
        row.floating_index_code = leg.floating_index_code;
        row.currency = leg.currency;
        built.legs.push_back(row);

        swap_leg_amount amount;
        amount.leg_number = number;
        amount.sequence_number = 1;
        amount.start_date = terms.start_date;
        amount.amount = leg.notional;
        built.amounts.push_back(amount);

        swap_leg_rate leg_rate;
        leg_rate.leg_number = number;
        leg_rate.rate_role = leg.rate_role;
        leg_rate.sequence_number = 1;
        leg_rate.start_date = terms.start_date;
        leg_rate.value = leg.value;
        built.rates.push_back(leg_rate);
    };

    add_leg(1, terms.fixed);
    add_leg(2, terms.floating);
    return built;
}

}

[[nodiscard]] inline rate_product construct_rate_swap(const rate_swap_terms& terms) {
    using namespace rate_terms;
    auto built = rate_builders::common_product("vanilla swap", terms.common);
    if (terms.settlement_lag < 0)
        refuse("vanilla swap", "a settlement lag of zero or more days");
    vanilla_swap_instrument fact;
    fact.settlement_lag = terms.settlement_lag;
    built.fact = fact;
    return built;
}

[[nodiscard]] inline rate_product construct_rate_cross_currency_swap(const rate_swap_terms& terms) {
    using namespace rate_terms;
    auto built = rate_builders::common_product("cross currency swap", terms.common);
    if (terms.settlement_lag < 0)
        refuse("cross currency swap", "a settlement lag of zero or more days");
    if (terms.common.fixed.currency == terms.common.floating.currency)
        refuse("cross currency swap", "two different leg currencies");
    vanilla_swap_instrument fact;
    fact.settlement_lag = terms.settlement_lag;
    built.fact = fact;
    return built;
}

[[nodiscard]] inline rate_product construct_rate_fra(const rate_fra_terms& terms) {
    using namespace rate_terms;
    auto built = rate_builders::common_product("forward rate agreement", terms.common);
    require_one_of(
        "forward rate agreement", "a Long or Short direction", terms.long_short, directions);
    fra_instrument fact;
    fact.currency = terms.common.fixed.currency;
    fact.rate_index = terms.common.floating.floating_index_code;
    fact.long_short = terms.long_short;
    fact.notional = terms.common.fixed.notional;
    built.fact = fact;
    return built;
}

[[nodiscard]] inline rate_product construct_rate_cap_floor(const rate_cap_floor_terms& terms) {
    using namespace rate_terms;
    auto built = rate_builders::common_product("cap or floor", terms.common);
    require_amount("cap or floor", "a positive strike", terms.strike);
    built.fact = cap_floor_instrument{};
    instrument_strike strike;
    strike.strike_role = "cap";
    strike.sequence_number = 1;
    strike.bare_value = terms.strike;
    strike.bare_currency = terms.common.fixed.currency;
    built.strikes.push_back(strike);
    return built;
}

[[nodiscard]] inline rate_product construct_rate_swaption(const rate_swaption_terms& terms) {
    using namespace rate_terms;
    auto built = rate_builders::common_product("swaption", terms.common);
    if (terms.expiry_date == std::chrono::year_month_day{})
        refuse("swaption", "an expiry date");
    require_one_of("swaption", "a Long or Short direction", terms.long_short, directions);
    require_one_of("swaption",
                   "a European, Bermudan or American exercise type",
                   terms.exercise_type,
                   exercise_types);
    require_one_of(
        "swaption", "a Cash or Physical settlement type", terms.settlement_type, settlement_types);
    swaption_instrument fact;
    fact.expiry_date = terms.expiry_date;
    fact.exercise_type = terms.exercise_type;
    fact.settlement_type = terms.settlement_type;
    fact.long_short = terms.long_short;
    built.fact = fact;
    return built;
}

[[nodiscard]] inline rate_product
construct_rate_callable_swap(const rate_callable_swap_terms& terms) {
    auto built = rate_builders::common_product("callable swap", terms.common);
    built.fact = callable_swap_instrument{};
    return built;
}

[[nodiscard]] inline rate_product
construct_rate_knock_out_swap(const rate_knock_out_swap_terms& terms) {
    using namespace rate_terms;
    auto built = rate_builders::common_product("knock out swap", terms.common);
    if (terms.barrier_start_date == std::chrono::year_month_day{})
        refuse("knock out swap", "the date its barrier starts");
    require_one_of(
        "knock out swap", "a barrier type from ORE's set", terms.barrier_type, barrier_types);
    knock_out_swap_instrument fact;
    fact.barrier_start_date = terms.barrier_start_date;
    fact.barrier_type = terms.barrier_type;
    built.fact = fact;
    return built;
}

[[nodiscard]] inline rate_product construct_rate_flexi_swap(const rate_flexi_swap_terms& terms) {
    using namespace rate_terms;
    auto built = rate_builders::common_product("flexi swap", terms.common);
    require_one_of("flexi swap", "a Long or Short direction", terms.option_long_short, directions);
    flexi_swap_instrument fact;
    fact.option_long_short = terms.option_long_short;
    built.fact = fact;
    return built;
}

[[nodiscard]] inline rate_product
construct_rate_inflation_swap(const rate_inflation_swap_terms& terms) {
    using namespace rate_terms;
    require_date("inflation swap", "a start date", terms.common.start_date);
    require_date("inflation swap", "a maturity date", terms.common.maturity_date);
    if (!(terms.common.maturity_date > terms.common.start_date))
        refuse("inflation swap", "a maturity date after its start date");
    require_text("inflation swap", "an inflation index", terms.inflation_index_code);
    require_text("inflation swap", "an observation lag", terms.lag_convention);
    if (terms.base_cpi.has_value() && !(*terms.base_cpi > 0.0))
        refuse("inflation swap", "a positive base CPI");

    /*
     * The swap is one fixed rate against the index. Both sides are stated as
     * legs so the store owns the notional and the document can carry the
     * cashflows, and the fixed side states the rate the trade pays.
     */
    const auto& fixed = terms.common.fixed;
    const auto& cpi = terms.common.floating;
    require_text("inflation swap", "a currency on its fixed leg", fixed.currency);
    require_text("inflation swap", "a day count fraction on its fixed leg",
                 fixed.day_count_fraction_code);
    require_text("inflation swap", "a payment frequency on its fixed leg",
                 fixed.payment_frequency_code);
    require_amount("inflation swap", "a positive notional on its fixed leg", fixed.notional);
    require_not_negative("inflation swap", "a non-negative fixed rate", fixed.value);
    require_text("inflation swap", "a currency on its CPI leg", cpi.currency);
    require_text("inflation swap", "a day count fraction on its CPI leg",
                 cpi.day_count_fraction_code);
    require_text("inflation swap", "a payment frequency on its CPI leg",
                 cpi.payment_frequency_code);
    require_amount("inflation swap", "a positive notional on its CPI leg", cpi.notional);

    rate_product built;
    built.row.start_date = terms.common.start_date;
    built.row.maturity_date = terms.common.maturity_date;
    built.row.description = "Generated inflation swap.";

    const auto add_leg = [&](int number, const rate_leg_terms& leg) {
        require_one_of(
            "inflation swap", "a Fixed or CPI leg type", leg.leg_type_code, inflation_leg_types);
        swap_leg row;
        row.identity.leg_number = number;
        row.payer = leg.payer;
        row.leg_type_code = leg.leg_type_code;
        row.day_count_fraction_code = leg.day_count_fraction_code;
        row.business_day_convention_code = leg.business_day_convention_code;
        row.payment_frequency_code = leg.payment_frequency_code;
        row.currency = leg.currency;
        /*
         * Neither leg states a benchmark index, and the CPI leg cannot: the leg
         * column is the soft reference to the benchmark catalogue every
         * floating leg names, and an inflation index is not a member of it. The
         * index the trade observes is the product's own fact, which is what the
         * document reads the CPI leg's fields from.
         */
        built.legs.push_back(std::move(row));

        swap_leg_amount amount;
        amount.leg_number = number;
        amount.sequence_number = 1;
        amount.start_date = terms.common.start_date;
        amount.amount = leg.notional;
        built.amounts.push_back(std::move(amount));

        /*
         * Only the fixed side pays a rate. The CPI side is the index's own
         * accrual and the store has no rate row for it, so it states none.
         */
        if (leg.leg_type_code != "Fixed")
            return;
        swap_leg_rate leg_rate;
        leg_rate.leg_number = number;
        leg_rate.rate_role = leg.rate_role;
        leg_rate.sequence_number = 1;
        leg_rate.start_date = terms.common.start_date;
        leg_rate.value = leg.value;
        built.rates.push_back(std::move(leg_rate));
    };
    add_leg(1, terms.common.fixed);
    add_leg(2, terms.common.floating);

    inflation_swap_instrument fact;
    fact.inflation_index_code = terms.inflation_index_code;
    fact.base_cpi = terms.base_cpi;
    fact.lag_convention = terms.lag_convention;
    built.fact = fact;
    return built;
}

[[nodiscard]] inline rate_product
construct_rate_balance_guaranteed_swap(const rate_balance_guaranteed_swap_terms& terms) {
    using namespace rate_terms;
    auto built = rate_builders::common_product("balance guaranteed swap", terms.common);
    require_text("balance guaranteed swap", "a reference security", terms.reference_security);
    balance_guaranteed_swap_instrument fact;
    fact.reference_security = terms.reference_security;
    built.fact = fact;
    return built;
}

}

#endif

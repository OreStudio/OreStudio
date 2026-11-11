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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_COMMODITY_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_COMMODITY_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Commodity instrument.
 *
 * Represents every commodity product type ORE states. trade_type_code
 * discriminates the exact product, and each optional field block is null
 * when the sub-type does not state it: the option block for option
 * products, the pricing block for average-price and spread products, and
 * the exotic block for variance, accumulator, barrier and basket
 * products.
 *
 * A basket product's constituents are not a column: each is a row of
 * ores.trading.commodity_basket_constituent, keyed to this instrument and
 * its ordinal in the document. A text column held the list as a JSON array
 * and could not be typed, indexed or questioned, so the collection is a
 * child table now.
 */
struct commodity_instrument final {
    instrument_identity identity;

    /**
     * @brief Commodity identifier code (e.g. NGAS, WTI, GOLD).
     */
    std::string commodity_code;

    /**
     * @brief ISO 4217 currency code.
     *
     * Soft FK to ores_refdata_currencies_tbl: ISO 4217 currency codes belong to ores.refdata, so
     * the dependency is recorded rather than copied. PR 4 tightens the soft reference into a real
     * foreign key.
     */
    std::string currency;

    /**
     * @brief Contract quantity. Must be positive.
     */
    double quantity = 0.0;

    /**
     * @brief Unit of measure for the commodity (e.g. BBL, MMBTU, MT).
     */
    std::string unit;

    /**
     * @brief Start date for swaps, forwards, and strips.
     */
    std::optional<std::chrono::year_month_day> start_date;

    /**
     * @brief Maturity or expiry date.
     */
    std::optional<std::chrono::year_month_day> maturity_date;

    /**
     * @brief Fixed price for forwards and fixed-leg swaps.
     */
    std::optional<ores::utility::decimal::decimal> fixed_price;

    /**
     * @brief Call or Put; null for non-option products.
     *
     * Soft FK to ores_trading_option_types_tbl: the values are the closed ORE optionType set (Call,
     * Put). PR 4 tightens the soft reference into a real foreign key.
     */
    std::string option_type;

    /**
     * @brief Option strike price.
     */
    std::optional<ores::utility::decimal::decimal> strike_price;

    /**
     * @brief European or American exercise.
     *
     * Soft FK to ores_trading_exercise_types_tbl: the values are the closed ORE exerciseStyle set
     * (European, Bermudan, American). PR 4 tightens the soft reference into a real foreign key.
     */
    std::string exercise_type;

    /**
     * @brief Arithmetic or Geometric averaging for Asian options.
     *
     * Soft FK to ores_trading_average_types_tbl: ORE states no averaging set for
     * commodityAveragePriceOptionData, so this is a trading-local closed set by decision. PR 4
     * tightens the soft reference into a real foreign key.
     */
    std::string average_type;

    /**
     * @brief Start of the averaging window for Asian options.
     */
    std::optional<std::chrono::year_month_day> averaging_start_date;

    /**
     * @brief End of the averaging window for Asian options.
     */
    std::optional<std::chrono::year_month_day> averaging_end_date;

    /**
     * @brief Second commodity code for spread options.
     */
    std::string spread_commodity_code;

    /**
     * @brief Spread amount for spread options.
     */
    std::optional<ores::utility::decimal::decimal> spread_amount;

    /**
     * @brief Strip frequency code for option strips (e.g. Monthly, Quarterly).
     *
     * Soft FK to ores_refdata_payment_frequencies_tbl: payment frequencies belong to ores.refdata,
     * so the dependency is recorded rather than copied. PR 4 tightens the soft reference into a
     * real foreign key.
     */
    std::string strip_frequency_code;

    /**
     * @brief Strike variance for variance swap products.
     */
    std::optional<double> variance_strike;

    /**
     * @brief Per-fixing accumulation amount for accumulator products.
     */
    std::optional<ores::utility::decimal::decimal> accumulation_amount;

    /**
     * @brief Knock-out barrier level for accumulator products.
     */
    std::optional<ores::utility::decimal::decimal> knock_out_barrier;

    /**
     * @brief UpAndIn, UpAndOut, DownAndIn, DownAndOut.
     *
     * Soft FK to ores_trading_barrier_types_tbl: the values are the closed ORE barrierType set. PR
     * 4 tightens the soft reference into a real foreign key.
     */
    std::string barrier_type;

    /**
     * @brief Lower barrier level.
     */
    std::optional<ores::utility::decimal::decimal> lower_barrier;

    /**
     * @brief Upper barrier level.
     */
    std::optional<ores::utility::decimal::decimal> upper_barrier;

    /**
     * @brief Day count fraction code for swap products.
     *
     * Soft FK to ores_refdata_day_count_fraction_types_tbl: day count conventions belong to
     * ores.refdata, so the dependency is recorded rather than copied. PR 4 tightens the soft
     * reference into a real foreign key.
     */
    std::string day_count_fraction_code;

    /**
     * @brief Payment frequency code for swap products.
     *
     * Soft FK to ores_refdata_payment_frequencies_tbl: payment frequencies belong to ores.refdata,
     * so the dependency is recorded rather than copied. PR 4 tightens the soft reference into a
     * real foreign key.
     */
    std::string payment_frequency_code;

    /**
     * @brief Swaption expiry date for CommoditySwaption.
     */
    std::optional<std::chrono::year_month_day> swaption_expiry_date;

    /**
     * @brief The calendar a commodity variance swap observes in, as Calendar states it.
     */
    std::optional<std::string> calendar;

    /**
     * @brief Whether a commodity variance swap pays on variance or volatility, as MomentType states
     * it.
     */
    std::optional<std::string> moment_type;

    /**
     * @brief Whether a commodity variance swap counts past dividends, as AddPastDividends states
     * it.
     */
    std::optional<bool> add_past_dividends;

    /**
     * @brief What an average price option's payment date is relative to, as CommodityPayRelativeTo
     * states it.
     */
    std::optional<std::string> pay_relative_to;

    /**
     * @brief How often an average price option's quantity applies, as CommodityQuantityFrequency
     * states it.
     */
    std::optional<std::string> quantity_frequency;

    /**
     * @brief The days before expiry an average price option rolls to the next future, as
     * DeliveryRollDays states it.
     */
    std::optional<int> delivery_roll_days;

    /**
     * @brief The future months ahead an average price option observes, as FutureMonthOffset states
     * it.
     */
    std::optional<int> future_month_offset;

    /**
     * @brief Whether an average price option's averaging includes its period end, as
     * IncludePeriodEnd states it.
     */
    std::optional<bool> include_period_end;

    /**
     * @brief Whether a commodity forward is long or short, as Position states it.
     */
    std::optional<std::string> position;

    /**
     * @brief The day an intraday power forward delivers on, as DeliveryDate states it.
     */
    std::optional<std::chrono::year_month_day> delivery_date;

    /**
     * @brief The second of the delivery day an intraday power forward starts to deliver, as
     * DeliveryStart states it.
     */
    std::optional<int> delivery_start;

    /**
     * @brief The second of the delivery day an intraday power forward stops delivering, as
     * DeliveryEnd states it.
     */
    std::optional<int> delivery_end;

    /**
     * @brief The day a forward volatility agreement fixes the volatility it settles on, as FvaDate
     * states it.
     */
    std::optional<std::chrono::year_month_day> fva_date;

    /**
     * @brief The day a forward volatility agreement or a strike resettable option pays its premium,
     * as PremiumDate states it.
     */
    std::optional<std::chrono::year_month_day> premium_date;

    /**
     * @brief The day a scripted commodity product settles, as SettlementDate states it.
     */
    std::optional<std::chrono::year_month_day> settlement_date;

    /**
     * @brief The volatility a forward volatility agreement strikes at, as ImpliedVolStrike states
     * it.
     */
    std::optional<ores::utility::decimal::decimal> implied_vol_strike;

    /**
     * @brief The strike a strike resettable option resets to when its trigger is hit, as
     * ResetStrike states it.
     */
    std::optional<ores::utility::decimal::decimal> reset_strike;

    /**
     * @brief Whether a strike resettable option triggers above or below its trigger price, as
     * TriggerType states it.
     */
    std::optional<std::string> trigger_type;

    /**
     * @brief The price that resets a strike resettable option's strike, as TriggerPrice states it.
     */
    std::optional<ores::utility::decimal::decimal> trigger_price;

    /**
     * @brief The premium a strike resettable option pays, as Premium states it.
     */
    std::optional<ores::utility::decimal::decimal> premium_amount;

    /**
     * @brief The gain at which a TaRF knocks out, as TargetAmount states it.
     */
    std::optional<ores::utility::decimal::decimal> target_amount;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const commodity_instrument&, const commodity_instrument&) = default;
};

/**
 * @brief The canonical text of the members of commodity_instrument the customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const commodity_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "commodity_code", v.commodity_code);
    ores::utility::digest::append_member(out, "currency", v.currency);
    ores::utility::digest::append_member(out, "quantity", v.quantity);
    ores::utility::digest::append_member(out, "unit", v.unit);
    ores::utility::digest::append_member(out, "start_date", v.start_date);
    ores::utility::digest::append_member(out, "maturity_date", v.maturity_date);
    ores::utility::digest::append_member(out, "fixed_price", v.fixed_price);
    ores::utility::digest::append_member(out, "option_type", v.option_type);
    ores::utility::digest::append_member(out, "strike_price", v.strike_price);
    ores::utility::digest::append_member(out, "exercise_type", v.exercise_type);
    ores::utility::digest::append_member(out, "average_type", v.average_type);
    ores::utility::digest::append_member(out, "averaging_start_date", v.averaging_start_date);
    ores::utility::digest::append_member(out, "averaging_end_date", v.averaging_end_date);
    ores::utility::digest::append_member(out, "spread_commodity_code", v.spread_commodity_code);
    ores::utility::digest::append_member(out, "spread_amount", v.spread_amount);
    ores::utility::digest::append_member(out, "strip_frequency_code", v.strip_frequency_code);
    ores::utility::digest::append_member(out, "variance_strike", v.variance_strike);
    ores::utility::digest::append_member(out, "accumulation_amount", v.accumulation_amount);
    ores::utility::digest::append_member(out, "knock_out_barrier", v.knock_out_barrier);
    ores::utility::digest::append_member(out, "barrier_type", v.barrier_type);
    ores::utility::digest::append_member(out, "lower_barrier", v.lower_barrier);
    ores::utility::digest::append_member(out, "upper_barrier", v.upper_barrier);
    ores::utility::digest::append_member(out, "day_count_fraction_code", v.day_count_fraction_code);
    ores::utility::digest::append_member(out, "payment_frequency_code", v.payment_frequency_code);
    ores::utility::digest::append_member(out, "swaption_expiry_date", v.swaption_expiry_date);
    ores::utility::digest::append_member(out, "calendar", v.calendar);
    ores::utility::digest::append_member(out, "moment_type", v.moment_type);
    ores::utility::digest::append_member(out, "add_past_dividends", v.add_past_dividends);
    ores::utility::digest::append_member(out, "pay_relative_to", v.pay_relative_to);
    ores::utility::digest::append_member(out, "quantity_frequency", v.quantity_frequency);
    ores::utility::digest::append_member(out, "delivery_roll_days", v.delivery_roll_days);
    ores::utility::digest::append_member(out, "future_month_offset", v.future_month_offset);
    ores::utility::digest::append_member(out, "include_period_end", v.include_period_end);
    ores::utility::digest::append_member(out, "position", v.position);
    ores::utility::digest::append_member(out, "delivery_date", v.delivery_date);
    ores::utility::digest::append_member(out, "delivery_start", v.delivery_start);
    ores::utility::digest::append_member(out, "delivery_end", v.delivery_end);
    ores::utility::digest::append_member(out, "fva_date", v.fva_date);
    ores::utility::digest::append_member(out, "premium_date", v.premium_date);
    ores::utility::digest::append_member(out, "settlement_date", v.settlement_date);
    ores::utility::digest::append_member(out, "implied_vol_strike", v.implied_vol_strike);
    ores::utility::digest::append_member(out, "reset_strike", v.reset_strike);
    ores::utility::digest::append_member(out, "trigger_type", v.trigger_type);
    ores::utility::digest::append_member(out, "trigger_price", v.trigger_price);
    ores::utility::digest::append_member(out, "premium_amount", v.premium_amount);
    ores::utility::digest::append_member(out, "target_amount", v.target_amount);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for commodity_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity_instrument&) {
    return "ores.trading.commodity_instrument";
}

}

#endif

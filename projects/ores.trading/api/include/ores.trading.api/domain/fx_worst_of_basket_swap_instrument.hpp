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
#ifndef ORES_TRADING_API_DOMAIN_FX_WORST_OF_BASKET_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_FX_WORST_OF_BASKET_SWAP_INSTRUMENT_HPP

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
 * @brief FX worst-of basket swap.
 *
 * Represents FxWorstOfBasketSwap trades: a swap that pays a floating leg
 * against fixed coupons whose payment and knock-out depend on the worst
 * performing FX rate in a basket. ORE reads it through a class of its own.
 * The basket members are instrument underlying rows under the role
 * underlying; their initial prices and the per-date knock-out and fixed
 * trigger levels are instrument value rows under the roles initial_price,
 * knock_out_level and fixed_trigger_level; the eight schedules are
 * instrument schedule rows.
 */
struct fx_worst_of_basket_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Long or Short, as LongShort states it.
     */
    std::string long_short;

    /**
     * @brief The currency the swap pays in, as Currency states it.
     */
    std::string currency;

    /**
     * @brief The notional the coupons are paid on, as Quantity states it.
     */
    double quantity{};

    /**
     * @brief The fixed coupon rate, as FixedRate states it.
     */
    ores::utility::decimal::decimal fixed_rate;

    /**
     * @brief The floating leg's index, as FloatingIndex states it.
     */
    std::string floating_index;

    /**
     * @brief The floating leg's day count, as FloatingDayCountFraction states it.
     */
    std::string floating_day_count_fraction;

    /**
     * @brief The first fixed coupon, paid unconditionally, as InitialFixedRate states it.
     */
    std::optional<ores::utility::decimal::decimal> initial_fixed_rate;

    /**
     * @brief The date the first fixed coupon pays, as InitialFixedPayDate states it.
     */
    std::optional<std::chrono::year_month_day> initial_fixed_pay_date;

    /**
     * @brief The date a knocked-in payment is made, as KnockInPayDate states it.
     */
    std::optional<std::chrono::year_month_day> knock_in_pay_date;

    /**
     * @brief Whether the knock-in is watched only on its determination dates, as BermudanKnockIn
     * states it.
     */
    std::optional<bool> bermudan_knock_in;

    /**
     * @brief The level, as a fraction of the initial price, below which the swap knocks in, as
     * KnockInLevel states it.
     */
    std::optional<ores::utility::decimal::decimal> knock_in_level;

    /**
     * @brief Whether a missed fixed coupon is paid later, as AccumulatingFixedCoupons states it.
     */
    std::optional<bool> accumulating_fixed_coupons;

    /**
     * @brief Whether the fixed coupons accrue over their period, as AccruingFixedCoupons states it.
     */
    std::optional<bool> accruing_fixed_coupons;

    /**
     * @brief The spread over the floating index, as FloatingSpread states it.
     */
    std::optional<ores::utility::decimal::decimal> floating_spread;

    /**
     * @brief The lookback period of the floating index, as FloatingLookback states it.
     */
    std::optional<std::string> floating_lookback;

    /**
     * @brief The rate cutoff of an overnight floating index, as FloatingRateCutoff states it.
     */
    std::optional<std::string> floating_rate_cutoff;

    /**
     * @brief Whether an overnight floating index is averaged rather than compounded, as IsAveraged
     * states it.
     */
    std::optional<bool> is_averaged;

    /**
     * @brief Whether the spread is compounded with the index, as IncludeSpread states it.
     */
    std::optional<bool> include_spread;

    /**
     * @brief The strike of the knocked-in payment, as a fraction of the initial price, as Strike
     * states it.
     */
    std::optional<ores::utility::decimal::decimal> strike;

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
    friend bool operator==(const fx_worst_of_basket_swap_instrument&,
                           const fx_worst_of_basket_swap_instrument&) = default;
};

/**
 * @brief The canonical text of the members of fx_worst_of_basket_swap_instrument the customer
 * agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const fx_worst_of_basket_swap_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "currency", v.currency);
    ores::utility::digest::append_member(out, "quantity", v.quantity);
    ores::utility::digest::append_member(out, "fixed_rate", v.fixed_rate);
    ores::utility::digest::append_member(out, "floating_index", v.floating_index);
    ores::utility::digest::append_member(
        out, "floating_day_count_fraction", v.floating_day_count_fraction);
    ores::utility::digest::append_member(out, "initial_fixed_rate", v.initial_fixed_rate);
    ores::utility::digest::append_member(out, "initial_fixed_pay_date", v.initial_fixed_pay_date);
    ores::utility::digest::append_member(out, "knock_in_pay_date", v.knock_in_pay_date);
    ores::utility::digest::append_member(out, "bermudan_knock_in", v.bermudan_knock_in);
    ores::utility::digest::append_member(out, "knock_in_level", v.knock_in_level);
    ores::utility::digest::append_member(
        out, "accumulating_fixed_coupons", v.accumulating_fixed_coupons);
    ores::utility::digest::append_member(out, "accruing_fixed_coupons", v.accruing_fixed_coupons);
    ores::utility::digest::append_member(out, "floating_spread", v.floating_spread);
    ores::utility::digest::append_member(out, "floating_lookback", v.floating_lookback);
    ores::utility::digest::append_member(out, "floating_rate_cutoff", v.floating_rate_cutoff);
    ores::utility::digest::append_member(out, "is_averaged", v.is_averaged);
    ores::utility::digest::append_member(out, "include_spread", v.include_spread);
    ores::utility::digest::append_member(out, "strike", v.strike);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for fx_worst_of_basket_swap_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_worst_of_basket_swap_instrument&) {
    return "ores.trading.fx_worst_of_basket_swap_instrument";
}

}

#endif

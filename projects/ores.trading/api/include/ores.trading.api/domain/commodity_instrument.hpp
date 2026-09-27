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
 */
struct commodity_instrument final {
    instrument_identity identity;

    /**
     * @brief Commodity identifier code (e.g. NGAS, WTI, GOLD).
     */
    std::string commodity_code;

    /**
     * @brief ISO 4217 currency code.
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
    std::string start_date;

    /**
     * @brief Maturity or expiry date.
     */
    std::string maturity_date;

    /**
     * @brief Fixed price for forwards and fixed-leg swaps.
     */
    std::optional<double> fixed_price;

    /**
     * @brief Call or Put; null for non-option products.
     */
    std::string option_type;

    /**
     * @brief Option strike price.
     */
    std::optional<double> strike_price;

    /**
     * @brief European or American exercise.
     */
    std::string exercise_type;

    /**
     * @brief Arithmetic or Geometric averaging for Asian options.
     */
    std::string average_type;

    /**
     * @brief Start of the averaging window for Asian options.
     */
    std::string averaging_start_date;

    /**
     * @brief End of the averaging window for Asian options.
     */
    std::string averaging_end_date;

    /**
     * @brief Second commodity code for spread options.
     */
    std::string spread_commodity_code;

    /**
     * @brief Spread amount for spread options.
     */
    std::optional<double> spread_amount;

    /**
     * @brief Strip frequency code for option strips (e.g. Monthly, Quarterly).
     */
    std::string strip_frequency_code;

    /**
     * @brief Strike variance for variance swap products.
     */
    std::optional<double> variance_strike;

    /**
     * @brief Per-fixing accumulation amount for accumulator products.
     */
    std::optional<double> accumulation_amount;

    /**
     * @brief Knock-out barrier level for accumulator products.
     */
    std::optional<double> knock_out_barrier;

    /**
     * @brief UpAndIn, UpAndOut, DownAndIn, DownAndOut.
     */
    std::string barrier_type;

    /**
     * @brief Lower barrier level.
     */
    std::optional<double> lower_barrier;

    /**
     * @brief Upper barrier level.
     */
    std::optional<double> upper_barrier;

    /**
     * @brief JSON array of {code, weight} constituents for basket products.
     */
    std::string basket_json;

    /**
     * @brief Day count fraction code for swap products.
     */
    std::string day_count_code;

    /**
     * @brief Payment frequency code for swap products.
     */
    std::string payment_frequency_code;

    /**
     * @brief Swaption expiry date for CommoditySwaption.
     */
    std::string swaption_expiry_date;

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

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
#ifndef ORES_TRADING_DOMAIN_COMMODITY_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_COMMODITY_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Commodity instrument economics for all commodity ORE product types.
 *
 * Discriminated by trade_type_code. Optional fields are empty/zero when not
 * applicable to the specific sub-type:
 *   option_type/strike_price/exercise_type: CommodityOption* products
 *   barrier_type/lower_barrier/upper_barrier: barrier option products
 *   average_type/averaging_start_date/averaging_end_date: Asian/average-price
 *   spread_commodity_code/spread_amount: CommoditySpreadOption
 *   strip_frequency_code: CommodityOptionStrip
 *   variance_strike: CommodityVarianceSwap and variants
 *   accumulation_amount/knock_out_barrier: CommodityAccumulator, CommodityTaRF
 *   basket_json: CommodityBasketOption, CommodityRainbowOption,
 *                CommodityWorstOfBasketSwap
 *   day_count_code/payment_frequency_code: CommoditySwap
 *   swaption_expiry_date: CommoditySwaption
 */
struct commodity_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this commodity instrument.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this instrument.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief UUID of the associated trade record.
     *
     * Soft FK to ores_trading_trades_tbl. Absent for standalone instruments.
     */
    std::optional<boost::uuids::uuid> trade_id;

    /**
     * @brief ORE product type code (CommodityForward, CommodityOption, etc.).
     */
    std::string trade_type_code;

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
     * @brief Maturity/expiry date.
     */
    std::string maturity_date;

    /**
     * @brief Fixed price for forwards and fixed-leg swaps. Nullopt if not applicable.
     */
    std::optional<double> fixed_price;

    /**
     * @brief Option type: 'Call' or 'Put'. Empty for non-option products.
     */
    std::string option_type;

    /**
     * @brief Option strike price. Nullopt for non-option products.
     */
    std::optional<double> strike_price;

    /**
     * @brief Exercise type: 'European' or 'American'.
     */
    std::string exercise_type;

    /**
     * @brief Averaging type for Asian options: 'Arithmetic' or 'Geometric'.
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
     * @brief Spread amount for spread options. Nullopt when not applicable.
     */
    std::optional<double> spread_amount;

    /**
     * @brief Strip frequency code for option strips (e.g. Monthly, Quarterly).
     */
    std::string strip_frequency_code;

    /**
     * @brief Strike variance for variance swap products. Nullopt when not applicable.
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
     * @brief Barrier type: e.g. 'UpIn', 'UpOut', 'DownIn', 'DownOut'.
     */
    std::string barrier_type;

    /**
     * @brief Lower barrier level. Nullopt when not applicable.
     */
    std::optional<double> lower_barrier;

    /**
     * @brief Upper barrier level. Nullopt when not applicable.
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
     * @brief Swaption expiry date for CommoditySwaption products.
     */
    std::string swaption_expiry_date;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this record.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif

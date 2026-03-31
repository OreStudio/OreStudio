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
#ifndef ORES_TRADING_DOMAIN_EQUITY_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_EQUITY_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Equity instrument economics for all equity ORE product types.
 *
 * Discriminated by trade_type_code. Optional fields are empty/zero when not
 * applicable to the specific sub-type:
 *   option_type/strike_price/exercise_type: all EquityOption* products
 *   barrier_type/lower_barrier/upper_barrier: EquityBarrierOption and variants
 *   average_type/averaging_start_date: EquityAsianOption
 *   variance_strike: EquityVarianceSwap and variants
 *   cliquet_frequency_code: EquityCliquetOption
 *   accumulation_amount/knock_out_barrier: EquityAccumulator, EquityTaRF
 *   basket_json: EquityBasketOption, EquityRainbowOption, EquityWorstOfBasketSwap
 *   day_count_code/payment_frequency_code/return_type: EquitySwap, TotalReturnSwap
 *   quantity: EquityPosition, EquityOptionPosition
 */
struct equity_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this equity instrument.
     */
    boost::uuids::uuid id;

    /**
     * @brief UUID of the associated trade record.
     *
     * Soft FK to ores_trading_trades_tbl. Absent for standalone instruments.
     */
    std::optional<boost::uuids::uuid> trade_id;

    /**
     * @brief ORE product type code (EquityOption, EquitySwap, etc.).
     */
    std::string trade_type_code;

    /**
     * @brief Underlying equity identifier (ticker/symbol, e.g. ".SPX", "AAPL").
     */
    std::string underlying_code;

    /**
     * @brief ISO 4217 currency code.
     */
    std::string currency;

    /**
     * @brief Contract notional or equity value.
     */
    double notional = 0.0;

    /**
     * @brief Number of shares or contracts. Used for position products.
     */
    double quantity = 0.0;

    /**
     * @brief Start date for swaps, accumulators, and similar products.
     */
    std::string start_date;

    /**
     * @brief Maturity/expiry date for options, swaps, and forwards.
     */
    std::string maturity_date;

    /**
     * @brief Option type: 'Call' or 'Put'. Empty for non-option products.
     */
    std::string option_type;

    /**
     * @brief Option strike price. Zero for non-option products.
     */
    double strike_price = 0.0;

    /**
     * @brief Exercise type: 'European', 'American', or 'Bermudan'.
     */
    std::string exercise_type;

    /**
     * @brief Barrier type: e.g. 'UpIn', 'UpOut', 'DownIn', 'DownOut'.
     */
    std::string barrier_type;

    /**
     * @brief Lower barrier level. Zero when not applicable.
     */
    double lower_barrier = 0.0;

    /**
     * @brief Upper barrier level. Zero when not applicable.
     */
    double upper_barrier = 0.0;

    /**
     * @brief Averaging type for Asian options: 'Arithmetic' or 'Geometric'.
     */
    std::string average_type;

    /**
     * @brief Start of the averaging window for Asian options.
     */
    std::string averaging_start_date;

    /**
     * @brief Strike variance for variance swap products. Zero when not applicable.
     */
    double variance_strike = 0.0;

    /**
     * @brief Payment frequency code for cliquet options. Empty when not applicable.
     */
    std::string cliquet_frequency_code;

    /**
     * @brief Per-fixing accumulation amount for accumulator products.
     */
    double accumulation_amount = 0.0;

    /**
     * @brief Knock-out barrier level for accumulator products.
     */
    double knock_out_barrier = 0.0;

    /**
     * @brief JSON array of {code, weight} constituents for basket/rainbow products.
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
     * @brief Equity return type for swap products: 'TotalReturn' or 'PriceReturn'.
     */
    std::string return_type;

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

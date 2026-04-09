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
#ifndef ORES_TRADING_DOMAIN_FX_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_FX_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief FX instrument economics for FxForward, FxSwap, and FxOption trades.
 *
 * Discriminated by trade_type_code. Option-specific fields (option_type,
 * strike_price, expiry_date) are empty/zero for non-option products.
 */
struct fx_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this FX instrument.
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
     * @brief ORE product type code (FxForward, FxSwap, FxOption).
     */
    std::string trade_type_code;

    /**
     * @brief ISO 4217 currency code of the bought leg (e.g. EUR).
     */
    std::string bought_currency;

    /**
     * @brief Amount bought in bought_currency.
     */
    double bought_amount = 0.0;

    /**
     * @brief ISO 4217 currency code of the sold leg (e.g. USD).
     */
    std::string sold_currency;

    /**
     * @brief Amount sold in sold_currency.
     */
    double sold_amount = 0.0;

    /**
     * @brief Settlement/value date (ISO 8601 date string, e.g. 2026-06-30).
     *
     * Absent for FX options: the settlement date is derived from the expiry
     * date and the currency pair's spot-day convention, not carried in the
     * trade structure.
     */
    std::optional<std::string> value_date;

    /**
     * @brief Optional settlement method (e.g. Cash, Physical).
     */
    std::string settlement;

    /**
     * @brief Option type: 'Call' or 'Put'. Empty for non-option products.
     */
    std::string option_type;

    /**
     * @brief Option strike price. Zero for non-option products.
     */
    double strike_price = 0.0;

    /**
     * @brief Option expiry date (ISO 8601). Empty for non-option products.
     */
    std::string expiry_date;

    /**
     * @brief Barrier type (e.g. UpAndIn, DownAndOut). Empty for vanilla types.
     */
    std::string barrier_type;

    /**
     * @brief Lower (or only) barrier level. Zero when not applicable.
     */
    double lower_barrier = 0.0;

    /**
     * @brief Upper barrier level for double-barrier products. Zero when not applicable.
     */
    double upper_barrier = 0.0;

    /**
     * @brief Notional / payoff amount. Used for digital, touch, and variance products.
     */
    double notional = 0.0;

    /**
     * @brief Strike variance/volatility for variance swap products.
     */
    double variance_strike = 0.0;

    /**
     * @brief FX pair or index identifier (e.g. TR20H-EUR-USD).
     */
    std::string underlying_code;

    /**
     * @brief Start date for variance swaps and accumulators.
     */
    std::string start_date;

    /**
     * @brief Per-fixing accumulation amount for accumulator and TaRF products.
     */
    double accumulation_amount = 0.0;

    /**
     * @brief Knock-out barrier level for accumulator and TaRF products.
     */
    double knock_out_barrier = 0.0;

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

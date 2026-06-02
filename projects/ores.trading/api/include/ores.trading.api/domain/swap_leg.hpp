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
#ifndef ORES_TRADING_DOMAIN_SWAP_LEG_HPP
#define ORES_TRADING_DOMAIN_SWAP_LEG_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief One leg of a rates instrument (Swap, CrossCurrencySwap, CapFloor,
 * Swaption).
 *
 * A plain IRS has two legs: one fixed, one floating. A cross-currency swap
 * also has two legs but with different currencies. Fields not applicable to
 * a leg type are left empty (e.g. floating_index_code for fixed legs,
 * fixed_rate for floating legs).
 */
struct swap_leg final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this leg.
     */
    boost::uuids::uuid id;

    /**
     * @brief UUID of the parent instrument.
     *
     * Soft FK to the per-type rates instrument table (e.g. ores_trading_fra_instruments_tbl).
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Leg ordering within the instrument (1-based).
     *
     * Leg 1 is conventionally the fixed or pay leg.
     */
    int leg_number = 1;

    /**
     * @brief Type of this leg.
     *
     * Soft FK to ores_trading_leg_types_tbl (Fixed, Floating, CMS, OIS, etc.).
     */
    std::string leg_type_code;

    /**
     * @brief Day count fraction convention for this leg.
     *
     * Soft FK to ores_trading_day_count_fraction_types_tbl.
     */
    std::string day_count_fraction_code;

    /**
     * @brief Business day convention for date adjustments.
     *
     * Soft FK to ores_trading_business_day_convention_types_tbl.
     */
    std::string business_day_convention_code;

    /**
     * @brief Payment frequency for this leg.
     *
     * Soft FK to ores_trading_payment_frequency_types_tbl.
     */
    std::string payment_frequency_code;

    /**
     * @brief Floating rate index (empty for fixed legs).
     *
     * Soft FK to ores_trading_floating_index_types_tbl.
     * Examples: EURIBOR6M, SOFR, LIBOR3M.
     */
    std::string floating_index_code;

    /**
     * @brief Fixed coupon rate as a decimal (e.g. 0.05 = 5%).
     *
     * Zero for floating legs.
     */
    double fixed_rate = 0.0;

    /**
     * @brief Floating spread added to the index rate, as a decimal.
     *
     * Zero for plain floating legs with no spread.
     */
    double spread = 0.0;

    /**
     * @brief Leg notional amount.
     *
     * May differ from the instrument notional for cross-currency swaps.
     */
    double notional = 0.0;

    /**
     * @brief ISO 4217 currency code for this leg (e.g. USD, EUR).
     */
    std::string currency;

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

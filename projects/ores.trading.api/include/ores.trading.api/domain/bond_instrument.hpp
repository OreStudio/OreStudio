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
#ifndef ORES_TRADING_DOMAIN_BOND_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_BOND_INSTRUMENT_HPP

#include <chrono>
#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Bond instrument economics for Bond, ForwardBond, CallableBond,
 * ConvertibleBond, and BondRepo trades.
 *
 * Discriminated by trade_type_code. Optional fields (call_date,
 * conversion_ratio) are empty/zero for non-applicable product types.
 */
struct bond_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this bond instrument.
     */
    boost::uuids::uuid id;

    /**
     * @brief ORE product type code (Bond, ForwardBond, CallableBond,
     * ConvertibleBond, BondRepo, BondFuture, BondOption, BondTRS,
     * BondPosition, Ascot).
     */
    std::string trade_type_code;

    /**
     * @brief Name of the bond issuer.
     */
    std::string issuer;

    /**
     * @brief ISO 4217 currency code (e.g. USD).
     */
    std::string currency;

    /**
     * @brief Face (notional) value of the bond.
     */
    double face_value = 0.0;

    /**
     * @brief Annual coupon rate as a decimal (e.g. 0.05 for 5%).
     */
    double coupon_rate = 0.0;

    /**
     * @brief Payment frequency code (e.g. Annual, SemiAnnual, Quarterly).
     */
    std::string coupon_frequency_code;

    /**
     * @brief Day count convention code (e.g. Actual365Fixed, Thirty360).
     */
    std::string day_count_code;

    /**
     * @brief Issue date (ISO 8601 date string, e.g. 2026-01-15).
     */
    std::string issue_date;

    /**
     * @brief Maturity date (ISO 8601 date string, e.g. 2036-01-15).
     */
    std::string maturity_date;

    /**
     * @brief Optional number of settlement days. Zero means not specified.
     */
    int settlement_days = 0;

    /**
     * @brief Optional call date for callable bonds (ISO 8601). Empty for
     * non-callable products.
     */
    std::string call_date;

    /**
     * @brief Optional conversion ratio for convertible bonds. Zero for
     * non-convertible products.
     */
    double conversion_ratio = 0.0;

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

    // -------------------------------------------------------------------------
    // Phase 7 extensions: BondFuture, BondOption, BondTRS, Ascot
    // -------------------------------------------------------------------------

    /**
     * @brief Delivery date for BondFuture. Empty for other types.
     */
    std::string future_expiry_date;

    /**
     * @brief Option type for BondOption: "Call" or "Put". Empty otherwise.
     */
    std::string option_type;

    /**
     * @brief Option expiry date for BondOption (ISO 8601). Empty otherwise.
     */
    std::string option_expiry_date;

    /**
     * @brief Option strike as clean price for BondOption. Null when not set.
     */
    std::optional<double> option_strike;

    /**
     * @brief Return type for BondTRS: "TotalReturn" or "PriceReturn".
     * Empty otherwise.
     */
    std::string trs_return_type;

    /**
     * @brief Funding leg floating index code for BondTRS. Empty otherwise.
     */
    std::string trs_funding_leg_code;

    /**
     * @brief ASCOT option type. Empty for non-Ascot products.
     */
    std::string ascot_option_type;
};

}

#endif

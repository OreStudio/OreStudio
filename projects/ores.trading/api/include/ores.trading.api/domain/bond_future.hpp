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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FUTURE_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FUTURE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Per-trade bond future facts: one row per future instrument, keyed by instrument_id.
 *
 * One row per bond future trade, keyed by the instrument row it
 * extends. The columns fix the ER row ("delivery date and the facts the
 * XSD bondFutureData carries") from external/ore/xsd/instruments.xsd
 * lines 422-448: every single-valued scalar of the structure, minus the
 * DeliveryBasket list, which has no destination in the nine tables and
 * lands in the shared instrument-keyed underlyings of the parent story
 * (recorded scope limit, task D7943D7E wave 1.3).
 */
struct bond_future final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the bond future instrument this fact row extends.
     *
     * The instrument row carries the trade, workspace and party; the fact row only carries the
     * future terms. Per the ER, no workspace column rides the fact tables.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Name of the futures contract (the bond the contract is written on).
     */
    std::string contract_name;

    /**
     * @brief Notional of one contract.
     */
    double contract_notional = 0.0;

    /**
     * @brief Long or short position in the contract.
     */
    std::string long_short;

    /**
     * @brief ISO 4217 currency code of the contract.
     */
    std::string currency;

    /**
     * @brief Delivery month of the contract (YYYY-MM text).
     */
    std::string contract_month;

    /**
     * @brief Deliverable grade of the contract, when the contract names one.
     */
    std::string deliverable_grade;

    /**
     * @brief Fair price of the contract at trade time.
     */
    double fair_price = 0.0;

    /**
     * @brief Settlement type of the contract (Cash, Physical).
     */
    std::string settlement;

    /**
     * @brief True when the settlement price is dirty (includes accrued).
     */
    bool settlement_dirty = false;

    /**
     * @brief Root date of the contract's expiry basis (ISO 8601 date string).
     */
    std::string root_date;

    /**
     * @brief Expiry basis of the contract, when the contract names one.
     */
    std::string expiry_basis;

    /**
     * @brief Settlement basis of the contract, when the contract names one.
     */
    std::string settlement_basis;

    /**
     * @brief Lag in days between the root date and the expiry basis.
     */
    int expiry_lag = 0;

    /**
     * @brief Lag in days between the expiry basis and the settlement basis.
     */
    int settlement_lag = 0;

    /**
     * @brief Last trading date of the contract (ISO 8601 date string).
     */
    std::string last_trading_date;

    /**
     * @brief Last delivery date of the contract (ISO 8601 date string). The ER names this the
     * delivery date of the fact row.
     */
    std::string last_delivery_date;

    /**
     * @brief Username of the person who last modified this bond future.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
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

/**
 * @brief Dispatch-key identifier for bond_future, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_future&) {
    return "ores.trading.bond_future";
}

}

#endif

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
#ifndef ORES_TRADE_DOMAIN_TRADE_PARTY_ROLE_HPP
#define ORES_TRADE_DOMAIN_TRADE_PARTY_ROLE_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trade::domain {

/**
 * @brief Counterparty role assignment on a trade.
 *
 * Associates a counterparty with a specific role on a trade.
 * The internal party (the house) is derived from book_id via books.party_id.
 * Supports multiple counterparties per trade in different roles.
 */
struct trade_party_role final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this party role assignment.
     *
     * Surrogate key for the trade party role record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Trade this role assignment belongs to.
     *
     * Soft FK to ores_trade_trades_tbl.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Counterparty fulfilling the role.
     *
     * Soft FK to ores_refdata_counterparties_tbl.
     */
    boost::uuids::uuid counterparty_id;

    /**
     * @brief Role the counterparty plays on this trade.
     *
     * Soft FK to ores_trade_party_role_types_tbl.
     */
    std::string role;

    /**
     * @brief Username of the person who last modified this trade party role.
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

}

#endif

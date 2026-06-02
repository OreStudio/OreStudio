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
#ifndef ORES_TRADING_DOMAIN_COMPOSITE_LEG_HPP
#define ORES_TRADING_DOMAIN_COMPOSITE_LEG_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief One constituent trade of a composite instrument (CompositeTrade,
 * MultiLegOption).
 *
 * The leg_sequence field provides 1-based ordering of the constituent trades
 * within the parent composite instrument.
 */
struct composite_leg final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this composite leg.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this instrument.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief UUID of the parent composite instrument.
     *
     * Soft FK to ores_trading_composite_instruments_tbl.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Leg ordering within the instrument (1-based).
     */
    int leg_sequence = 1;

    /**
     * @brief UUID string identifying the constituent trade.
     */
    std::string constituent_trade_id;

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

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
#ifndef ORES_TRADE_DOMAIN_TRADE_IDENTIFIER_HPP
#define ORES_TRADE_DOMAIN_TRADE_IDENTIFIER_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trade::domain {

/**
 * @brief External identifier (UTI, USI, internal) assigned to a trade.
 *
 * Junction entity assigning external identifiers to trades.
 * Each trade can have multiple identifiers of different types.
 * The issuing_party_id references either a party or counterparty.
 */
struct trade_identifier final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this trade identifier record.
     *
     * Surrogate key for the trade identifier record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Trade this identifier belongs to.
     *
     * Soft FK to ores_trade_trades_tbl.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Party or counterparty that issued this identifier.
     *
     * Soft FK to parties or counterparties. Absent if issuer unknown.
     */
    std::optional<boost::uuids::uuid> issuing_party_id;

    /**
     * @brief The identifier value.
     *
     * e.g., UTI value, internal trade reference.
     */
    std::string id_value;

    /**
     * @brief Type of identifier (e.g. UTI, USI, Internal).
     *
     * Soft FK to ores_trade_trade_id_types_tbl.
     */
    std::string id_type;

    /**
     * @brief Optional scheme qualifier for the identifier.
     *
     * e.g., the LEI namespace for UTI schemes.
     */
    std::string id_scheme;

    /**
     * @brief Username of the person who last modified this trade identifier.
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

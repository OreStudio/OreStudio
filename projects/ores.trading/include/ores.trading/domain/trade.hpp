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
#ifndef ORES_TRADING_DOMAIN_TRADE_HPP
#define ORES_TRADING_DOMAIN_TRADE_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Trade capturing FpML Trade Header properties.
 *
 * Temporal trade record. Each lifecycle event (New, Amendment, Novation,
 * etc.) creates a new temporal row for the same trade id. The internal party
 * is derived from book_id via books.party_id.
 */
struct trade final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this trade.
     *
     * Surrogate key for the trade record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Optional external trade identifier.
     *
     * e.g., UTI prefix or legacy system ID.
     */
    std::string external_id;

    /**
     * @brief Book that owns this trade.
     *
     * Soft FK to ores_refdata_books_tbl.
     */
    boost::uuids::uuid book_id;

    /**
     * @brief Portfolio this trade belongs to.
     *
     * Soft FK to ores_refdata_portfolios_tbl.
     */
    boost::uuids::uuid portfolio_id;

    /**
     * @brief UUID of the trade that replaced this one (e.g., after novation).
     *
     * Self-referencing soft FK. Absent for active trades.
     */
    std::optional<boost::uuids::uuid> successor_trade_id;

    /**
     * @brief Counterparty for this trade.
     *
     * Soft FK to ores_refdata_counterparties_tbl. Optional: some trades
     * may not have a counterparty (e.g., internal transfers).
     */
    std::optional<boost::uuids::uuid> counterparty_id;

    /**
     * @brief ORE instrument type code (e.g. Swap, FxForward, CapFloor).
     *
     * Soft FK to ores_trading_trade_types_tbl.
     */
    std::string trade_type;

    /**
     * @brief Netting set identifier for ORE aggregation.
     *
     * Groups trades under the same netting agreement.
     */
    std::string netting_set_id;

    /**
     * @brief Lifecycle event type (e.g. New, Amendment, Novation, Termination).
     *
     * Soft FK to ores_trading_lifecycle_events_tbl.
     */
    std::string lifecycle_event;

    /**
     * @brief Date the trade was agreed.
     *
     * ISO 8601 date: YYYY-MM-DD.
     */
    std::string trade_date;

    /**
     * @brief Timestamp when the trade was executed (with timezone).
     *
     * ISO 8601 timestamp: YYYY-MM-DD HH:MM:SS+TZ.
     */
    std::string execution_timestamp;

    /**
     * @brief Date from which the trade is effective.
     *
     * ISO 8601 date: YYYY-MM-DD.
     */
    std::string effective_date;

    /**
     * @brief Date on which the trade matures or terminates.
     *
     * ISO 8601 date: YYYY-MM-DD.
     */
    std::string termination_date;

    /**
     * @brief Username of the person who last modified this trade.
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

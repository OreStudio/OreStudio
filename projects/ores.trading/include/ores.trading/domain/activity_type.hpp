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
#ifndef ORES_TRADING_DOMAIN_ACTIVITY_TYPE_HPP
#define ORES_TRADING_DOMAIN_ACTIVITY_TYPE_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Internal trade activity classification.
 *
 * Each activity type classifies what happened to a trade (e.g. new_booking,
 * amendment, novation). It optionally maps to an FpML event type code for
 * wire-format messages, and optionally links to an FSM transition that drives
 * the trade's operational status change.
 *
 * Category values: new_activity, lifecycle_event, misbooking,
 *                  valuation_change, cancellation.
 */
struct activity_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique activity type code (e.g. 'new_booking', 'novation').
     */
    std::string code;

    /**
     * @brief High-level category grouping this activity type.
     *
     * One of: new_activity, lifecycle_event, misbooking,
     *         valuation_change, cancellation.
     */
    std::string category;

    /**
     * @brief Whether this activity type requires counterparty confirmation.
     */
    bool requires_confirmation = false;

    /**
     * @brief Detailed description of the activity type.
     */
    std::string description;

    /**
     * @brief Optional FpML event type code for wire-format mapping.
     *
     * Soft FK to ores_trading_fpml_event_types_tbl. Empty if no FpML equivalent.
     */
    std::string fpml_event_type_code;

    /**
     * @brief Optional FSM transition that this activity triggers.
     *
     * Soft FK to ores_dq_fsm_transitions_tbl. Nil UUID if no status change.
     */
    std::optional<boost::uuids::uuid> fsm_transition_id;

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

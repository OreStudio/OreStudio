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
#ifndef ORES_DQ_DOMAIN_FSM_STATE_HPP
#define ORES_DQ_DOMAIN_FSM_STATE_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::dq::domain {

/**
 * @brief A state within a finite state machine.
 *
 * States belong to exactly one machine and may be designated as
 * initial (entry point) or terminal (no outgoing transitions).
 */
struct fsm_state final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this state.
     */
    boost::uuids::uuid id;

    /**
     * @brief UUID of the FSM machine this state belongs to.
     */
    boost::uuids::uuid machine_id;

    /**
     * @brief Human-readable state name (e.g. "new", "live", "expired").
     *
     * Unique within a machine.
     */
    std::string name;

    /**
     * @brief Whether this is the initial state of the machine.
     */
    bool is_initial = false;

    /**
     * @brief Whether this is a terminal state (no outgoing transitions).
     */
    bool is_terminal = false;

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

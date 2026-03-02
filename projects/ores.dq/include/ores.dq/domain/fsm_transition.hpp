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
#ifndef ORES_DQ_DOMAIN_FSM_TRANSITION_HPP
#define ORES_DQ_DOMAIN_FSM_TRANSITION_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::dq::domain {

/**
 * @brief A valid state-to-state transition within a finite state machine.
 *
 * A NULL from_state_id represents an initial booking transition (no prior state).
 */
struct fsm_transition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this transition.
     */
    boost::uuids::uuid id;

    /**
     * @brief UUID of the FSM machine this transition belongs to.
     */
    boost::uuids::uuid machine_id;

    /**
     * @brief Source state UUID.
     *
     * NULL/absent for initial booking transitions (no prior state required).
     */
    std::optional<boost::uuids::uuid> from_state_id;

    /**
     * @brief Target state UUID.
     */
    boost::uuids::uuid to_state_id;

    /**
     * @brief Human-readable transition name (e.g. "confirm", "cancel_new").
     */
    std::string name;

    /**
     * @brief Optional guard function name for business rule enforcement.
     */
    std::string guard_function;

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

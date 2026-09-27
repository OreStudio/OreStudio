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
#ifndef ORES_WORKFLOW_CORE_SERVICE_FSM_STATE_MAP_HPP
#define ORES_WORKFLOW_CORE_SERVICE_FSM_STATE_MAP_HPP

#include "ores.database/domain/context.hpp"
#include "ores.workflow.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>

namespace ores::workflow::service {

/**
 * @brief Maps FSM state names to their UUIDs for a given machine.
 *
 * Loaded once at service startup, from the store dq owns. Use require() to
 * look up a state UUID or throw if the name is absent.
 */
struct fsm_state_map {
    std::unordered_map<std::string, boost::uuids::uuid> states;

    /**
     * @brief Returns the UUID for @p name or throws std::runtime_error.
     */
    [[nodiscard]] boost::uuids::uuid require(std::string_view name) const {
        const auto it = states.find(std::string(name));
        if (it == states.end())
            throw std::runtime_error("FSM state not found: " + std::string(name));
        return it->second;
    }
};

/**
 * @brief Loads the states of the machine named @p machine_name.
 *
 * The read goes to the store rather than over NATS, because a state's name is
 * unique only within its machine and the generated wire surface addresses a
 * state by name alone. The machine therefore has to be resolved where its name
 * lives, which is the database.
 *
 * Throws std::runtime_error when no machine of that name has any state, which
 * at startup means the seed is missing rather than that a workflow is broken.
 */
[[nodiscard]] ORES_WORKFLOW_CORE_EXPORT fsm_state_map
load_fsm_states(ores::database::context ctx, const std::string& machine_name);

}

#endif

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
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include "ores.dq.core/repository/fsm_state_repository.hpp"
#include <format>

namespace ores::workflow::service {

fsm_state_map load_fsm_states(ores::database::context ctx, const std::string& machine_name) {
    const auto states = ores::dq::repository::fsm_state_repository().read_latest_by_machine_name(
        ctx, machine_name);

    if (states.empty())
        throw std::runtime_error(
            std::format("No fsm states are stored for machine '{}'", machine_name));

    fsm_state_map m;
    for (const auto& s : states)
        m.states.emplace(s.name, s.id);
    return m;
}

}

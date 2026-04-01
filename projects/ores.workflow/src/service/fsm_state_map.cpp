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
#include "ores.workflow/service/fsm_state_map.hpp"

#include <format>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.dq.api/messaging/fsm_protocol.hpp"

namespace ores::workflow::service {

fsm_state_map load_fsm_states(
    ores::nats::service::nats_client& nats,
    const std::string& machine_name) {
    using namespace ores::dq::messaging;

    const auto json = rfl::json::write(
        get_fsm_states_request{.machine_name = machine_name});
    const auto msg = nats.authenticated_request(
        get_fsm_states_request::nats_subject, json);

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto result = rfl::json::read<get_fsm_states_response>(sv);
    if (!result)
        throw std::runtime_error(std::format(
            "Failed to parse fsm-states response for machine '{}': {}",
            machine_name, result.error().what()));

    if (!result->success)
        throw std::runtime_error(std::format(
            "fsm-states.list failed for machine '{}': {}",
            machine_name, result->message));

    fsm_state_map m;
    for (const auto& s : result->states)
        m.states.emplace(s.name, s.id);
    return m;
}

}

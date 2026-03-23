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
#include "ores.trading/service/trade_status_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/nil_generator.hpp>
#include "ores.dq.core/repository/fsm_transition_repository.hpp"
#include "ores.trading/repository/activity_type_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::service {

using namespace ores::logging;

boost::uuids::uuid trade_status_service::resolve_status(
    context ctx,
    const std::string& activity_type_code,
    std::optional<boost::uuids::uuid> current_status_id) {

    BOOST_LOG_SEV(lg(), debug)
        << "Resolving trade status for activity type: " << activity_type_code;

    // Activity types and FSM transitions are system-level configuration seeded
    // under the system tenant; use a system context for both lookups.
    const auto sys_ctx = ctx.with_tenant(
        utility::uuid::tenant_id::system(), ctx.actor());

    // Step 1: Load the activity type to find the linked FSM transition.
    repository::activity_type_repository at_repo;
    const auto at_results = at_repo.read_latest(sys_ctx, activity_type_code);
    if (at_results.empty()) {
        throw std::invalid_argument(
            "Unknown activity type code: " + activity_type_code);
    }
    const auto& at = at_results.front();

    // Step 2: If no FSM transition is linked, status is unchanged.
    if (!at.fsm_transition_id.has_value()) {
        BOOST_LOG_SEV(lg(), debug)
            << "Activity type has no FSM transition; status unchanged.";
        return current_status_id.value_or(boost::uuids::nil_uuid());
    }

    // Step 3: Load the FSM transition.
    dq::repository::fsm_transition_repository tr_repo;
    const auto transition = tr_repo.find_by_id(sys_ctx, *at.fsm_transition_id);
    if (!transition.has_value()) {
        throw std::logic_error(
            "FSM transition not found for id: "
            + boost::uuids::to_string(*at.fsm_transition_id));
    }

    // Step 4: Validate that the current status matches the transition's
    // from_state_id.  A null from_state_id means this is the initial booking
    // transition: the trade must not yet have a status (nil UUID or nullopt).
    const auto current_id =
        current_status_id.value_or(boost::uuids::nil_uuid());

    if (!transition->from_state_id.has_value()) {
        // Initial transition: current status must be nil (new trade).
        if (!boost::uuids::uuid(current_id).is_nil()) {
            throw std::logic_error(
                "Invalid FSM transition '" + transition->name
                + "': initial booking requires nil current status, but got "
                + boost::uuids::to_string(current_id));
        }
    } else {
        // Non-initial transition: current status must match from_state_id.
        if (current_id != *transition->from_state_id) {
            throw std::logic_error(
                "Invalid FSM transition '" + transition->name
                + "': expected from_state_id "
                + boost::uuids::to_string(*transition->from_state_id)
                + " but current status is "
                + boost::uuids::to_string(current_id));
        }
    }

    // Step 5: Return the target state.
    BOOST_LOG_SEV(lg(), debug)
        << "FSM transition '" << transition->name
        << "' applied: new status_id = "
        << boost::uuids::to_string(transition->to_state_id);
    return transition->to_state_id;
}

}

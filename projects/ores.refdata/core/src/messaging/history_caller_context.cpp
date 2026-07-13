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
#include "ores.refdata.core/messaging/history_caller_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>
#include <stdexcept>

namespace ores::refdata::messaging {

namespace {

struct caller_context_state {
    std::string tenant_id;
    std::optional<std::string> party_id;
    std::vector<std::string> visible_party_ids;
    std::string actor;
    std::vector<std::string> roles;
    std::string workspace_id;
};

}

std::string pack_history_caller_context(const ores::database::context& ctx) {
    caller_context_state state;
    state.tenant_id = ctx.tenant_id().to_string();
    if (const auto pid = ctx.party_id())
        state.party_id = boost::uuids::to_string(*pid);
    for (const auto& vpid : ctx.visible_party_ids())
        state.visible_party_ids.push_back(boost::uuids::to_string(vpid));
    state.actor = ctx.actor();
    state.roles = ctx.roles();
    state.workspace_id = ctx.workspace_id();
    return rfl::json::write(state);
}

ores::database::context
unpack_history_caller_context(const ores::database::context& base_ctx, const std::string& caller_context) {
    auto state = rfl::json::read<caller_context_state>(caller_context);
    if (!state)
        throw std::runtime_error("Invalid caller_context: not valid JSON");

    auto tid = ores::utility::uuid::tenant_id::from_string(state->tenant_id);
    if (!tid)
        throw std::runtime_error("Invalid caller_context: not a tenant_id: " + tid.error());

    boost::uuids::string_generator sg;
    ores::database::context ctx = base_ctx.with_tenant(*tid, state->actor);
    if (state->party_id) {
        std::vector<boost::uuids::uuid> visible;
        visible.reserve(state->visible_party_ids.size());
        for (const auto& vpid : state->visible_party_ids)
            visible.push_back(sg(vpid));
        ctx = base_ctx.with_party(*tid, sg(*state->party_id), std::move(visible), state->actor);
    }
    return ctx.with_roles(state->roles).with_workspace(state->workspace_id);
}

}

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
#include "ores.workflow.core/service/workflow_actor.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"

namespace ores::workflow::service {

namespace {

inline auto& workflow_actor_lg() {
    static auto instance = ores::logging::make_logger("ores.workflow.service.workflow_actor");
    return instance;
}

} // namespace

using namespace ores::logging;

std::string
actor_from_message(const ores::nats::message& msg,
                   const std::optional<ores::security::jwt::jwt_authenticator>& verifier,
                   const std::string& fallback) {

    if (!verifier)
        return fallback;

    const auto token = ores::nats::service::extract_actor_bearer(msg);
    if (token.empty())
        return fallback;

    const auto claims = verifier->validate(token);
    if (!claims) {
        BOOST_LOG_SEV(workflow_actor_lg(), warn)
            << "Caller token rejected (" << ores::security::jwt::to_string(claims.error())
            << "); attributing the run to the service account";
        return fallback;
    }

    if (const auto username = claims->username.value_or(""); !username.empty())
        return username;

    // A token that verifies but names nobody is still no actor, and the store
    // refuses one, so the run falls back rather than failing to start.
    BOOST_LOG_SEV(workflow_actor_lg(), warn)
        << "Caller token names no username; attributing the run to the service account";
    return fallback;
}

}

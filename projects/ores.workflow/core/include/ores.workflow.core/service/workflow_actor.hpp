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
#ifndef ORES_WORKFLOW_CORE_SERVICE_WORKFLOW_ACTOR_HPP
#define ORES_WORKFLOW_CORE_SERVICE_WORKFLOW_ACTOR_HPP

#include "ores.nats/domain/message.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.workflow.core/export.hpp"
#include <optional>
#include <string>

namespace ores::workflow::service {

/**
 * @brief The username a workflow start request is attributed to.
 *
 * Reads the caller's username from the token the publisher forwarded -- the
 * original end-user JWT rather than the service's own -- preferring a
 * delegated header over the last hop's Authorization. Falls back to
 * @p fallback when the message carries no token, carries one the verifier
 * rejects, or names no username.
 *
 * The fallback is deliberate rather than a failure. The engine also starts
 * workflows with no caller at all, on recovery and on its own dispatch, and
 * the service account is the truth there. A record that names nobody is not
 * an option either way, because the store refuses an empty actor.
 */
[[nodiscard]] ORES_WORKFLOW_CORE_EXPORT std::string actor_from_message(
    const ores::nats::message& msg,
    const std::optional<ores::security::jwt::jwt_authenticator>& verifier,
    const std::string& fallback);

}

#endif

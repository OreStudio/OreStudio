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
#ifndef ORES_SERVICE_SERVICE_REQUEST_CONTEXT_HPP
#define ORES_SERVICE_SERVICE_REQUEST_CONTEXT_HPP

#include <optional>
#include "ores.database/domain/context.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::service::service {

/**
 * @brief Builds a per-request database context from an inbound NATS message.
 *
 * Extracts the Bearer JWT from the Authorization header, validates it, and
 * returns a context scoped to the tenant + party encoded in the claims.
 * Falls back to @p base_ctx at any validation or parse failure so that
 * callers never receive an unusable context.
 *
 * @param base_ctx  Service-level context (tenant-neutral) to fall back to.
 * @param msg       Inbound NATS message whose headers may carry a Bearer token.
 * @param verifier  JWT authenticator. If empty, @p base_ctx is returned as-is.
 */
ores::database::context make_request_context(
    const ores::database::context& base_ctx,
    const ores::nats::message& msg,
    const std::optional<ores::security::jwt::jwt_authenticator>& verifier);

} // namespace ores::service::service

#endif

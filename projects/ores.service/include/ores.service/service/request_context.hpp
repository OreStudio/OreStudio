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

#include <expected>
#include <optional>
#include "ores.database/domain/context.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/error_code.hpp"

namespace ores::service::service {

/**
 * @brief Builds a per-request database context from an inbound NATS message.
 *
 * Extracts the Bearer JWT from the Authorization header, validates it, and
 * returns a context scoped to the tenant + party encoded in the claims.
 *
 * If @p verifier is empty the base context is returned directly (service does
 * not require authentication). Otherwise:
 * - Missing or malformed Authorization header → error_code::unauthorized
 * - Valid but expired token                   → error_code::token_expired
 * - Invalid token (bad signature, etc.)       → error_code::unauthorized
 * - Valid token                               → scoped database context
 *
 * @param base_ctx  Service-level context (tenant-neutral), returned as-is when
 *                  no verifier is present.
 * @param msg       Inbound NATS message whose headers may carry a Bearer token.
 * @param verifier  JWT authenticator. If empty, @p base_ctx is returned as-is.
 */
std::expected<ores::database::context, ores::service::error_code>
make_request_context(
    const ores::database::context& base_ctx,
    const ores::nats::message& msg,
    const std::optional<ores::security::jwt::jwt_authenticator>& verifier);

/**
 * @brief Builds a per-request database context from a raw JWT Bearer token.
 *
 * Validates @p token and returns a context scoped to the tenant + party
 * encoded in the claims.  Used by service handlers that receive a JWT token
 * string directly rather than via a NATS message header.
 *
 * - Expired token                         → error_code::token_expired
 * - Invalid token (bad signature, etc.)   → error_code::unauthorized
 * - Valid token                           → scoped database context
 *
 * @param base_ctx  Service-level context (tenant-neutral).
 * @param token     Raw JWT Bearer token (without the "Bearer " prefix).
 * @param verifier  JWT authenticator used to validate the token.
 */
std::expected<ores::database::context, ores::service::error_code>
make_context_from_jwt(
    const ores::database::context& base_ctx,
    const std::string& token,
    const ores::security::jwt::jwt_authenticator& verifier);

} // namespace ores::service::service

#endif

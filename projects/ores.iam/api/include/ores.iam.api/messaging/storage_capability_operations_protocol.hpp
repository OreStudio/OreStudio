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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_MESSAGING_STORAGE_CAPABILITY_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_STORAGE_CAPABILITY_OPERATIONS_PROTOCOL_HPP

#include "ores.security/jwt/jwt_claims.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief Asks IAM for a storage capability naming a tenant's objects.
 *
 * Sent by a service that dispatches work to a node, with its own token. The
 * caller must hold the mint permission, and the tenant must be one it may act
 * for. The grants are the exact rows the token will carry.
 */
struct mint_storage_capability_request {
    using response_type = struct mint_storage_capability_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.mint_storage_capability";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The tenant whose objects the capability reaches.
     */
    std::string tenant_id;
    /**
     * @brief The grants the capability carries, one row per operation.
     */
    std::vector<ores::security::jwt::storage_grant> grants;
};

/**
 * @brief The capability, or why there is none.
 */
struct mint_storage_capability_response {
    bool success = false;
    std::string message;
    std::string token;
    /**
     * @brief When the capability stops working, in seconds since the epoch.
     */
    std::int64_t expires_at = 0;
};

}

#endif

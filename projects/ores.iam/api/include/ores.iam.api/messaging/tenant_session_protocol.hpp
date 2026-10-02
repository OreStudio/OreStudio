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
#ifndef ORES_IAM_API_MESSAGING_TENANT_SESSION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_TENANT_SESSION_PROTOCOL_HPP

#include <string>

namespace ores::iam::messaging {

/**
 * @brief Enter one tenant as a system administrator, reading only.
 *
 * Refused to a caller outside the system tenant, to a caller already inside a
 * tenant, to a caller without iam::tenants:impersonate, and for the system
 * tenant, an unknown tenant, or a tenant with no system party yet.
 */
struct enter_tenant_request {
    using response_type = struct enter_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.enter_tenant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The id of the tenant to enter.
     */
    std::string tenant_id;
};

struct enter_tenant_response {
    bool success = false;
    /**
     * @brief Why the entry was refused, or empty.
     */
    std::string message;
    /**
     * @brief The session token scoped to the tenant.
     */
    std::string token;
    std::string tenant_id;
    std::string tenant_code;
    /**
     * @brief The tenant's name, which the screens show while inside.
     */
    std::string tenant_name;
    /**
     * @brief The tenant's system party, which the session acts as.
     */
    std::string party_id;
    /**
     * @brief The system party's name, which the screens show while inside.
     */
    std::string party_name;
    /**
     * @brief How long the session lasts. It is not refreshed.
     */
    int access_lifetime_s = 1800;
};

/**
 * @brief Leave the tenant the caller's session acts in.
 *
 * The tenant comes from the caller's token, so the request carries nothing.
 * Refused to a session that is not inside a tenant.
 */
struct leave_tenant_request {
    using response_type = struct leave_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.leave_tenant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct leave_tenant_response {
    bool success = false;
    std::string message;
};

}

#endif

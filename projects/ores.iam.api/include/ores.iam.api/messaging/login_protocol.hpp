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
#ifndef ORES_IAM_MESSAGING_LOGIN_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_LOGIN_PROTOCOL_HPP

#include <string>
#include <vector>

namespace ores::iam::messaging {

struct party_summary {
    std::string id;
    std::string name;
    std::string party_category;
    std::string business_center_code;
};

struct login_request {
    using response_type = struct login_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth.login";
    std::string principal;
    std::string password;
};

struct login_response {
    bool success = false;
    std::string account_id;
    std::string tenant_id;
    std::string tenant_name;
    std::string username;
    std::string email;
    bool password_reset_required = false;
    bool tenant_bootstrap_mode = false;
    bool party_setup_required = false;
    std::string token;
    std::string error_message;
    std::string message;
    std::string selected_party_id;
    std::vector<party_summary> available_parties;
    /**
     * @brief Token lifetime in seconds as configured on the server.
     *
     * Clients use this to arm the proactive refresh timer so that the
     * timer interval tracks any server-side configuration changes.
     */
    int access_lifetime_s = 1800;
    /**
     * @brief The IAM session UUID created for this login.
     *
     * Matches the session record in ores_iam_sessions_tbl. Clients should
     * forward this as Nats-Session-Id on every subsequent request so that
     * all calls from a single login session can be correlated in logs.
     */
    std::string session_id;
};

struct logout_request {
    using response_type = struct logout_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth.logout";
};

struct logout_response {
    bool success = false;
    std::string message;
};

struct public_key_request {
    static constexpr std::string_view nats_subject = "iam.v1.auth.public-key";
};

/**
 * @brief Request to refresh a JWT token.
 *
 * The current token is passed in the Authorization: Bearer header.
 * No request body is needed — identity is taken from the token claims.
 */
struct refresh_request {
    using response_type = struct refresh_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth.refresh";
};

/**
 * @brief Response to a token refresh request.
 */
struct refresh_response {
    bool success = false;
    std::string token;
    std::string message;
    /**
     * @brief Token lifetime in seconds for the newly issued token.
     *
     * Clients re-arm the proactive refresh timer using this value.
     */
    int access_lifetime_s = 1800;
};

/**
 * @brief Authenticates a service account and issues a JWT.
 *
 * Service accounts cannot log in with the regular password-based login path.
 * They authenticate by presenting their database user password (which is
 * stored as a SHA-256 hash in the service account row). On success the IAM
 * service creates a session and returns a short-lived RS256 JWT identical in
 * structure to a human login token.
 *
 * The @p username must match the @c username column of an existing service
 * account (i.e. the database user name such as "ores_local1_reporting_service").
 * The @p password is the plaintext database password for that user.
 */
struct service_login_request {
    using response_type = struct service_login_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth.service-login";
    std::string username;
    std::string password;
};

struct service_login_response {
    bool success = false;
    std::string token;
    std::string message;
    int access_lifetime_s = 1800;
};

}

#endif

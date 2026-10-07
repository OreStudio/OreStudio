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
#ifndef ORES_IAM_API_MESSAGING_LOGIN_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_LOGIN_PROTOCOL_HPP

#include <string>
#include <vector>

namespace ores::iam::messaging {

struct party_summary {
    std::string id;
    std::string name;
    std::string party_category;
    std::string business_center_code;
};

/**
 * @brief The database row the deployment was built against.
 *
 * The four values =ores_database_info_tbl= records when the database is
 * created or recreated: the hash of the schema it was built from, the build
 * environment, the commit, and when the database was created. The table holds
 * exactly one row. It is declared by hand and owes nothing to the generators,
 * because it records the checkout that built the database before any service
 * runs.
 */
struct database_info {
    /** @brief The hash of the SQL scripts the database was built from. */
    std::string fingerprint;
    /** @brief The build environment the database was built in. */
    std::string environment;
    /** @brief The git commit the database was built from. */
    std::string commit;
    /** @brief When the database was created or recreated. */
    std::string created;
};

struct login_request {
    using response_type = struct login_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.login";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    std::string principal;
    std::string password;
};

struct login_response {
    bool success = false;
    std::string account_id;
    std::string tenant_id;
    std::string tenant_name;
    /**
     * @brief The build the answering service runs, in full.
     *
     * A session is a session with a deployment, so the answer that opens one
     * states which build it was opened against: a client that signs in states the
     * deployment's version without having asked whether the deployment still
     * needs an administrator, and a client that did ask can see whether the two
     * answers agree.
     */
    std::string version;
    /**
     * @brief The database the deployment stores into, in full.
     *
     * The row travels with the build the answer already states, because the two
     * answer one question — what am I talking to — and the database is its third.
     * Its audience is the build's audience: everyone who may open a session may
     * read it, so it needs no subject, no permission and no read of its own. The
     * iam service reads it once per login from =ores_database_info_tbl=.
     */
    database_info database;
    std::string username;
    std::string email;
    bool password_reset_required = false;
    bool tenant_bootstrap_mode = false;
    bool party_setup_required = false;
    /**
     * @brief Set when the party provisioner wizard has completed
     * (onboarding.party = true) but the party is still Inactive. The
     * client should show a message instead of re-launching the wizard.
     */
    std::string party_setup_warning;
    std::string token;
    std::string error_message;
    /**
     * @brief The stable code a client branches on.
     *
     * Empty when the sign-in succeeded. The message beside it is for a person
     * and may change; this is what a screen branches on, so a locked account is
     * distinguishable from a wrong password without matching English prose. The
     * codes a sign-in can answer with are collected on the Entry journeys page.
     */
    std::string error_code;
    std::string message;
    std::string selected_party_id;
    std::vector<party_summary> available_parties;
    /**
     * @brief The account's stored default party, if set and among
     * @c available_parties. Empty when unset. Only meaningful when
     * @c selected_party_id is empty (multi-party login, picker step).
     */
    std::string default_party_id;
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
    static constexpr std::string_view nats_subject = "iam.v1.ops.logout";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct logout_response {
    bool success = false;
    std::string message;
};

struct public_key_request {
    static constexpr std::string_view nats_subject = "iam.v1.ops.public_key";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
};

/**
 * @brief Request to refresh a JWT token.
 *
 * The current token is passed in the Authorization: Bearer header.
 * No request body is needed — identity is taken from the token claims.
 */
struct refresh_request {
    using response_type = struct refresh_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.refresh";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
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
    static constexpr std::string_view nats_subject = "iam.v1.ops.service_login";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
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

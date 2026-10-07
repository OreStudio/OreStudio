/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_SECURITY_JWT_JWT_CLAIMS_HPP
#define ORES_SECURITY_JWT_JWT_CLAIMS_HPP

#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::security::jwt {

/**
 * @brief The audience of the token that only lets a person choose a party.
 *
 * Login issues it when an account works in more than one party. Party
 * selection is the one operation that accepts it; refresh and every service's
 * request context refuse it. No signer or verifier sets an audience, so the
 * check is by this name, and every party to it uses this constant.
 */
inline constexpr std::string_view party_selection_audience = "select_party_only";

/**
 * @brief One object grant inside a storage capability.
 *
 * A capability names the buckets and key prefixes a node may reach and the
 * operations it may perform, so the storage service answers an object-scoped
 * question without asking IAM for the caller's permissions. It is the
 * capability the Storage policy page describes: a node holds one for the job
 * it is running and nothing else.
 */
struct storage_grant final {
    /**
     * @brief The bucket the grant names.
     */
    std::string bucket;

    /**
     * @brief The key prefix the grant is confined to.
     */
    std::string key_prefix;

    /**
     * @brief The operations allowed: get, put, delete or list.
     */
    std::vector<std::string> ops;
};

/**
 * @brief Represents the claims extracted from a JWT token.
 */
struct jwt_claims final {
    /**
     * @brief Subject claim - typically the account ID.
     */
    std::string subject;

    /**
     * @brief Issuer of the token.
     */
    std::string issuer;

    /**
     * @brief Intended audience for the token.
     */
    std::string audience;

    /**
     * @brief Time when the token expires.
     */
    std::chrono::system_clock::time_point expires_at;

    /**
     * @brief Time when the token was issued.
     */
    std::chrono::system_clock::time_point issued_at;

    /**
     * @brief User roles/permissions.
     */
    std::vector<std::string> roles;

    /**
     * @brief Optional username claim.
     */
    std::optional<std::string> username;

    /**
     * @brief Optional email claim.
     */
    std::optional<std::string> email;

    /**
     * @brief Optional session ID for tracking sessions.
     *
     * When present, identifies the database session record created during
     * login, allowing proper session termination on logout and LRU-caching
     * of session state by services.
     */
    std::optional<std::string> session_id;

    /**
     * @brief Optional session start time for efficient database updates.
     *
     * The sessions table uses (id, start_time) as composite primary key
     * for TimescaleDB hypertable partitioning. Including start_time in the
     * token allows efficient UPDATE queries without full table scans.
     */
    std::optional<std::chrono::system_clock::time_point> session_start_time;

    /**
     * @brief Optional tenant ID (UUID string).
     *
     * Identifies the tenant context for the authenticated account.
     */
    std::optional<std::string> tenant_id;

    /**
     * @brief Optional party ID (UUID string, nil UUID if no party selected).
     *
     * Identifies the active party for the session.
     */
    std::optional<std::string> party_id;

    /**
     * @brief List of visible party IDs (UUID strings) for the session.
     *
     * Contains the user's own party and all descendant parties, computed
     * at login time via recursive CTE on the party hierarchy.
     */
    std::vector<std::string> visible_party_ids;

    /**
     * @brief The tenant the subject acts from, when the session is inside
     * another tenant.
     *
     * A system administrator who enters a tenant gets a token whose tenant is
     * the one entered and whose subject is still the administrator. This claim
     * names the tenant the administrator belongs to, so a reader can tell the
     * session is not one of the tenant's own.
     */
    std::optional<std::string> acting_from_tenant_id;

    /**
     * @brief The service a run token acts for, when the token is not a person's.
     *
     * The actor claim of RFC 8693. A run token names the step service that
     * exchanged the run grant, so a reader can tell which service acted and
     * the issue log can name it. Empty for a person's token.
     */
    std::optional<std::string> act;

    /**
     * @brief The run grant a run token was exchanged for.
     */
    std::optional<std::string> grant_id;

    /**
     * @brief The run a run token serves.
     *
     * The pair of grant and run keys the step services' run token cache, so
     * two runs under one grant never share a token.
     */
    std::optional<std::string> run_id;

    /**
     * @brief Object grants a storage capability allows, when the token is one.
     *
     * A session or run token carries none and reaches storage by its
     * permissions. A capability carries them and reaches only the objects they
     * name, so a node never holds a caller's whole authority.
     */
    std::vector<storage_grant> storage_grants;

    /**
     * @brief Create a claims object with issued_at set to now and
     *        expires_at set to now + ttl.
     *
     * @param ttl Token lifetime. The caller is responsible for choosing an
     *            appropriate duration; this function does not apply any
     *            default — it only captures the current clock and computes
     *            the expiry.
     */
    static jwt_claims with_ttl(std::chrono::seconds ttl) {
        jwt_claims c;
        c.issued_at = std::chrono::system_clock::now();
        c.expires_at = c.issued_at + ttl;
        return c;
    }
};

}

#endif

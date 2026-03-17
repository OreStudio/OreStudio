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

#include <string>
#include <vector>
#include <chrono>
#include <optional>

namespace ores::security::jwt {

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

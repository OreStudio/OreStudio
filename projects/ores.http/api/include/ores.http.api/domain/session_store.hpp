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
#ifndef ORES_HTTP_DOMAIN_SESSION_STORE_HPP
#define ORES_HTTP_DOMAIN_SESSION_STORE_HPP

#include "ores.http.api/export.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include <chrono>
#include <cstddef>
#include <functional>
#include <mutex>
#include <optional>
#include <string>
#include <unordered_map>

namespace ores::http::domain {

/**
 * @brief What the server holds on behalf of one signed-in caller.
 *
 * The token is here and nowhere else: the browser holds an identifier that means
 * nothing by itself.
 */
struct ORES_HTTP_API_EXPORT session_record final {
    /// The bearer token the gateway presents to the services on this caller's behalf.
    std::string token;

    /**
     * @brief What the caller may do, as the token stated it when they signed in.
     *
     * The account is =claims.subject=, the tenant =claims.tenant_id= and the party
     * =claims.party_id=, so a handler reads the same claims a bearer token would have
     * carried and does not learn where the proof came from.
     */
    ores::security::jwt::jwt_claims claims;

    /// When the session was opened.
    std::chrono::system_clock::time_point created;

    /// When it was last used, which is what the expiry slides from.
    std::chrono::system_clock::time_point last_used;
};

/**
 * @brief The sessions the gateway holds, in its own memory.
 *
 * A session is a fact about custody and not about the domain, so it is not in the
 * database: a restart signs everybody out, which is the right default for a
 * credential.
 *
 * The store keys on the SHA-256 of the identifier it hands out, so a dump of this
 * memory is not a set of usable credentials. Expiry slides on use: a session that
 * keeps working keeps living, and one that is left alone is evicted.
 *
 * Every method may be called from any thread.
 */
class ORES_HTTP_API_EXPORT session_store final {
public:
    /// How a session's expiry is decided, and how the clock is read.
    using clock = std::function<std::chrono::system_clock::time_point()>;

    /**
     * @brief Opens a store whose sessions expire this long after their last use.
     *
     * @param ttl how long a quiet session lives
     * @param now the clock, so a test can decide what time it is
     */
    explicit session_store(
        std::chrono::seconds ttl, clock now = [] { return std::chrono::system_clock::now(); });

    /**
     * @brief Keeps a record and returns the identifier the caller may hold.
     *
     * A new identifier is minted every time, so a session that is replaced cannot
     * be taken over by whoever held the old one.
     */
    std::string open(session_record record);

    /**
     * @brief Finds a session and slides its expiry.
     *
     * @returns nothing when the identifier is unknown or the session has expired.
     */
    std::optional<session_record> touch(const std::string& identifier);

    /**
     * @brief Finds a session without sliding its expiry.
     */
    [[nodiscard]] std::optional<session_record> find(const std::string& identifier) const;

    /**
     * @brief Ends a session.
     *
     * @returns whether there was one to end.
     */
    bool close(const std::string& identifier);

    /**
     * @brief Removes the sessions whose expiry has passed.
     *
     * @returns how many were removed.
     */
    std::size_t evict_expired();

    /**
     * @brief Replaces a session's identifier, keeping what it holds.
     *
     * Signing in mints a new identifier and so does a change of privilege: what a
     * caller holds before the change never outlives it.
     *
     * @returns the new identifier, or nothing when the session is gone.
     */
    std::optional<std::string> rotate(const std::string& identifier);

    /// How many sessions the store holds, expired or not.
    [[nodiscard]] std::size_t size() const;

    /**
     * @brief The key the store keeps a session under.
     *
     * Exposed so a test can say what the store does not hold: the identifier the
     * caller presents never appears as a key.
     */
    [[nodiscard]] static std::string key_of(const std::string& identifier);

private:
    struct entry final {
        session_record record;
        std::chrono::system_clock::time_point expires;
    };

    [[nodiscard]] bool expired(const entry& kept) const;

    mutable std::mutex mutex_;
    std::chrono::seconds ttl_;
    clock now_;
    std::unordered_map<std::string, entry> entries_;
};

}

#endif

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
#ifndef ORES_HTTP_API_MCP_SESSION_REGISTRY_HPP
#define ORES_HTTP_API_MCP_SESSION_REGISTRY_HPP

#include "ores.http.api/export.hpp"
#include <chrono>
#include <cstddef>
#include <mutex>
#include <string>
#include <unordered_map>

namespace ores::http::mcp {

/**
 * @brief The MCP sessions an endpoint has opened, each bound to its owner.
 *
 * An Mcp-Session-Id is a transport identifier, not a credential: every request
 * still authenticates with the agent's key. A session is bound to the person
 * and agent that opened it, so an identifier another agent learns names no
 * session for it. A session left idle past its limit ends. An owner holds at
 * most a few sessions, and opening one more ends its own oldest, so one key
 * cannot crowd out another's; when the registry is still full, the session
 * used longest ago ends to make room.
 */
class ORES_HTTP_API_EXPORT session_registry final {
public:
    using clock = std::chrono::steady_clock;

    enum class standing { live, absent };

    explicit session_registry(std::chrono::seconds idle_limit = std::chrono::hours(1),
                              std::size_t capacity = 10000,
                              std::size_t per_owner = 16);

    /**
     * @brief Opens a session for an owner and answers its identifier.
     */
    std::string open(const std::string& owner, const std::string& protocol_version);

    /**
     * @brief Whether a session is live for this owner, and keeps it alive if so.
     *
     * A session another owner opened is absent, as one never opened is.
     */
    standing touch(const std::string& identifier, const std::string& owner);

    /**
     * @brief Ends an owner's session. Answers false when it has none by that name.
     */
    bool close(const std::string& identifier, const std::string& owner);

    [[nodiscard]] std::size_t size() const;

private:
    struct entry final {
        std::string owner;
        std::string protocol_version;
        clock::time_point last_used;
    };

    void evict_idle(clock::time_point now);

    std::chrono::seconds idle_limit_;
    std::size_t capacity_;
    std::size_t per_owner_;
    mutable std::mutex mutex_;
    std::unordered_map<std::string, entry> entries_;
};

}

#endif

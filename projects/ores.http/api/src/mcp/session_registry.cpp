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
#include "ores.http.api/mcp/session_registry.hpp"
#include "ores.http.api/domain/session_store.hpp"
#include <algorithm>

namespace ores::http::mcp {

session_registry::session_registry(std::chrono::seconds idle_limit,
                                   std::size_t capacity,
                                   std::size_t per_owner)
    : idle_limit_(idle_limit)
    , capacity_(capacity)
    , per_owner_(per_owner) {}

void session_registry::evict_idle(clock::time_point now) {
    std::erase_if(entries_,
                  [&](const auto& item) { return now - item.second.last_used > idle_limit_; });
}

std::string session_registry::open(const std::string& owner, const std::string& protocol_version) {
    const auto now = clock::now();
    std::scoped_lock lock(mutex_);
    evict_idle(now);
    const auto owned = std::ranges::count_if(
        entries_, [&](const auto& item) { return item.second.owner == owner; });
    if (owned > 0 && static_cast<std::size_t>(owned) >= per_owner_) {
        auto oldest = entries_.end();
        for (auto it = entries_.begin(); it != entries_.end(); ++it)
            if (it->second.owner == owner &&
                (oldest == entries_.end() || it->second.last_used < oldest->second.last_used))
                oldest = it;
        entries_.erase(oldest);
    }
    if (!entries_.empty() && entries_.size() >= capacity_) {
        const auto oldest = std::ranges::min_element(
            entries_, {}, [](const auto& item) { return item.second.last_used; });
        entries_.erase(oldest);
    }
    auto identifier = domain::mint_session_identifier();
    entries_.emplace(identifier, entry{owner, protocol_version, now});
    return identifier;
}

session_registry::standing session_registry::touch(const std::string& identifier,
                                                   const std::string& owner) {
    const auto now = clock::now();
    std::scoped_lock lock(mutex_);
    const auto found = entries_.find(identifier);
    if (found == entries_.end() || found->second.owner != owner)
        return standing::absent;
    if (now - found->second.last_used > idle_limit_) {
        entries_.erase(found);
        return standing::absent;
    }
    found->second.last_used = now;
    return standing::live;
}

bool session_registry::close(const std::string& identifier, const std::string& owner) {
    std::scoped_lock lock(mutex_);
    const auto found = entries_.find(identifier);
    if (found == entries_.end() || found->second.owner != owner)
        return false;
    entries_.erase(found);
    return true;
}

std::size_t session_registry::size() const {
    std::scoped_lock lock(mutex_);
    return entries_.size();
}

}

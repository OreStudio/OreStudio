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
#include "ores.http.core/stream/change_coalescer.hpp"

#include <utility>

namespace ores::http::stream {

void change_coalescer::record(const change_event& event) {
    if (event.entity.empty() || event.id.empty())
        return;
    const std::lock_guard lock(mu_);
    dirty_[{event.tenant, event.party, event.entity}].insert(event.id);
}

std::vector<coalesced_frame> change_coalescer::drain() {
    std::map<key, std::set<std::string>> taken;
    {
        const std::lock_guard lock(mu_);
        taken.swap(dirty_);
    }

    std::vector<coalesced_frame> frames;
    frames.reserve(taken.size());
    for (auto& [key, ids] : taken) {
        const auto& [tenant, party, entity] = key;
        frames.push_back({
            .tenant = tenant,
            .party = party,
            .entity = entity,
            .ids = {ids.begin(), ids.end()},
        });
    }
    return frames;
}

std::size_t change_coalescer::pending() const {
    const std::lock_guard lock(mu_);
    std::size_t total = 0;
    for (const auto& [key, ids] : dirty_)
        total += ids.size();
    return total;
}

}

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
#include "ores.marketdata.service/app/adapter_registry.hpp"
#include <optional>

namespace ores::marketdata::service::app {

std::string_view to_string(adapter_state state) {
    switch (state) {
        case adapter_state::announced:
            return "announced";
        case adapter_state::ready:
            return "ready";
        case adapter_state::serving:
            return "serving";
        case adapter_state::stale:
            return "stale";
        case adapter_state::rejected:
            return "rejected";
    }
    return "announced";
}

messaging::adapter_announce_response
adapter_registry::announce(const messaging::adapter_announce_request& request,
                           const universe_check& check,
                           std::chrono::system_clock::time_point now) {
    messaging::adapter_announce_response response;

    entry fresh;
    fresh.status.adapter_id = request.adapter_id;
    fresh.status.provider_code = request.provider_code;
    fresh.status.last_heartbeat_at = now;
    fresh.state = adapter_state::announced;

    if (request.adapter_id.empty()) {
        response.message = "The adapter id is empty.";
        return response;
    }

    if (!check.provider_known(request.provider_code)) {
        fresh.state = adapter_state::rejected;
        response.message = "The provider " + request.provider_code + " is not in the catalogue.";
    } else {
        for (const auto& id : request.universe) {
            if (check.id_known(request.provider_code, id))
                ++response.served_count;
            else
                ++response.unknown_count;
        }
        fresh.state = adapter_state::ready;
        response.success = true;
    }
    fresh.status.served_count = response.served_count;
    fresh.status.unknown_count = response.unknown_count;
    fresh.status.state = std::string(to_string(fresh.state));

    std::lock_guard lock(mu_);
    adapters_[request.adapter_id] = std::move(fresh);
    return response;
}

heartbeat_outcome adapter_registry::heartbeat(const std::string& adapter_id,
                                              std::uint64_t counter,
                                              std::chrono::system_clock::time_point now) {
    std::lock_guard lock(mu_);
    const auto found = adapters_.find(adapter_id);
    if (found == adapters_.end())
        return {};
    auto& e = found->second;
    e.status.last_heartbeat_at = now;
    e.status.heartbeat_counter = counter;

    heartbeat_outcome outcome{.known = true, .recovered = false};
    if (e.state == adapter_state::stale) {
        e.state = adapter_state::ready;
        e.status.state = std::string(to_string(e.state));
        outcome.recovered = true;
    }
    return outcome;
}

std::vector<std::string> adapter_registry::expire(std::chrono::system_clock::time_point now) {
    std::vector<std::string> stale;
    std::lock_guard lock(mu_);
    for (auto& [id, e] : adapters_) {
        if (e.state == adapter_state::stale || e.state == adapter_state::rejected)
            continue;
        if (now - e.status.last_heartbeat_at > heartbeat_timeout_) {
            e.state = adapter_state::stale;
            e.status.state = std::string(to_string(e.state));
            stale.push_back(id);
        }
    }
    return stale;
}

void adapter_registry::demand_replied(const std::string& adapter_id,
                                      std::size_t demanded,
                                      const messaging::adapter_demand_response& response) {
    std::lock_guard lock(mu_);
    const auto found = adapters_.find(adapter_id);
    if (found == adapters_.end())
        return;
    auto& e = found->second;
    if (e.state == adapter_state::stale || e.state == adapter_state::rejected)
        return;
    e.status.demanded_count = static_cast<std::uint32_t>(demanded);
    e.status.accepted = response.accepted;
    e.status.rejected = response.rejected;
    e.state = response.accepted.empty() ? adapter_state::ready : adapter_state::serving;
    e.status.state = std::string(to_string(e.state));
}

std::optional<adapter_state> adapter_registry::state_of(const std::string& adapter_id) const {
    std::lock_guard lock(mu_);
    const auto found = adapters_.find(adapter_id);
    if (found == adapters_.end())
        return std::nullopt;
    return found->second.state;
}

std::optional<std::string> adapter_registry::provider_of(const std::string& adapter_id) const {
    std::lock_guard lock(mu_);
    const auto found = adapters_.find(adapter_id);
    if (found == adapters_.end())
        return std::nullopt;
    return found->second.status.provider_code;
}

std::vector<std::string> adapter_registry::demandable() const {
    std::vector<std::string> ids;
    std::lock_guard lock(mu_);
    for (const auto& [id, e] : adapters_) {
        if (e.state != adapter_state::stale && e.state != adapter_state::rejected)
            ids.push_back(id);
    }
    return ids;
}

std::vector<messaging::adapter_status> adapter_registry::status() const {
    std::vector<messaging::adapter_status> out;
    std::lock_guard lock(mu_);
    out.reserve(adapters_.size());
    for (const auto& [id, e] : adapters_)
        out.push_back(e.status);
    return out;
}

}

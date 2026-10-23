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
#ifndef ORES_MARKETDATA_SERVICE_APP_ADAPTER_REGISTRY_HPP
#define ORES_MARKETDATA_SERVICE_APP_ADAPTER_REGISTRY_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include <chrono>
#include <cstdint>
#include <functional>
#include <map>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief The state of an adapter as the service sees it.
 */
enum class adapter_state {
    /// The adapter has sent its provider and universe.
    announced,
    /// The universe agrees with the catalogue and no demand is set.
    ready,
    /// The adapter holds a non-empty demand set.
    serving,
    /// The adapter missed its heartbeat.
    stale,
    /// The provider is unknown to the catalogue.
    rejected
};

[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::string_view to_string(adapter_state state);

/**
 * @brief What the registry asks the catalogue when an adapter announces itself.
 */
struct universe_check {
    /// Whether the catalogue holds the provider.
    std::function<bool(const std::string& provider_code)> provider_known;
    /// Whether the catalogue holds the vendor id for the provider.
    std::function<bool(const std::string& provider_code, const messaging::vendor_id& id)> id_known;
};

/**
 * @brief What a heartbeat changed.
 */
struct heartbeat_outcome {
    /// The adapter announced itself earlier. A heartbeat of any other adapter changes nothing.
    bool known = false;
    /// The adapter was stale and is not any longer, so the service sends its demand again.
    bool recovered = false;
};

/**
 * @brief The state of each adapter, and the changes of state the four messages cause.
 *
 * An announce checks the provider and the universe against the catalogue: an unknown provider
 * makes the adapter @c rejected, and any other makes it @c ready. An accepted demand reply makes
 * it @c serving, and a reply that accepts nothing makes it @c ready. An adapter that sends no
 * heartbeat for the timeout becomes @c stale, and keeps its routes and its last reply. A
 * heartbeat of a stale adapter makes it @c ready again and tells the caller to send the demand
 * set once more. The registry reads no clock: the caller states the time of each call. A mutex
 * guards the state.
 */
class ORES_MARKETDATA_SERVICE_EXPORT adapter_registry {
public:
    /// The time without a heartbeat after which an adapter is stale.
    static constexpr std::chrono::seconds default_heartbeat_timeout{15};

    explicit adapter_registry(
        std::chrono::milliseconds heartbeat_timeout = default_heartbeat_timeout)
        : heartbeat_timeout_(heartbeat_timeout) {}

    /// Records the announce of an adapter and returns the answer to it.
    messaging::adapter_announce_response
    announce(const messaging::adapter_announce_request& request,
             const universe_check& check,
             std::chrono::system_clock::time_point now);

    /// Records a heartbeat.
    heartbeat_outcome heartbeat(const std::string& adapter_id,
                                std::uint64_t counter,
                                std::chrono::system_clock::time_point now);

    /// Makes stale every adapter that has sent no heartbeat for the timeout, and returns their ids.
    std::vector<std::string> expire(std::chrono::system_clock::time_point now);

    /// Records the answer of an adapter to a demand set of @p demanded ids.
    void demand_replied(const std::string& adapter_id,
                        std::size_t demanded,
                        const messaging::adapter_demand_response& response);

    /// The state of one adapter, or nothing for an adapter that never announced itself.
    [[nodiscard]] std::optional<adapter_state> state_of(const std::string& adapter_id) const;

    /// The provider an adapter announced, or nothing for an adapter that never announced itself.
    [[nodiscard]] std::optional<std::string> provider_of(const std::string& adapter_id) const;

    /// The ids of the adapters that can hold a demand set: every adapter that is neither stale nor
    /// rejected.
    [[nodiscard]] std::vector<std::string> demandable() const;

    /// Every adapter, in the order of their ids.
    [[nodiscard]] std::vector<messaging::adapter_status> status() const;

private:
    struct entry {
        messaging::adapter_status status;
        adapter_state state = adapter_state::announced;
    };

    std::chrono::milliseconds heartbeat_timeout_;
    mutable std::mutex mu_;
    std::map<std::string, entry> adapters_;
};

}

#endif

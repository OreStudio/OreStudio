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
#ifndef ORES_EVENTING_SERVICE_EVENT_CHANNEL_REGISTRY_HPP
#define ORES_EVENTING_SERVICE_EVENT_CHANNEL_REGISTRY_HPP

#include <mutex>
#include <vector>
#include <string>
#include <unordered_map>
#include "ores.eventing/domain/event_channel_info.hpp"
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::eventing::service {

/**
 * @brief Registry of available event channels.
 *
 * Tracks all event channels that clients can subscribe to, along with
 * their descriptions. Channels are registered during application
 * initialization when entity-to-event mappings are configured.
 *
 * Thread Safety:
 * - All operations are thread-safe.
 * - Typically, channels are registered at startup before concurrent access.
 *
 * Usage:
 * @code
 *     event_channel_registry registry;
 *
 *     // Register channels during initialization
 *     registry.register_channel<currency_changed_event>("Currency data modified");
 *
 *     // Later, query available channels
 *     auto channels = registry.get_channels();
 * @endcode
 */
class event_channel_registry final {
public:
    event_channel_registry() = default;
    ~event_channel_registry() = default;

    event_channel_registry(const event_channel_registry&) = delete;
    event_channel_registry& operator=(const event_channel_registry&) = delete;
    event_channel_registry(event_channel_registry&&) = delete;
    event_channel_registry& operator=(event_channel_registry&&) = delete;

    /**
     * @brief Register a channel for a typed event.
     *
     * Uses event_traits to get the channel name automatically.
     *
     * @tparam Event The domain event type (must have event_traits specialization).
     * @param description Human-readable description of the channel.
     */
    template<domain::has_event_traits Event>
    void register_channel(const std::string& description) {
        using traits = domain::event_traits<Event>;
        register_channel(std::string{traits::name}, description);
    }

    /**
     * @brief Register a channel by name.
     *
     * @param name The channel name (e.g., "ores.refdata.currency_changed").
     * @param description Human-readable description of the channel.
     */
    void register_channel(const std::string& name, const std::string& description);

    /**
     * @brief Get all registered channels.
     *
     * @return Vector of event channel info, sorted by name.
     */
    [[nodiscard]] std::vector<domain::event_channel_info> get_channels() const;

    /**
     * @brief Check if a channel is registered.
     *
     * @param name The channel name to check.
     * @return true if the channel is registered.
     */
    [[nodiscard]] bool is_registered(const std::string& name) const;

    /**
     * @brief Get the number of registered channels.
     *
     * @return The count of registered channels.
     */
    [[nodiscard]] std::size_t size() const;

private:
    mutable std::mutex mutex_;
    std::unordered_map<std::string, domain::event_channel_info> channels_;
};

}

#endif

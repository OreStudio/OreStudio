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
#ifndef ORES_EVENTING_SERVICE_POSTGRES_EVENT_SOURCE_HPP
#define ORES_EVENTING_SERVICE_POSTGRES_EVENT_SOURCE_HPP

#include <memory>
#include <functional>
#include <unordered_map>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.eventing/domain/entity_change_event.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.eventing/service/postgres_listener_service.hpp"

namespace ores::eventing::service {

/**
 * @brief Event source that bridges PostgreSQL LISTEN/NOTIFY to the event bus.
 *
 * This class wraps postgres_listener_service and translates low-level
 * entity_change_event notifications into typed domain events that are
 * published to the event bus.
 *
 * Components can register their entity-to-event mappings via register_mapping(),
 * enabling the event source to automatically publish the correct typed event
 * when a database notification is received.
 *
 * Usage:
 * @code
 *     event_bus bus;
 *     postgres_event_source source(ctx, bus);
 *
 *     // Register entity mappings
 *     source.register_mapping<currency_changed_event>("ores.risk.currency",
 *         "ores_currencies");
 *
 *     source.start();
 * @endcode
 */
class postgres_event_source final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(
            "ores.eventing.service.postgres_event_source");
        return instance;
    }

    /**
     * @brief Type-erased publisher function.
     *
     * Takes a timestamp and entity_ids, publishes the appropriate typed event.
     */
    using publisher_fn = std::function<void(
        std::chrono::system_clock::time_point,
        const std::vector<std::string>&)>;

    struct entity_mapping {
        std::string channel_name;
        publisher_fn publisher;
    };

public:
    /**
     * @brief Constructs a postgres_event_source.
     *
     * @param ctx Database context for the listener connection.
     * @param bus Reference to the event bus for publishing events.
     */
    postgres_event_source(database::context ctx, event_bus& bus);

    ~postgres_event_source();

    postgres_event_source(const postgres_event_source&) = delete;
    postgres_event_source& operator=(const postgres_event_source&) = delete;

    /**
     * @brief Register a mapping from entity name to typed domain event.
     *
     * When a notification is received for the specified entity, the event
     * source will publish an instance of Event to the bus with the
     * notification's timestamp.
     *
     * @tparam Event The domain event type to publish (must have a timestamp member).
     * @param entity_name The fully qualified entity name (e.g., "ores.risk.currency").
     * @param channel_name The PostgreSQL channel to listen on (e.g., "ores_currencies").
     */
    template<typename Event>
    void register_mapping(const std::string& entity_name,
                          const std::string& channel_name) {
        using namespace ores::telemetry::log;
        BOOST_LOG_SEV(lg(), info)
            << "Registering entity-to-event mapping: entity='" << entity_name
            << "', channel='" << channel_name << "'";

        entity_mappings_[entity_name] = entity_mapping{
            .channel_name = channel_name,
            .publisher = [this, entity_name](std::chrono::system_clock::time_point ts,
                                              const std::vector<std::string>& entity_ids) {
                using namespace ores::telemetry::log;
                BOOST_LOG_SEV(lg(), info)
                    << "Publishing domain event for entity: " << entity_name
                    << " with " << entity_ids.size() << " entity IDs";
                bus_.publish(Event{ts, entity_ids});
            }
        };

        listener_.subscribe(channel_name);
        BOOST_LOG_SEV(lg(), debug)
            << "Subscribed to PostgreSQL channel: " << channel_name;
    }

    /**
     * @brief Start the event source.
     *
     * Begins listening for PostgreSQL notifications on all registered channels.
     */
    void start();

    /**
     * @brief Stop the event source.
     *
     * Stops listening for notifications.
     */
    void stop();

private:
    /**
     * @brief Handle incoming entity change events.
     *
     * Maps the entity name to the appropriate typed event and publishes it.
     */
    void on_entity_change(const domain::entity_change_event& e);

    event_bus& bus_;
    postgres_listener_service listener_;
    std::unordered_map<std::string, entity_mapping> entity_mappings_;
};

}

#endif

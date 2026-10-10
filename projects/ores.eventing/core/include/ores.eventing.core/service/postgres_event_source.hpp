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
#ifndef ORES_EVENTING_CORE_SERVICE_POSTGRES_EVENT_SOURCE_HPP
#define ORES_EVENTING_CORE_SERVICE_POSTGRES_EVENT_SOURCE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/postgres_listener_service.hpp"
#include "ores.eventing.api/domain/entity_event.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <atomic>
#include <chrono>
#include <cstdint>
#include <functional>
#include <memory>
#include <unordered_map>

namespace ores::eventing::service {

/**
 * @brief Event source that bridges PostgreSQL LISTEN/NOTIFY to the event bus.
 *
 * This class wraps postgres_listener_service and turns the notify trigger's
 * canonical notification into events on the event bus. A channel registered
 * with register_entity_event_mapping() publishes two things for each
 * notification: the typed event its traits name, converted through the event's
 * own key record, and an entity_change_event carrying the entity's name, the
 * changed ids and the tenant. The second is for an in-process subscriber that
 * must know the tenant, which the typed event does not state; it filters on
 * the entity name.
 *
 * Usage:
 * @code
 *     event_bus bus;
 *     postgres_event_source source(ctx, bus);
 *
 *     source.register_entity_event_mapping<country_event>("ores_refdata_countries");
 *
 *     source.start();
 * @endcode
 */
class ORES_EVENTING_CORE_EXPORT postgres_event_source final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger("ores.eventing.service.postgres_event_source");
        return instance;
    }

    /**
     * @brief Type-erased publisher for one canonical entity event.
     *
     * Takes the store's notification and publishes the typed event the
     * notification names.
     */
    using entity_event_publisher_fn = std::function<void(const domain::entity_event_notification&)>;

    struct entity_event_mapping {
        std::string channel_name;
        entity_event_publisher_fn publisher;
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
     * @brief Register a mapping from a canonical event's channel to its type.
     *
     * The notification trigger publishes the specification's event fields on
     * @p channel_name: the event identity, the key, the action, the version,
     * the time and the correlation. The mapping converts each notification
     * into @p Event -- which knows its own key record -- and publishes it on
     * the bus, then publishes the notification as an entity_change_event for
     * a subscriber that needs the tenant. A subscriber then publishes the
     * typed event to NATS on the subject the action names.
     *
     * The conversion is read from the event's own traits, because only the
     * event knows the columns its key carries.
     *
     * @tparam Event The generated event type, which must specialize
     * entity_event_traits.
     * @param channel_name The PostgreSQL channel to listen on.
     */
    template <typename Event>
    void register_entity_event_mapping(const std::string& channel_name) {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), info) << "Registering canonical event mapping: channel='"
                                  << channel_name << "', prefix='"
                                  << domain::entity_event_traits<Event>::subject_prefix << "'";

        entity_event_mappings_[channel_name] = entity_event_mapping{
            .channel_name = channel_name,
            .publisher = [this,
                          channel_name](const domain::entity_event_notification& notification) {
                bus_.publish(domain::entity_event_traits<Event>::from_notification(notification));
            }};

        listener_.subscribe(channel_name);
        BOOST_LOG_SEV(lg(), debug) << "Subscribed to PostgreSQL channel: " << channel_name;
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

    /**
     * @brief Blocks until the underlying listener has issued LISTEN for
     * every channel registered so far, or @p timeout elapses.
     *
     * The listener thread issues LISTEN asynchronously after start(); a
     * write performed before LISTEN is actually registered on the
     * connection produces no notification (Postgres does not queue
     * NOTIFYs sent before a matching LISTEN). Callers -- in particular
     * tests -- that write immediately after start() should wait on this
     * first, rather than sleep for a guessed duration.
     *
     * @return true if the listener became ready before the timeout.
     */
    [[nodiscard]] bool
    wait_until_ready(std::chrono::milliseconds timeout = std::chrono::seconds(2));

private:
    event_bus& bus_;
    ores::database::service::postgres_listener_service listener_;
    std::unordered_map<std::string, entity_event_mapping> entity_event_mappings_;
    std::string registered_entities_;
    std::atomic<std::uint64_t> parse_failure_count_{0};
};

}

#endif

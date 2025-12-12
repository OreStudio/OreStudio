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
#ifndef ORES_EVENTING_SERVICE_REGISTRAR_HPP
#define ORES_EVENTING_SERVICE_REGISTRAR_HPP

#include "ores.utility/log/make_logger.hpp"
#include "ores.eventing/service/postgres_event_source.hpp"

namespace ores::eventing::service {

/**
 * @brief Helper class for registering entity-to-event mappings.
 *
 * Each component should call register_mapping() with their event types
 * during application initialization.
 *
 * Usage:
 * @code
 *     // In ores.risk registration code:
 *     eventing::service::registrar::register_mapping<currency_changed_event>(
 *         source, "ores.risk.currency", "ores_currencies");
 * @endcode
 */
class registrar {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.eventing.service.registrar");
        return instance;
    }

public:
    /**
     * @brief Register a single entity-to-event mapping.
     *
     * @tparam Event The domain event type to publish.
     * @param source The postgres event source to configure.
     * @param entity_name The fully qualified entity name.
     * @param channel_name The PostgreSQL channel to listen on.
     */
    template<typename Event>
    static void register_mapping(postgres_event_source& source,
                                 const std::string& entity_name,
                                 const std::string& channel_name) {
        BOOST_LOG_SEV(lg(), utility::log::info)
            << "Registering event mapping: " << entity_name
            << " -> " << channel_name;
        source.register_mapping<Event>(entity_name, channel_name);
    }
};

}

#endif

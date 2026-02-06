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
#ifndef ORES_ASSETS_EVENTING_ASSETS_CHANGED_EVENT_HPP
#define ORES_ASSETS_EVENTING_ASSETS_CHANGED_EVENT_HPP

#include <chrono>
#include <vector>
#include <string>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::assets::eventing {

/**
 * @brief Domain event indicating that asset data has changed.
 *
 * This event is published when currency-image mappings are created,
 * updated, or deleted. Clients (e.g., ImageCache) subscribe to this
 * event to invalidate their cached data and reload the latest images.
 */
struct assets_changed_event final {
    /**
     * @brief The timestamp of when the change occurred (in UTC).
     *
     * Clients can use this timestamp to query the database for entities
     * that have changed since this point.
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief ISO codes of currencies whose image mappings changed.
     *
     * Contains the ISO 4217 codes (e.g., "USD", "EUR") of currencies that
     * had their image mappings created, updated, or deleted.
     */
    std::vector<std::string> iso_codes;

    /**
     * @brief The tenant that owns the changed entity.
     */
    std::string tenant_id;
};

} // namespace ores::assets::eventing

namespace ores::eventing::domain {

/**
 * @brief Event traits specialization for assets_changed_event.
 */
template<>
struct event_traits<ores::assets::eventing::assets_changed_event> {
    static constexpr std::string_view name = "ores.assets.assets_changed";
};

} // namespace ores::eventing::domain

#endif // ORES_ASSETS_EVENTING_ASSETS_CHANGED_EVENT_HPP

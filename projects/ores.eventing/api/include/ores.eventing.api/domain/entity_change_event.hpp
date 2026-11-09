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
#ifndef ORES_EVENTING_API_DOMAIN_ENTITY_CHANGE_EVENT_HPP
#define ORES_EVENTING_API_DOMAIN_ENTITY_CHANGE_EVENT_HPP

#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <chrono>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

namespace ores::eventing::domain {

/**
 * @brief Represents a low-level notification about a change to an entity at the
 * repository level.
 */
struct entity_change_event final {
    /**
     * @brief The fully qualified name of the entity that changed (e.g.,
     * "ores.refdata.currency").
     */
    std::string entity;

    /**
     * @brief The timestamp of when the change occurred (in UTC).
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief Identifiers of the specific entities that changed.
     *
     * For currencies, this contains ISO codes. For accounts, UUIDs.
     * May be empty if the trigger doesn't provide specific IDs.
     */
    std::vector<std::string> entity_ids;

    /**
     * @brief The tenant that owns the changed entity.
     *
     * Used for multi-tenancy isolation: notifications are only delivered
     * to sessions belonging to the same tenant.
     */
    std::string tenant_id;
};

/**
 * @brief The members of entity_change_event, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<entity_change_event>) {
    using ::ores::utility::codec::field;
    return std::tuple{field{"entity", &entity_change_event::entity},
                      field{"timestamp", &entity_change_event::timestamp},
                      field{"entity_ids", &entity_change_event::entity_ids},
                      field{"tenant_id", &entity_change_event::tenant_id}};
}

static_assert(::ores::utility::codec::names_every_member<entity_change_event>(),
              "entity_change_event gained a member its wire_fields does not name.");

}

template <>
struct ores::utility::codec::wire_traits<ores::eventing::domain::entity_change_event>
    : ores::utility::codec::shaped_wire_traits<ores::eventing::domain::entity_change_event> {};

#endif

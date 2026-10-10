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
#ifndef ORES_EVENTING_API_DOMAIN_EVENT_TRAITS_HPP
#define ORES_EVENTING_API_DOMAIN_EVENT_TRAITS_HPP

#include <string_view>

namespace ores::eventing::domain {

/**
 * @brief Traits template for mapping event types to their logical names.
 *
 * An in-process domain event that is not a change to an entity, such as a
 * role being assigned, specializes this template to name itself on the bus.
 * A change to an entity does not use it: that event states its NATS subject
 * through entity_event_traits.
 *
 * Example:
 * @code
 *     template<>
 *     struct event_traits<role_assigned_event> {
 *         static constexpr std::string_view name = "ores.iam.role_assigned";
 *     };
 * @endcode
 *
 * @tparam Event The event type.
 */
template <typename Event>
struct event_traits {
    // Primary template is intentionally not defined.
    // Each event type must provide its own specialization.
    // This causes a compile-time error if an event is used without traits.
};

/**
 * @brief Concept for types that have event_traits specialization.
 */
template <typename T>
concept has_event_traits = requires {
    { event_traits<T>::name } -> std::convertible_to<std::string_view>;
};

}

#endif

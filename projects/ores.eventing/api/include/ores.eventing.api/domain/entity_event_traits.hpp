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
#ifndef ORES_EVENTING_API_DOMAIN_ENTITY_EVENT_TRAITS_HPP
#define ORES_EVENTING_API_DOMAIN_ENTITY_EVENT_TRAITS_HPP

#include "ores.eventing.api/domain/entity_event.hpp"
#include <string>
#include <string_view>

namespace ores::eventing::domain {

/**
 * @brief Traits template for a canonical entity event.
 *
 * A component's generated event type specializes this to say two things: the
 * prefix of the subjects it is published on, and how the store's notification
 * becomes the typed event. The conversion belongs to the event because only it
 * knows the columns its key record carries.
 *
 * The prefix names the events collection, and the action is the last segment:
 *
 * @code
 *     template<>
 *     struct entity_event_traits<ores::iam::messaging::tenant_type_event> {
 *         static constexpr std::string_view subject_prefix =
 *             "iam.v1.tenant_types_events";
 *         static ores::iam::messaging::tenant_type_event
 *         from_notification(const entity_event_notification& n);
 *     };
 * @endcode
 *
 * @tparam Event The generated event type.
 */
template <typename Event>
struct entity_event_traits;

/**
 * @brief The subject one action of an event is published on.
 *
 * One payload is addressed by three subjects, so the subject is the prefix
 * plus the action the payload reports. A subscriber that wants one action
 * subscribes to one subject; a subscriber that wants all three subscribes to
 * the prefix's wildcard.
 */
template <typename Event>
[[nodiscard]] std::string event_subject(std::string_view action) {
    std::string subject(entity_event_traits<Event>::subject_prefix);
    subject.push_back('.');
    subject.append(action);
    return subject;
}

}

#endif

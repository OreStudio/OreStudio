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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_changed_event.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_EVENTING_BOOK_PURPOSE_TYPE_EVENT_HPP
#define ORES_REFDATA_API_EVENTING_BOOK_PURPOSE_TYPE_EVENT_HPP

#include "ores.eventing.api/domain/entity_event.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.refdata.api/messaging/book_purpose_type_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <rfl/json.hpp>
#include <string>

namespace ores::eventing::domain {

/**
 * @brief Event traits for book purpose type's changes.
 *
 * The events collection is addressed by three subjects, one per action, so the
 * traits state the prefix and the action completes it. The conversion is this
 * entity's own because the notification carries the key as a JSON object and
 * only this entity knows which columns its key record holds.
 */
template <>
struct entity_event_traits<ores::refdata::messaging::book_purpose_type_event> {
    static constexpr std::string_view subject_prefix = "refdata.v1.book_purpose_types_events";

    static ores::refdata::messaging::book_purpose_type_event
    from_notification(const entity_event_notification& notification) {
        ores::refdata::messaging::book_purpose_type_event event;
        event.event_id = boost::lexical_cast<boost::uuids::uuid>(notification.event_id);
        if (const auto key =
                rfl::json::read<ores::refdata::messaging::book_purpose_type_key>(notification.key))
            event.key = *key;
        event.action = notification.action;
        event.version = notification.version;
        event.occurred_at = notification.occurred_at;
        event.correlation_id = notification.correlation_id;
        return event;
    }
};

}

#endif

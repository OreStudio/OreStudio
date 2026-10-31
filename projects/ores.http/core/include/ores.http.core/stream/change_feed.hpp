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
#ifndef ORES_HTTP_CORE_STREAM_CHANGE_FEED_HPP
#define ORES_HTTP_CORE_STREAM_CHANGE_FEED_HPP

#include "ores.http.core/export.hpp"
#include "ores.http.core/stream/change_coalescer.hpp"
#include "ores.nats/domain/message.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::stream {

/**
 * @brief What a store's event subject says about the change behind it.
 *
 * The collection drops the @c _events suffix the subject carries, so it is the same
 * name the route path and the model use.
 */
struct entity_event_address final {
    std::string component;
    std::string collection;
    std::string action;
};

/**
 * @brief Reads a component, a collection and an action out of an event subject.
 *
 * A subject is the component, the schema version, the events collection and the
 * action: @c iam.v1.tenant_types_events.created. It is read from the right, because
 * the delivered subject carries the deployment's prefix in front of it and the
 * prefix's length is the deployment's business.
 *
 * @return nothing when the subject is not an entity event, so a caller can drop a
 *         message rather than record a change named after something else.
 */
ORES_HTTP_CORE_EXPORT std::optional<entity_event_address>
parse_entity_event_subject(std::string_view subject);

/**
 * @brief The subject that hears every entity event of every component.
 *
 * One subscription for the whole process: a component that a later model adds is
 * heard without this string changing. It is relative, so the transport prefixes it.
 *
 * The pattern cannot name the events collections. A NATS wildcard is a whole token,
 * so =*_events= is a literal token that matches nothing, and the first version of
 * this constant stated exactly that and heard nothing at all. The collection is
 * therefore filtered in the parse rather than in the pattern, and every versioned
 * subject of every component arrives here. The cost is one more copy of each
 * request, and it goes away when the events collection becomes its own token.
 */
inline constexpr std::string_view all_entity_events = "*.v1.>";

/**
 * @brief Turns what the stores announce into changes for the coalescer.
 *
 * A store publishes one event per changed row on a subject that names the collection
 * and the action, with the tenancy in the headers. This reads the three of them and
 * records the coarsest change that is true of the event: this collection moved for
 * this tenant.
 *
 * A message with no tenant is dropped: no reader could be shown it, and a frame
 * addressed at nobody would reach every subscriber and help none of them.
 */
class ORES_HTTP_CORE_EXPORT change_feed final {
public:
    explicit change_feed(change_coalescer& coalescer)
        : coalescer_(coalescer) {}

    /// Records the change behind one message. @return whether it recorded one.
    bool notify(const ores::nats::message& message);

private:
    change_coalescer& coalescer_;
};

}

#endif

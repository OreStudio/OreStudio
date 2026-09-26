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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_MESSAGING_LIFECYCLE_EVENT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_LIFECYCLE_EVENT_PROTOCOL_HPP

#include "ores.trading.api/domain/lifecycle_event.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct lifecycle_event_key {
    std::string code;
};

struct lifecycle_event_write {
    std::string code;
    std::string description;
    std::optional<boost::uuids::uuid> fsm_state_id;
};

struct lifecycle_event_change {
    lifecycle_event_write write;
    ores::utility::domain::precondition precondition;
};

struct lifecycle_event_removal {
    lifecycle_event_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct lifecycle_event_lookup {
    lifecycle_event_key key;
    std::optional<ores::trading::domain::lifecycle_event> lifecycle_event;
};

struct lifecycle_event_event {
    boost::uuids::uuid event_id;
    lifecycle_event_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct lifecycle_event_version_key {
    lifecycle_event_key lifecycle_event;
    std::uint32_t version;
};

struct lifecycle_event_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_lifecycle_events_request {
    using response_type = struct list_lifecycle_events_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
};

struct list_lifecycle_events_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::lifecycle_event> events;
    std::uint64_t total;
};

struct get_lifecycle_event_request {
    using response_type = struct get_lifecycle_event_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    lifecycle_event_key key;
};

struct get_lifecycle_event_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::lifecycle_event> lifecycle_event;
};

struct get_many_lifecycle_events_request {
    using response_type = struct get_many_lifecycle_events_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<lifecycle_event_key> keys;
};

struct get_many_lifecycle_events_response {
    ores::utility::domain::result result;
    std::vector<lifecycle_event_lookup> entries;
};

struct put_lifecycle_event_request {
    using response_type = struct put_lifecycle_event_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    lifecycle_event_change change;
    ores::utility::domain::change_intent intent;
};

struct put_lifecycle_event_response {
    ores::utility::domain::result result;
    ores::trading::domain::lifecycle_event lifecycle_event;
};

struct put_many_lifecycle_events_request {
    using response_type = struct put_many_lifecycle_events_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<lifecycle_event_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_lifecycle_events_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::lifecycle_event> events;
};

struct delete_lifecycle_event_request {
    using response_type = struct delete_lifecycle_event_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    lifecycle_event_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_lifecycle_event_response {
    ores::utility::domain::result result;
};

struct delete_many_lifecycle_events_request {
    using response_type = struct delete_many_lifecycle_events_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<lifecycle_event_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_lifecycle_events_response {
    ores::utility::domain::result result;
};

struct list_lifecycle_event_versions_request {
    using response_type = struct list_lifecycle_event_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    lifecycle_event_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<lifecycle_event_versions_filter> filter;
};

struct list_lifecycle_event_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::lifecycle_event> versions;
    std::uint64_t total;
};

struct get_lifecycle_event_version_request {
    using response_type = struct get_lifecycle_event_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    lifecycle_event_version_key key;
};

struct get_lifecycle_event_version_response {
    ores::utility::domain::result result;
    ores::trading::domain::lifecycle_event version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace lifecycle_event_event_subjects {
inline constexpr std::string_view created = "trading.v1.lifecycle_events_events.created";
inline constexpr std::string_view updated = "trading.v1.lifecycle_events_events.updated";
inline constexpr std::string_view deleted = "trading.v1.lifecycle_events_events.deleted";
}

}

#endif

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
#ifndef ORES_TRADING_API_MESSAGING_ENTRY_CHANNEL_TYPE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_ENTRY_CHANNEL_TYPE_PROTOCOL_HPP

#include "ores.trading.api/domain/entry_channel_type.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct entry_channel_type_key {
    std::string code;
};

struct entry_channel_type_lookup {
    entry_channel_type_key key;
    std::optional<ores::trading::domain::entry_channel_type> entry_channel_type;
};

struct entry_channel_types_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct entry_channel_type_event {
    boost::uuids::uuid event_id;
    entry_channel_type_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_entry_channel_types_request {
    using response_type = struct list_entry_channel_types_response;
    static constexpr std::string_view nats_subject = "trading.v1.entry_channel_types.list";
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
    std::optional<entry_channel_types_filter> filter;
};

struct list_entry_channel_types_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::entry_channel_type> entry_channel_types;
    std::uint64_t total;
};

struct get_entry_channel_type_request {
    using response_type = struct get_entry_channel_type_response;
    static constexpr std::string_view nats_subject = "trading.v1.entry_channel_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    entry_channel_type_key key;
};

struct get_entry_channel_type_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::entry_channel_type> entry_channel_type;
};

struct get_many_entry_channel_types_request {
    using response_type = struct get_many_entry_channel_types_response;
    static constexpr std::string_view nats_subject = "trading.v1.entry_channel_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<entry_channel_type_key> keys;
};

struct get_many_entry_channel_types_response {
    ores::utility::domain::result result;
    std::vector<entry_channel_type_lookup> entries;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace entry_channel_type_event_subjects {
inline constexpr std::string_view created = "trading.v1.entry_channel_types_events.created";
inline constexpr std::string_view updated = "trading.v1.entry_channel_types_events.updated";
inline constexpr std::string_view deleted = "trading.v1.entry_channel_types_events.deleted";
}

}

#endif

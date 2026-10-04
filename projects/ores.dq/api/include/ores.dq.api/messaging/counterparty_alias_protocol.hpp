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
#ifndef ORES_DQ_API_MESSAGING_COUNTERPARTY_ALIAS_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_COUNTERPARTY_ALIAS_PROTOCOL_HPP

#include "ores.dq.api/domain/counterparty_alias.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct counterparty_alias_key {
    std::string id_value;
};

struct counterparty_alias_lookup {
    counterparty_alias_key key;
    std::optional<ores::dq::domain::counterparty_alias> counterparty_alias;
};

struct counterparty_aliases_filter {
    std::optional<std::vector<std::string>> id_value_one_of;
};

struct counterparty_alias_event {
    boost::uuids::uuid event_id;
    counterparty_alias_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_counterparty_aliases_request {
    using response_type = struct list_counterparty_aliases_response;
    static constexpr std::string_view nats_subject = "dq.v1.counterparty_aliases.list";
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
    std::optional<counterparty_aliases_filter> filter;
};

struct list_counterparty_aliases_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::counterparty_alias> counterparty_aliases;
    std::uint64_t total;
};

struct get_counterparty_alias_request {
    using response_type = struct get_counterparty_alias_response;
    static constexpr std::string_view nats_subject = "dq.v1.counterparty_aliases.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_alias_key key;
};

struct get_counterparty_alias_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::counterparty_alias> counterparty_alias;
};

struct get_many_counterparty_aliases_request {
    using response_type = struct get_many_counterparty_aliases_response;
    static constexpr std::string_view nats_subject = "dq.v1.counterparty_aliases.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_alias_key> keys;
};

struct get_many_counterparty_aliases_response {
    ores::utility::domain::result result;
    std::vector<counterparty_alias_lookup> entries;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace counterparty_alias_event_subjects {
inline constexpr std::string_view created = "dq.v1.counterparty_aliases_events.created";
inline constexpr std::string_view updated = "dq.v1.counterparty_aliases_events.updated";
inline constexpr std::string_view deleted = "dq.v1.counterparty_aliases_events.deleted";
}

}

#endif

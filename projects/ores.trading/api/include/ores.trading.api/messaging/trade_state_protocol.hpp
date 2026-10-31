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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_STATE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_STATE_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_state.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::trading::messaging {

struct trade_state_key {
    boost::uuids::uuid trade_id;
};

struct trade_state_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    boost::uuids::uuid status_id;
};

struct trade_state_change {
    trade_state_write write;
    ores::utility::domain::precondition precondition;
};

struct trade_state_removal {
    trade_state_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct trade_state_lookup {
    trade_state_key key;
    std::optional<ores::trading::domain::trade_state> trade_state;
};

struct trade_states_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct trade_state_event {
    boost::uuids::uuid event_id;
    trade_state_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct trade_state_version_key {
    trade_state_key trade_state;
    std::uint32_t version;
};

struct trade_state_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_trade_states_request {
    using response_type = struct list_trade_states_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states.list";
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
    std::optional<trade_states_filter> filter;
    std::optional<std::string> as_of;
};

struct list_trade_states_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_state> trade_states;
    std::uint64_t total;
};

struct get_trade_state_request {
    using response_type = struct get_trade_state_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_state_key key;
};

struct get_trade_state_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_state> trade_state;
};

struct get_many_trade_states_request {
    using response_type = struct get_many_trade_states_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_state_key> keys;
};

struct get_many_trade_states_response {
    ores::utility::domain::result result;
    std::vector<trade_state_lookup> entries;
};

struct put_trade_state_request {
    using response_type = struct put_trade_state_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_state_change change;
    ores::utility::domain::change_intent intent;
};

struct put_trade_state_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_state> trade_state;
};

struct put_many_trade_states_request {
    using response_type = struct put_many_trade_states_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_state_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_trade_states_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_state> trade_states;
};

struct delete_trade_state_request {
    using response_type = struct delete_trade_state_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_state_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_trade_state_response {
    ores::utility::domain::result result;
};

struct delete_many_trade_states_request {
    using response_type = struct delete_many_trade_states_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_state_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_trade_states_response {
    ores::utility::domain::result result;
};

struct list_trade_state_versions_request {
    using response_type = struct list_trade_state_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_state_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_state_versions_filter> filter;
};

struct list_trade_state_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_state> versions;
    std::uint64_t total;
};

struct get_trade_state_version_request {
    using response_type = struct get_trade_state_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_states_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_state_version_key key;
};

struct get_trade_state_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_state> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_state_event_subjects {
inline constexpr std::string_view created = "trading.v1.trade_states_events.created";
inline constexpr std::string_view updated = "trading.v1.trade_states_events.updated";
inline constexpr std::string_view deleted = "trading.v1.trade_states_events.deleted";
}
/**
 * @brief The members of trade_state_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_state_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_state_key::trade_id},
    };
}

/**
 * @brief The members of trade_state_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_state_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_state_write::trade_id},
        utility::codec::field{"trade_activity_id", &trade_state_write::trade_activity_id},
        utility::codec::field{"status_id", &trade_state_write::status_id},
    };
}

/**
 * @brief The members of trade_state_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_state_change&) {
    return std::tuple{
        utility::codec::field{"write", &trade_state_change::write},
        utility::codec::field{"precondition", &trade_state_change::precondition},
    };
}

/**
 * @brief The members of trade_state_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_state_removal&) {
    return std::tuple{
        utility::codec::field{"key", &trade_state_removal::key},
        utility::codec::field{"precondition", &trade_state_removal::precondition},
    };
}

/**
 * @brief The members of trade_state_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_state_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &trade_state_lookup::key},
        utility::codec::field{"trade_state", &trade_state_lookup::trade_state},
    };
}

/**
 * @brief The members of trade_states_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_states_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id_one_of", &trade_states_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of trade_state_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_state_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &trade_state_event::event_id},
        utility::codec::field{"key", &trade_state_event::key},
        utility::codec::field{"action", &trade_state_event::action},
        utility::codec::field{"version", &trade_state_event::version},
        utility::codec::field{"occurred_at", &trade_state_event::occurred_at},
        utility::codec::field{"correlation_id", &trade_state_event::correlation_id},
    };
}

/**
 * @brief The members of trade_state_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_state_version_key&) {
    return std::tuple{
        utility::codec::field{"trade_state", &trade_state_version_key::trade_state},
        utility::codec::field{"version", &trade_state_version_key::version},
    };
}

/**
 * @brief The members of trade_state_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_state_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &trade_state_versions_filter::version},
        utility::codec::field{"from_version", &trade_state_versions_filter::from_version},
        utility::codec::field{"to_version", &trade_state_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_trade_states_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_states_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_trade_states_request::offset},
        utility::codec::field{"limit", &list_trade_states_request::limit},
        utility::codec::field{"order", &list_trade_states_request::order},
        utility::codec::field{"filter", &list_trade_states_request::filter},
        utility::codec::field{"as_of", &list_trade_states_request::as_of},
    };
}

/**
 * @brief The members of list_trade_states_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_states_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_states_response::result},
        utility::codec::field{"trade_states", &list_trade_states_response::trade_states},
        utility::codec::field{"total", &list_trade_states_response::total},
    };
}

/**
 * @brief The members of get_trade_state_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_state_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_state_request::key},
    };
}

/**
 * @brief The members of get_trade_state_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_state_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_state_response::result},
        utility::codec::field{"trade_state", &get_trade_state_response::trade_state},
    };
}

/**
 * @brief The members of get_many_trade_states_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_states_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_trade_states_request::keys},
    };
}

/**
 * @brief The members of get_many_trade_states_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_states_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_trade_states_response::result},
        utility::codec::field{"entries", &get_many_trade_states_response::entries},
    };
}

/**
 * @brief The members of put_trade_state_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_state_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_trade_state_request::change},
        utility::codec::field{"intent", &put_trade_state_request::intent},
    };
}

/**
 * @brief The members of put_trade_state_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_state_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_trade_state_response::result},
        utility::codec::field{"trade_state", &put_trade_state_response::trade_state},
    };
}

/**
 * @brief The members of put_many_trade_states_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_states_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_trade_states_request::changes},
        utility::codec::field{"intent", &put_many_trade_states_request::intent},
    };
}

/**
 * @brief The members of put_many_trade_states_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_states_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_trade_states_response::result},
        utility::codec::field{"trade_states", &put_many_trade_states_response::trade_states},
    };
}

/**
 * @brief The members of delete_trade_state_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_state_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_trade_state_request::removal},
        utility::codec::field{"intent", &delete_trade_state_request::intent},
    };
}

/**
 * @brief The members of delete_trade_state_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_state_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_trade_state_response::result},
    };
}

/**
 * @brief The members of delete_many_trade_states_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_states_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_trade_states_request::removals},
        utility::codec::field{"intent", &delete_many_trade_states_request::intent},
    };
}

/**
 * @brief The members of delete_many_trade_states_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_states_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_trade_states_response::result},
    };
}

/**
 * @brief The members of list_trade_state_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_state_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_trade_state_versions_request::key},
        utility::codec::field{"offset", &list_trade_state_versions_request::offset},
        utility::codec::field{"limit", &list_trade_state_versions_request::limit},
        utility::codec::field{"order", &list_trade_state_versions_request::order},
        utility::codec::field{"filter", &list_trade_state_versions_request::filter},
    };
}

/**
 * @brief The members of list_trade_state_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_state_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_state_versions_response::result},
        utility::codec::field{"versions", &list_trade_state_versions_response::versions},
        utility::codec::field{"total", &list_trade_state_versions_response::total},
    };
}

/**
 * @brief The members of get_trade_state_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_state_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_state_version_request::key},
    };
}

/**
 * @brief The members of get_trade_state_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_state_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_state_version_response::result},
        utility::codec::field{"version", &get_trade_state_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_state_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_state_key& v);
    static ores::trading::messaging::trade_state_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_state_key& v);
    static ores::trading::messaging::trade_state_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_state_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_state_write& v);
    static ores::trading::messaging::trade_state_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_state_write& v);
    static ores::trading::messaging::trade_state_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_state_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_state_change& v);
    static ores::trading::messaging::trade_state_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_state_change& v);
    static ores::trading::messaging::trade_state_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_state_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_state_removal& v);
    static ores::trading::messaging::trade_state_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_state_removal& v);
    static ores::trading::messaging::trade_state_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_state_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_state_lookup& v);
    static ores::trading::messaging::trade_state_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_state_lookup& v);
    static ores::trading::messaging::trade_state_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_states_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_states_filter& v);
    static ores::trading::messaging::trade_states_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_states_filter& v);
    static ores::trading::messaging::trade_states_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_state_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_state_event& v);
    static ores::trading::messaging::trade_state_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_state_event& v);
    static ores::trading::messaging::trade_state_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_state_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_state_version_key& v);
    static ores::trading::messaging::trade_state_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_state_version_key& v);
    static ores::trading::messaging::trade_state_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_state_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_state_versions_filter& v);
    static ores::trading::messaging::trade_state_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_state_versions_filter& v);
    static ores::trading::messaging::trade_state_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_trade_states_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_trade_states_request& v);
    static ores::trading::messaging::list_trade_states_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_trade_states_request& v);
    static ores::trading::messaging::list_trade_states_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_trade_states_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_trade_states_response& v);
    static ores::trading::messaging::list_trade_states_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_trade_states_response& v);
    static ores::trading::messaging::list_trade_states_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_state_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_state_request& v);
    static ores::trading::messaging::get_trade_state_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_trade_state_request& v);
    static ores::trading::messaging::get_trade_state_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_state_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_state_response& v);
    static ores::trading::messaging::get_trade_state_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_trade_state_response& v);
    static ores::trading::messaging::get_trade_state_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_states_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_trade_states_request& v);
    static ores::trading::messaging::get_many_trade_states_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_states_request& v);
    static ores::trading::messaging::get_many_trade_states_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_states_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_states_response& v);
    static ores::trading::messaging::get_many_trade_states_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_states_response& v);
    static ores::trading::messaging::get_many_trade_states_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_trade_state_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_trade_state_request& v);
    static ores::trading::messaging::put_trade_state_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_trade_state_request& v);
    static ores::trading::messaging::put_trade_state_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_trade_state_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_trade_state_response& v);
    static ores::trading::messaging::put_trade_state_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_trade_state_response& v);
    static ores::trading::messaging::put_trade_state_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_states_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_trade_states_request& v);
    static ores::trading::messaging::put_many_trade_states_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_states_request& v);
    static ores::trading::messaging::put_many_trade_states_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_states_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_states_response& v);
    static ores::trading::messaging::put_many_trade_states_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_states_response& v);
    static ores::trading::messaging::put_many_trade_states_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_trade_state_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_trade_state_request& v);
    static ores::trading::messaging::delete_trade_state_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_trade_state_request& v);
    static ores::trading::messaging::delete_trade_state_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_trade_state_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_trade_state_response& v);
    static ores::trading::messaging::delete_trade_state_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_state_response& v);
    static ores::trading::messaging::delete_trade_state_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_states_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_states_request& v);
    static ores::trading::messaging::delete_many_trade_states_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_states_request& v);
    static ores::trading::messaging::delete_many_trade_states_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_states_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_states_response& v);
    static ores::trading::messaging::delete_many_trade_states_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_states_response& v);
    static ores::trading::messaging::delete_many_trade_states_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_state_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_state_versions_request& v);
    static ores::trading::messaging::list_trade_state_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_state_versions_request& v);
    static ores::trading::messaging::list_trade_state_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_state_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_state_versions_response& v);
    static ores::trading::messaging::list_trade_state_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_state_versions_response& v);
    static ores::trading::messaging::list_trade_state_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_state_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_state_version_request& v);
    static ores::trading::messaging::get_trade_state_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_state_version_request& v);
    static ores::trading::messaging::get_trade_state_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_state_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_state_version_response& v);
    static ores::trading::messaging::get_trade_state_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_state_version_response& v);
    static ores::trading::messaging::get_trade_state_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

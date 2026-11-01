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
#ifndef ORES_TRADING_API_MESSAGING_ASCOT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_ASCOT_PROTOCOL_HPP

#include "ores.trading.api/domain/ascot.hpp"
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

struct ascot_key {
    boost::uuids::uuid trade_id;
};

struct ascot_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    std::string ascot_option_type;
};

struct ascot_change {
    ascot_write write;
    ores::utility::domain::precondition precondition;
};

struct ascot_removal {
    ascot_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct ascot_lookup {
    ascot_key key;
    std::optional<ores::trading::domain::ascot> ascot;
};

struct ascots_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct ascot_event {
    boost::uuids::uuid event_id;
    ascot_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct ascot_version_key {
    ascot_key ascot;
    std::uint32_t version;
};

struct ascot_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_ascots_request {
    using response_type = struct list_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.list";
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
    std::optional<ascots_filter> filter;
    std::optional<std::string> as_of;
};

struct list_ascots_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::ascot> ascots;
    std::uint64_t total;
};

struct get_ascot_request {
    using response_type = struct get_ascot_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_key key;
};

struct get_ascot_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::ascot> ascot;
};

struct get_many_ascots_request {
    using response_type = struct get_many_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ascot_key> keys;
};

struct get_many_ascots_response {
    ores::utility::domain::result result;
    std::vector<ascot_lookup> entries;
};

struct put_ascot_request {
    using response_type = struct put_ascot_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_change change;
    ores::utility::domain::change_intent intent;
};

struct put_ascot_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::ascot> ascot;
};

struct put_many_ascots_request {
    using response_type = struct put_many_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ascot_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_ascots_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::ascot> ascots;
};

struct delete_ascot_request {
    using response_type = struct delete_ascot_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_ascot_response {
    ores::utility::domain::result result;
};

struct delete_many_ascots_request {
    using response_type = struct delete_many_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ascot_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_ascots_response {
    ores::utility::domain::result result;
};

struct list_ascot_versions_request {
    using response_type = struct list_ascot_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ascot_versions_filter> filter;
};

struct list_ascot_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::ascot> versions;
    std::uint64_t total;
};

struct get_ascot_version_request {
    using response_type = struct get_ascot_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_version_key key;
};

struct get_ascot_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::ascot> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace ascot_event_subjects {
inline constexpr std::string_view created = "trading.v1.ascots_events.created";
inline constexpr std::string_view updated = "trading.v1.ascots_events.updated";
inline constexpr std::string_view deleted = "trading.v1.ascots_events.deleted";
}
/**
 * @brief The members of ascot_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascot_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &ascot_key::trade_id},
    };
}

/**
 * @brief The members of ascot_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascot_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &ascot_write::trade_id},
        utility::codec::field{"trade_activity_id", &ascot_write::trade_activity_id},
        utility::codec::field{"ascot_option_type", &ascot_write::ascot_option_type},
    };
}

/**
 * @brief The members of ascot_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascot_change&) {
    return std::tuple{
        utility::codec::field{"write", &ascot_change::write},
        utility::codec::field{"precondition", &ascot_change::precondition},
    };
}

/**
 * @brief The members of ascot_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascot_removal&) {
    return std::tuple{
        utility::codec::field{"key", &ascot_removal::key},
        utility::codec::field{"precondition", &ascot_removal::precondition},
    };
}

/**
 * @brief The members of ascot_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascot_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &ascot_lookup::key},
        utility::codec::field{"ascot", &ascot_lookup::ascot},
    };
}

/**
 * @brief The members of ascots_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascots_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id_one_of", &ascots_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of ascot_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascot_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &ascot_event::event_id},
        utility::codec::field{"key", &ascot_event::key},
        utility::codec::field{"action", &ascot_event::action},
        utility::codec::field{"version", &ascot_event::version},
        utility::codec::field{"occurred_at", &ascot_event::occurred_at},
        utility::codec::field{"correlation_id", &ascot_event::correlation_id},
    };
}

/**
 * @brief The members of ascot_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascot_version_key&) {
    return std::tuple{
        utility::codec::field{"ascot", &ascot_version_key::ascot},
        utility::codec::field{"version", &ascot_version_key::version},
    };
}

/**
 * @brief The members of ascot_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ascot_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &ascot_versions_filter::version},
        utility::codec::field{"from_version", &ascot_versions_filter::from_version},
        utility::codec::field{"to_version", &ascot_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_ascots_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ascots_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_ascots_request::offset},
        utility::codec::field{"limit", &list_ascots_request::limit},
        utility::codec::field{"order", &list_ascots_request::order},
        utility::codec::field{"filter", &list_ascots_request::filter},
        utility::codec::field{"as_of", &list_ascots_request::as_of},
    };
}

/**
 * @brief The members of list_ascots_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ascots_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_ascots_response::result},
        utility::codec::field{"ascots", &list_ascots_response::ascots},
        utility::codec::field{"total", &list_ascots_response::total},
    };
}

/**
 * @brief The members of get_ascot_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ascot_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_ascot_request::key},
    };
}

/**
 * @brief The members of get_ascot_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ascot_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_ascot_response::result},
        utility::codec::field{"ascot", &get_ascot_response::ascot},
    };
}

/**
 * @brief The members of get_many_ascots_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_ascots_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_ascots_request::keys},
    };
}

/**
 * @brief The members of get_many_ascots_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_ascots_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_ascots_response::result},
        utility::codec::field{"entries", &get_many_ascots_response::entries},
    };
}

/**
 * @brief The members of put_ascot_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_ascot_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_ascot_request::change},
        utility::codec::field{"intent", &put_ascot_request::intent},
    };
}

/**
 * @brief The members of put_ascot_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_ascot_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_ascot_response::result},
        utility::codec::field{"ascot", &put_ascot_response::ascot},
    };
}

/**
 * @brief The members of put_many_ascots_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_ascots_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_ascots_request::changes},
        utility::codec::field{"intent", &put_many_ascots_request::intent},
    };
}

/**
 * @brief The members of put_many_ascots_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_ascots_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_ascots_response::result},
        utility::codec::field{"ascots", &put_many_ascots_response::ascots},
    };
}

/**
 * @brief The members of delete_ascot_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_ascot_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_ascot_request::removal},
        utility::codec::field{"intent", &delete_ascot_request::intent},
    };
}

/**
 * @brief The members of delete_ascot_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_ascot_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_ascot_response::result},
    };
}

/**
 * @brief The members of delete_many_ascots_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_ascots_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_ascots_request::removals},
        utility::codec::field{"intent", &delete_many_ascots_request::intent},
    };
}

/**
 * @brief The members of delete_many_ascots_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_ascots_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_ascots_response::result},
    };
}

/**
 * @brief The members of list_ascot_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ascot_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_ascot_versions_request::key},
        utility::codec::field{"offset", &list_ascot_versions_request::offset},
        utility::codec::field{"limit", &list_ascot_versions_request::limit},
        utility::codec::field{"order", &list_ascot_versions_request::order},
        utility::codec::field{"filter", &list_ascot_versions_request::filter},
    };
}

/**
 * @brief The members of list_ascot_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ascot_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_ascot_versions_response::result},
        utility::codec::field{"versions", &list_ascot_versions_response::versions},
        utility::codec::field{"total", &list_ascot_versions_response::total},
    };
}

/**
 * @brief The members of get_ascot_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ascot_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_ascot_version_request::key},
    };
}

/**
 * @brief The members of get_ascot_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ascot_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_ascot_version_response::result},
        utility::codec::field{"version", &get_ascot_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascot_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascot_key& v);
    static ores::trading::messaging::ascot_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascot_key& v);
    static ores::trading::messaging::ascot_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascot_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascot_write& v);
    static ores::trading::messaging::ascot_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascot_write& v);
    static ores::trading::messaging::ascot_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascot_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascot_change& v);
    static ores::trading::messaging::ascot_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascot_change& v);
    static ores::trading::messaging::ascot_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascot_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascot_removal& v);
    static ores::trading::messaging::ascot_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascot_removal& v);
    static ores::trading::messaging::ascot_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascot_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascot_lookup& v);
    static ores::trading::messaging::ascot_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascot_lookup& v);
    static ores::trading::messaging::ascot_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascots_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascots_filter& v);
    static ores::trading::messaging::ascots_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascots_filter& v);
    static ores::trading::messaging::ascots_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascot_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascot_event& v);
    static ores::trading::messaging::ascot_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascot_event& v);
    static ores::trading::messaging::ascot_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascot_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascot_version_key& v);
    static ores::trading::messaging::ascot_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascot_version_key& v);
    static ores::trading::messaging::ascot_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::ascot_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::ascot_versions_filter& v);
    static ores::trading::messaging::ascot_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::ascot_versions_filter& v);
    static ores::trading::messaging::ascot_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_ascots_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_ascots_request& v);
    static ores::trading::messaging::list_ascots_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_ascots_request& v);
    static ores::trading::messaging::list_ascots_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_ascots_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_ascots_response& v);
    static ores::trading::messaging::list_ascots_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_ascots_response& v);
    static ores::trading::messaging::list_ascots_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_ascot_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_ascot_request& v);
    static ores::trading::messaging::get_ascot_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_ascot_request& v);
    static ores::trading::messaging::get_ascot_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_ascot_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_ascot_response& v);
    static ores::trading::messaging::get_ascot_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_ascot_response& v);
    static ores::trading::messaging::get_ascot_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_ascots_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_ascots_request& v);
    static ores::trading::messaging::get_many_ascots_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_many_ascots_request& v);
    static ores::trading::messaging::get_many_ascots_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_ascots_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_ascots_response& v);
    static ores::trading::messaging::get_many_ascots_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_many_ascots_response& v);
    static ores::trading::messaging::get_many_ascots_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_ascot_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_ascot_request& v);
    static ores::trading::messaging::put_ascot_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_ascot_request& v);
    static ores::trading::messaging::put_ascot_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_ascot_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_ascot_response& v);
    static ores::trading::messaging::put_ascot_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_ascot_response& v);
    static ores::trading::messaging::put_ascot_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_ascots_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_ascots_request& v);
    static ores::trading::messaging::put_many_ascots_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_many_ascots_request& v);
    static ores::trading::messaging::put_many_ascots_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_ascots_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_ascots_response& v);
    static ores::trading::messaging::put_many_ascots_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_many_ascots_response& v);
    static ores::trading::messaging::put_many_ascots_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_ascot_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_ascot_request& v);
    static ores::trading::messaging::delete_ascot_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_ascot_request& v);
    static ores::trading::messaging::delete_ascot_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_ascot_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_ascot_response& v);
    static ores::trading::messaging::delete_ascot_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_ascot_response& v);
    static ores::trading::messaging::delete_ascot_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_many_ascots_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_many_ascots_request& v);
    static ores::trading::messaging::delete_many_ascots_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_many_ascots_request& v);
    static ores::trading::messaging::delete_many_ascots_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_many_ascots_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_many_ascots_response& v);
    static ores::trading::messaging::delete_many_ascots_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_ascots_response& v);
    static ores::trading::messaging::delete_many_ascots_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_ascot_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_ascot_versions_request& v);
    static ores::trading::messaging::list_ascot_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_ascot_versions_request& v);
    static ores::trading::messaging::list_ascot_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_ascot_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_ascot_versions_response& v);
    static ores::trading::messaging::list_ascot_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_ascot_versions_response& v);
    static ores::trading::messaging::list_ascot_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_ascot_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_ascot_version_request& v);
    static ores::trading::messaging::get_ascot_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_ascot_version_request& v);
    static ores::trading::messaging::get_ascot_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_ascot_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_ascot_version_response& v);
    static ores::trading::messaging::get_ascot_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_ascot_version_response& v);
    static ores::trading::messaging::get_ascot_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

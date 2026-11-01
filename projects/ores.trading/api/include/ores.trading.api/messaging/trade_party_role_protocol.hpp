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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_PARTY_ROLE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_PARTY_ROLE_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_party_role.hpp"
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

struct trade_party_role_key {
    boost::uuids::uuid trade_id;
    std::string role;
};

struct trade_party_role_write {
    boost::uuids::uuid trade_id;
    std::string role;
    boost::uuids::uuid trade_activity_id;
};

struct trade_party_role_change {
    trade_party_role_write write;
    ores::utility::domain::precondition precondition;
};

struct trade_party_role_removal {
    trade_party_role_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct trade_party_role_lookup {
    trade_party_role_key key;
    std::optional<ores::trading::domain::trade_party_role> trade_party_role;
};

struct trade_party_roles_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct trade_party_role_event {
    boost::uuids::uuid event_id;
    trade_party_role_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct trade_party_role_version_key {
    trade_party_role_key trade_party_role;
    std::uint32_t version;
};

struct trade_party_role_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_trade_party_roles_request {
    using response_type = struct list_trade_party_roles_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.list";
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
    std::optional<trade_party_roles_filter> filter;
    std::optional<std::string> as_of;
};

struct list_trade_party_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_party_role> roles;
    std::uint64_t total;
};

struct get_trade_party_role_request {
    using response_type = struct get_trade_party_role_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_party_role_key key;
};

struct get_trade_party_role_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_party_role> trade_party_role;
};

struct get_many_trade_party_roles_request {
    using response_type = struct get_many_trade_party_roles_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_party_role_key> keys;
};

struct get_many_trade_party_roles_response {
    ores::utility::domain::result result;
    std::vector<trade_party_role_lookup> entries;
};

struct put_trade_party_role_request {
    using response_type = struct put_trade_party_role_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_party_role_change change;
    ores::utility::domain::change_intent intent;
};

struct put_trade_party_role_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_party_role> trade_party_role;
};

struct put_many_trade_party_roles_request {
    using response_type = struct put_many_trade_party_roles_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_party_role_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_trade_party_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_party_role> roles;
};

struct delete_trade_party_role_request {
    using response_type = struct delete_trade_party_role_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_party_role_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_trade_party_role_response {
    ores::utility::domain::result result;
};

struct delete_many_trade_party_roles_request {
    using response_type = struct delete_many_trade_party_roles_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_party_role_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_trade_party_roles_response {
    ores::utility::domain::result result;
};

struct list_by_trade_id_trade_party_roles_request {
    using response_type = struct list_by_trade_id_trade_party_roles_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_party_roles.list_by_trade_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid trade_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_party_roles_filter> filter;
};

struct list_by_trade_id_trade_party_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_party_role> roles;
    std::uint64_t total;
};

struct list_trade_party_role_versions_request {
    using response_type = struct list_trade_party_role_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_party_role_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_party_role_versions_filter> filter;
};

struct list_trade_party_role_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_party_role> versions;
    std::uint64_t total;
};

struct get_trade_party_role_version_request {
    using response_type = struct get_trade_party_role_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_party_role_version_key key;
};

struct get_trade_party_role_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_party_role> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_party_role_event_subjects {
inline constexpr std::string_view created = "trading.v1.trade_party_roles_events.created";
inline constexpr std::string_view updated = "trading.v1.trade_party_roles_events.updated";
inline constexpr std::string_view deleted = "trading.v1.trade_party_roles_events.deleted";
}
/**
 * @brief The members of trade_party_role_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_role_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_party_role_key::trade_id},
        utility::codec::field{"role", &trade_party_role_key::role},
    };
}

/**
 * @brief The members of trade_party_role_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_role_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_party_role_write::trade_id},
        utility::codec::field{"role", &trade_party_role_write::role},
        utility::codec::field{"trade_activity_id", &trade_party_role_write::trade_activity_id},
    };
}

/**
 * @brief The members of trade_party_role_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_role_change&) {
    return std::tuple{
        utility::codec::field{"write", &trade_party_role_change::write},
        utility::codec::field{"precondition", &trade_party_role_change::precondition},
    };
}

/**
 * @brief The members of trade_party_role_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_role_removal&) {
    return std::tuple{
        utility::codec::field{"key", &trade_party_role_removal::key},
        utility::codec::field{"precondition", &trade_party_role_removal::precondition},
    };
}

/**
 * @brief The members of trade_party_role_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_role_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &trade_party_role_lookup::key},
        utility::codec::field{"trade_party_role", &trade_party_role_lookup::trade_party_role},
    };
}

/**
 * @brief The members of trade_party_roles_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_roles_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_party_roles_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &trade_party_roles_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of trade_party_role_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_role_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &trade_party_role_event::event_id},
        utility::codec::field{"key", &trade_party_role_event::key},
        utility::codec::field{"action", &trade_party_role_event::action},
        utility::codec::field{"version", &trade_party_role_event::version},
        utility::codec::field{"occurred_at", &trade_party_role_event::occurred_at},
        utility::codec::field{"correlation_id", &trade_party_role_event::correlation_id},
    };
}

/**
 * @brief The members of trade_party_role_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_role_version_key&) {
    return std::tuple{
        utility::codec::field{"trade_party_role", &trade_party_role_version_key::trade_party_role},
        utility::codec::field{"version", &trade_party_role_version_key::version},
    };
}

/**
 * @brief The members of trade_party_role_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_party_role_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &trade_party_role_versions_filter::version},
        utility::codec::field{"from_version", &trade_party_role_versions_filter::from_version},
        utility::codec::field{"to_version", &trade_party_role_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_trade_party_roles_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_party_roles_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_trade_party_roles_request::offset},
        utility::codec::field{"limit", &list_trade_party_roles_request::limit},
        utility::codec::field{"order", &list_trade_party_roles_request::order},
        utility::codec::field{"filter", &list_trade_party_roles_request::filter},
        utility::codec::field{"as_of", &list_trade_party_roles_request::as_of},
    };
}

/**
 * @brief The members of list_trade_party_roles_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_party_roles_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_party_roles_response::result},
        utility::codec::field{"roles", &list_trade_party_roles_response::roles},
        utility::codec::field{"total", &list_trade_party_roles_response::total},
    };
}

/**
 * @brief The members of get_trade_party_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_party_role_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_party_role_request::key},
    };
}

/**
 * @brief The members of get_trade_party_role_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_party_role_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_party_role_response::result},
        utility::codec::field{"trade_party_role", &get_trade_party_role_response::trade_party_role},
    };
}

/**
 * @brief The members of get_many_trade_party_roles_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_party_roles_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_trade_party_roles_request::keys},
    };
}

/**
 * @brief The members of get_many_trade_party_roles_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_party_roles_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_trade_party_roles_response::result},
        utility::codec::field{"entries", &get_many_trade_party_roles_response::entries},
    };
}

/**
 * @brief The members of put_trade_party_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_party_role_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_trade_party_role_request::change},
        utility::codec::field{"intent", &put_trade_party_role_request::intent},
    };
}

/**
 * @brief The members of put_trade_party_role_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_party_role_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_trade_party_role_response::result},
        utility::codec::field{"trade_party_role", &put_trade_party_role_response::trade_party_role},
    };
}

/**
 * @brief The members of put_many_trade_party_roles_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_party_roles_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_trade_party_roles_request::changes},
        utility::codec::field{"intent", &put_many_trade_party_roles_request::intent},
    };
}

/**
 * @brief The members of put_many_trade_party_roles_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_party_roles_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_trade_party_roles_response::result},
        utility::codec::field{"roles", &put_many_trade_party_roles_response::roles},
    };
}

/**
 * @brief The members of delete_trade_party_role_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_party_role_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_trade_party_role_request::removal},
        utility::codec::field{"intent", &delete_trade_party_role_request::intent},
    };
}

/**
 * @brief The members of delete_trade_party_role_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_party_role_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_trade_party_role_response::result},
    };
}

/**
 * @brief The members of delete_many_trade_party_roles_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_party_roles_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_trade_party_roles_request::removals},
        utility::codec::field{"intent", &delete_many_trade_party_roles_request::intent},
    };
}

/**
 * @brief The members of delete_many_trade_party_roles_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_party_roles_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_trade_party_roles_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_trade_party_roles_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_trade_id_trade_party_roles_request&) {
    return std::tuple{
        utility::codec::field{"trade_id", &list_by_trade_id_trade_party_roles_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_trade_party_roles_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_trade_party_roles_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_trade_party_roles_request::limit},
        utility::codec::field{"order", &list_by_trade_id_trade_party_roles_request::order},
        utility::codec::field{"filter", &list_by_trade_id_trade_party_roles_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_trade_party_roles_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_trade_id_trade_party_roles_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_trade_id_trade_party_roles_response::result},
        utility::codec::field{"roles", &list_by_trade_id_trade_party_roles_response::roles},
        utility::codec::field{"total", &list_by_trade_id_trade_party_roles_response::total},
    };
}

/**
 * @brief The members of list_trade_party_role_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_party_role_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_trade_party_role_versions_request::key},
        utility::codec::field{"offset", &list_trade_party_role_versions_request::offset},
        utility::codec::field{"limit", &list_trade_party_role_versions_request::limit},
        utility::codec::field{"order", &list_trade_party_role_versions_request::order},
        utility::codec::field{"filter", &list_trade_party_role_versions_request::filter},
    };
}

/**
 * @brief The members of list_trade_party_role_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_party_role_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_party_role_versions_response::result},
        utility::codec::field{"versions", &list_trade_party_role_versions_response::versions},
        utility::codec::field{"total", &list_trade_party_role_versions_response::total},
    };
}

/**
 * @brief The members of get_trade_party_role_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_party_role_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_party_role_version_request::key},
    };
}

/**
 * @brief The members of get_trade_party_role_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_party_role_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_party_role_version_response::result},
        utility::codec::field{"version", &get_trade_party_role_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_party_role_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_party_role_key& v);
    static ores::trading::messaging::trade_party_role_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_party_role_key& v);
    static ores::trading::messaging::trade_party_role_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_party_role_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_party_role_write& v);
    static ores::trading::messaging::trade_party_role_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_party_role_write& v);
    static ores::trading::messaging::trade_party_role_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_party_role_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_party_role_change& v);
    static ores::trading::messaging::trade_party_role_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_party_role_change& v);
    static ores::trading::messaging::trade_party_role_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_party_role_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_party_role_removal& v);
    static ores::trading::messaging::trade_party_role_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_party_role_removal& v);
    static ores::trading::messaging::trade_party_role_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_party_role_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_party_role_lookup& v);
    static ores::trading::messaging::trade_party_role_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_party_role_lookup& v);
    static ores::trading::messaging::trade_party_role_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_party_roles_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_party_roles_filter& v);
    static ores::trading::messaging::trade_party_roles_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_party_roles_filter& v);
    static ores::trading::messaging::trade_party_roles_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_party_role_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_party_role_event& v);
    static ores::trading::messaging::trade_party_role_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_party_role_event& v);
    static ores::trading::messaging::trade_party_role_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_party_role_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_party_role_version_key& v);
    static ores::trading::messaging::trade_party_role_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_party_role_version_key& v);
    static ores::trading::messaging::trade_party_role_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_party_role_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_party_role_versions_filter& v);
    static ores::trading::messaging::trade_party_role_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_party_role_versions_filter& v);
    static ores::trading::messaging::trade_party_role_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_party_roles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_party_roles_request& v);
    static ores::trading::messaging::list_trade_party_roles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_party_roles_request& v);
    static ores::trading::messaging::list_trade_party_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_party_roles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_party_roles_response& v);
    static ores::trading::messaging::list_trade_party_roles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_party_roles_response& v);
    static ores::trading::messaging::list_trade_party_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_party_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_party_role_request& v);
    static ores::trading::messaging::get_trade_party_role_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_party_role_request& v);
    static ores::trading::messaging::get_trade_party_role_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_party_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_party_role_response& v);
    static ores::trading::messaging::get_trade_party_role_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_party_role_response& v);
    static ores::trading::messaging::get_trade_party_role_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_party_roles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_party_roles_request& v);
    static ores::trading::messaging::get_many_trade_party_roles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_party_roles_request& v);
    static ores::trading::messaging::get_many_trade_party_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_party_roles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_party_roles_response& v);
    static ores::trading::messaging::get_many_trade_party_roles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_party_roles_response& v);
    static ores::trading::messaging::get_many_trade_party_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_trade_party_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_trade_party_role_request& v);
    static ores::trading::messaging::put_trade_party_role_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_trade_party_role_request& v);
    static ores::trading::messaging::put_trade_party_role_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_trade_party_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_trade_party_role_response& v);
    static ores::trading::messaging::put_trade_party_role_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_trade_party_role_response& v);
    static ores::trading::messaging::put_trade_party_role_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_party_roles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_party_roles_request& v);
    static ores::trading::messaging::put_many_trade_party_roles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_party_roles_request& v);
    static ores::trading::messaging::put_many_trade_party_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_party_roles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_party_roles_response& v);
    static ores::trading::messaging::put_many_trade_party_roles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_party_roles_response& v);
    static ores::trading::messaging::put_many_trade_party_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_trade_party_role_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_trade_party_role_request& v);
    static ores::trading::messaging::delete_trade_party_role_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_party_role_request& v);
    static ores::trading::messaging::delete_trade_party_role_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_trade_party_role_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_trade_party_role_response& v);
    static ores::trading::messaging::delete_trade_party_role_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_party_role_response& v);
    static ores::trading::messaging::delete_trade_party_role_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_party_roles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_party_roles_request& v);
    static ores::trading::messaging::delete_many_trade_party_roles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_party_roles_request& v);
    static ores::trading::messaging::delete_many_trade_party_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_party_roles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_party_roles_response& v);
    static ores::trading::messaging::delete_many_trade_party_roles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_party_roles_response& v);
    static ores::trading::messaging::delete_many_trade_party_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_trade_party_roles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_trade_party_roles_request& v);
    static ores::trading::messaging::list_by_trade_id_trade_party_roles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_trade_party_roles_request& v);
    static ores::trading::messaging::list_by_trade_id_trade_party_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_trade_party_roles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_trade_party_roles_response& v);
    static ores::trading::messaging::list_by_trade_id_trade_party_roles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_trade_party_roles_response& v);
    static ores::trading::messaging::list_by_trade_id_trade_party_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_party_role_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_party_role_versions_request& v);
    static ores::trading::messaging::list_trade_party_role_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_party_role_versions_request& v);
    static ores::trading::messaging::list_trade_party_role_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_party_role_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_party_role_versions_response& v);
    static ores::trading::messaging::list_trade_party_role_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_party_role_versions_response& v);
    static ores::trading::messaging::list_trade_party_role_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_party_role_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_party_role_version_request& v);
    static ores::trading::messaging::get_trade_party_role_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_party_role_version_request& v);
    static ores::trading::messaging::get_trade_party_role_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_party_role_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_party_role_version_response& v);
    static ores::trading::messaging::get_trade_party_role_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_party_role_version_response& v);
    static ores::trading::messaging::get_trade_party_role_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

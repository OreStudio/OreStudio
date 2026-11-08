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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_ADDITIONAL_FIELD_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_ADDITIONAL_FIELD_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_additional_field_codec.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::trading::messaging {

struct trade_additional_field_key {
    boost::uuids::uuid trade_id;
    int sequence_number;

    friend bool operator==(const trade_additional_field_key&,
                           const trade_additional_field_key&) = default;
};

struct trade_additional_field_write {
    boost::uuids::uuid trade_id;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::string name;
    std::string value;

    friend bool operator==(const trade_additional_field_write&,
                           const trade_additional_field_write&) = default;
};

struct trade_additional_field_change {
    trade_additional_field_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const trade_additional_field_change&,
                           const trade_additional_field_change&) = default;
};

struct trade_additional_field_removal {
    trade_additional_field_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const trade_additional_field_removal&,
                           const trade_additional_field_removal&) = default;
};

struct trade_additional_field_lookup {
    trade_additional_field_key key;
    std::optional<ores::trading::domain::trade_additional_field> trade_additional_field;

    friend bool operator==(const trade_additional_field_lookup&,
                           const trade_additional_field_lookup&) = default;
};

struct trade_additional_fields_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const trade_additional_fields_filter&,
                           const trade_additional_fields_filter&) = default;
};

struct trade_additional_field_event {
    boost::uuids::uuid event_id;
    trade_additional_field_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const trade_additional_field_event&,
                           const trade_additional_field_event&) = default;
};

struct trade_additional_field_version_key {
    trade_additional_field_key trade_additional_field;
    std::uint32_t version;

    friend bool operator==(const trade_additional_field_version_key&,
                           const trade_additional_field_version_key&) = default;
};

struct trade_additional_field_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const trade_additional_field_versions_filter&,
                           const trade_additional_field_versions_filter&) = default;
};

struct list_trade_additional_fields_request {
    using response_type = struct list_trade_additional_fields_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.list";
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
    std::optional<trade_additional_fields_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_trade_additional_fields_request&,
                           const list_trade_additional_fields_request&) = default;
};

struct list_trade_additional_fields_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_additional_field> trade_additional_fields;
    std::uint64_t total;

    friend bool operator==(const list_trade_additional_fields_response&,
                           const list_trade_additional_fields_response&) = default;
};

struct get_trade_additional_field_request {
    using response_type = struct get_trade_additional_field_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_key key;

    friend bool operator==(const get_trade_additional_field_request&,
                           const get_trade_additional_field_request&) = default;
};

struct get_trade_additional_field_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_additional_field> trade_additional_field;

    friend bool operator==(const get_trade_additional_field_response&,
                           const get_trade_additional_field_response&) = default;
};

struct get_many_trade_additional_fields_request {
    using response_type = struct get_many_trade_additional_fields_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_additional_field_key> keys;

    friend bool operator==(const get_many_trade_additional_fields_request&,
                           const get_many_trade_additional_fields_request&) = default;
};

struct get_many_trade_additional_fields_response {
    ores::utility::domain::result result;
    std::vector<trade_additional_field_lookup> entries;

    friend bool operator==(const get_many_trade_additional_fields_response&,
                           const get_many_trade_additional_fields_response&) = default;
};

struct put_trade_additional_field_request {
    using response_type = struct put_trade_additional_field_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_trade_additional_field_request&,
                           const put_trade_additional_field_request&) = default;
};

struct put_trade_additional_field_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_additional_field> trade_additional_field;

    friend bool operator==(const put_trade_additional_field_response&,
                           const put_trade_additional_field_response&) = default;
};

struct put_many_trade_additional_fields_request {
    using response_type = struct put_many_trade_additional_fields_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_additional_field_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_trade_additional_fields_request&,
                           const put_many_trade_additional_fields_request&) = default;
};

struct put_many_trade_additional_fields_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_additional_field> trade_additional_fields;

    friend bool operator==(const put_many_trade_additional_fields_response&,
                           const put_many_trade_additional_fields_response&) = default;
};

struct delete_trade_additional_field_request {
    using response_type = struct delete_trade_additional_field_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_trade_additional_field_request&,
                           const delete_trade_additional_field_request&) = default;
};

struct delete_trade_additional_field_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_trade_additional_field_response&,
                           const delete_trade_additional_field_response&) = default;
};

struct delete_many_trade_additional_fields_request {
    using response_type = struct delete_many_trade_additional_fields_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_additional_fields.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_additional_field_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_trade_additional_fields_request&,
                           const delete_many_trade_additional_fields_request&) = default;
};

struct delete_many_trade_additional_fields_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_trade_additional_fields_response&,
                           const delete_many_trade_additional_fields_response&) = default;
};

struct list_by_trade_id_trade_additional_fields_request {
    using response_type = struct list_by_trade_id_trade_additional_fields_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_additional_fields.list_by_trade_id";
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
    std::optional<trade_additional_fields_filter> filter;

    friend bool operator==(const list_by_trade_id_trade_additional_fields_request&,
                           const list_by_trade_id_trade_additional_fields_request&) = default;
};

struct list_by_trade_id_trade_additional_fields_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_additional_field> trade_additional_fields;
    std::uint64_t total;

    friend bool operator==(const list_by_trade_id_trade_additional_fields_response&,
                           const list_by_trade_id_trade_additional_fields_response&) = default;
};

struct list_trade_additional_field_versions_request {
    using response_type = struct list_trade_additional_field_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_additional_fields_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_additional_field_versions_filter> filter;

    friend bool operator==(const list_trade_additional_field_versions_request&,
                           const list_trade_additional_field_versions_request&) = default;
};

struct list_trade_additional_field_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_additional_field> versions;
    std::uint64_t total;

    friend bool operator==(const list_trade_additional_field_versions_response&,
                           const list_trade_additional_field_versions_response&) = default;
};

struct get_trade_additional_field_version_request {
    using response_type = struct get_trade_additional_field_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_additional_fields_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_version_key key;

    friend bool operator==(const get_trade_additional_field_version_request&,
                           const get_trade_additional_field_version_request&) = default;
};

struct get_trade_additional_field_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_additional_field> version;

    friend bool operator==(const get_trade_additional_field_version_response&,
                           const get_trade_additional_field_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_additional_field_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.trade_additional_fields.created";
inline constexpr std::string_view updated = "trading.v1.events.trade_additional_fields.updated";
inline constexpr std::string_view deleted = "trading.v1.events.trade_additional_fields.deleted";
}
/**
 * @brief The members of trade_additional_field_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_additional_field_key>) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_additional_field_key::trade_id},
        utility::codec::field{"sequence_number", &trade_additional_field_key::sequence_number},
    };
}

/**
 * @brief The members of trade_additional_field_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_additional_field_write>) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_additional_field_write::trade_id},
        utility::codec::field{"sequence_number", &trade_additional_field_write::sequence_number},
        utility::codec::field{"trade_activity_id",
                              &trade_additional_field_write::trade_activity_id},
        utility::codec::field{"name", &trade_additional_field_write::name},
        utility::codec::field{"value", &trade_additional_field_write::value},
    };
}

/**
 * @brief The members of trade_additional_field_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_additional_field_change>) {
    return std::tuple{
        utility::codec::field{"write", &trade_additional_field_change::write},
        utility::codec::field{"precondition", &trade_additional_field_change::precondition},
    };
}

/**
 * @brief The members of trade_additional_field_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_additional_field_removal>) {
    return std::tuple{
        utility::codec::field{"key", &trade_additional_field_removal::key},
        utility::codec::field{"precondition", &trade_additional_field_removal::precondition},
    };
}

/**
 * @brief The members of trade_additional_field_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_additional_field_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &trade_additional_field_lookup::key},
        utility::codec::field{"trade_additional_field",
                              &trade_additional_field_lookup::trade_additional_field},
    };
}

/**
 * @brief The members of trade_additional_fields_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_additional_fields_filter>) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_additional_fields_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &trade_additional_fields_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of trade_additional_field_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_additional_field_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &trade_additional_field_event::event_id},
        utility::codec::field{"key", &trade_additional_field_event::key},
        utility::codec::field{"action", &trade_additional_field_event::action},
        utility::codec::field{"version", &trade_additional_field_event::version},
        utility::codec::field{"occurred_at", &trade_additional_field_event::occurred_at},
        utility::codec::field{"correlation_id", &trade_additional_field_event::correlation_id},
    };
}

/**
 * @brief The members of trade_additional_field_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_additional_field_version_key>) {
    return std::tuple{
        utility::codec::field{"trade_additional_field",
                              &trade_additional_field_version_key::trade_additional_field},
        utility::codec::field{"version", &trade_additional_field_version_key::version},
    };
}

/**
 * @brief The members of trade_additional_field_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<trade_additional_field_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &trade_additional_field_versions_filter::version},
        utility::codec::field{"from_version",
                              &trade_additional_field_versions_filter::from_version},
        utility::codec::field{"to_version", &trade_additional_field_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_trade_additional_fields_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_trade_additional_fields_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_trade_additional_fields_request::offset},
        utility::codec::field{"limit", &list_trade_additional_fields_request::limit},
        utility::codec::field{"order", &list_trade_additional_fields_request::order},
        utility::codec::field{"filter", &list_trade_additional_fields_request::filter},
        utility::codec::field{"as_of", &list_trade_additional_fields_request::as_of},
    };
}

/**
 * @brief The members of list_trade_additional_fields_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_trade_additional_fields_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_additional_fields_response::result},
        utility::codec::field{"trade_additional_fields",
                              &list_trade_additional_fields_response::trade_additional_fields},
        utility::codec::field{"total", &list_trade_additional_fields_response::total},
    };
}

/**
 * @brief The members of get_trade_additional_field_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_trade_additional_field_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_additional_field_request::key},
    };
}

/**
 * @brief The members of get_trade_additional_field_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_trade_additional_field_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_additional_field_response::result},
        utility::codec::field{"trade_additional_field",
                              &get_trade_additional_field_response::trade_additional_field},
    };
}

/**
 * @brief The members of get_many_trade_additional_fields_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_trade_additional_fields_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_trade_additional_fields_request::keys},
    };
}

/**
 * @brief The members of get_many_trade_additional_fields_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_trade_additional_fields_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_trade_additional_fields_response::result},
        utility::codec::field{"entries", &get_many_trade_additional_fields_response::entries},
    };
}

/**
 * @brief The members of put_trade_additional_field_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_trade_additional_field_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_trade_additional_field_request::change},
        utility::codec::field{"intent", &put_trade_additional_field_request::intent},
    };
}

/**
 * @brief The members of put_trade_additional_field_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_trade_additional_field_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_trade_additional_field_response::result},
        utility::codec::field{"trade_additional_field",
                              &put_trade_additional_field_response::trade_additional_field},
    };
}

/**
 * @brief The members of put_many_trade_additional_fields_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_trade_additional_fields_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_trade_additional_fields_request::changes},
        utility::codec::field{"intent", &put_many_trade_additional_fields_request::intent},
    };
}

/**
 * @brief The members of put_many_trade_additional_fields_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_trade_additional_fields_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_trade_additional_fields_response::result},
        utility::codec::field{"trade_additional_fields",
                              &put_many_trade_additional_fields_response::trade_additional_fields},
    };
}

/**
 * @brief The members of delete_trade_additional_field_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_trade_additional_field_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_trade_additional_field_request::removal},
        utility::codec::field{"intent", &delete_trade_additional_field_request::intent},
    };
}

/**
 * @brief The members of delete_trade_additional_field_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_trade_additional_field_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_trade_additional_field_response::result},
    };
}

/**
 * @brief The members of delete_many_trade_additional_fields_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_trade_additional_fields_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_trade_additional_fields_request::removals},
        utility::codec::field{"intent", &delete_many_trade_additional_fields_request::intent},
    };
}

/**
 * @brief The members of delete_many_trade_additional_fields_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_trade_additional_fields_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_trade_additional_fields_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_trade_additional_fields_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_trade_additional_fields_request>) {
    return std::tuple{
        utility::codec::field{"trade_id",
                              &list_by_trade_id_trade_additional_fields_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_trade_additional_fields_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_trade_additional_fields_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_trade_additional_fields_request::limit},
        utility::codec::field{"order", &list_by_trade_id_trade_additional_fields_request::order},
        utility::codec::field{"filter", &list_by_trade_id_trade_additional_fields_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_trade_additional_fields_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_trade_additional_fields_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_trade_id_trade_additional_fields_response::result},
        utility::codec::field{
            "trade_additional_fields",
            &list_by_trade_id_trade_additional_fields_response::trade_additional_fields},
        utility::codec::field{"total", &list_by_trade_id_trade_additional_fields_response::total},
    };
}

/**
 * @brief The members of list_trade_additional_field_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_trade_additional_field_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_trade_additional_field_versions_request::key},
        utility::codec::field{"offset", &list_trade_additional_field_versions_request::offset},
        utility::codec::field{"limit", &list_trade_additional_field_versions_request::limit},
        utility::codec::field{"order", &list_trade_additional_field_versions_request::order},
        utility::codec::field{"filter", &list_trade_additional_field_versions_request::filter},
    };
}

/**
 * @brief The members of list_trade_additional_field_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_trade_additional_field_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_additional_field_versions_response::result},
        utility::codec::field{"versions", &list_trade_additional_field_versions_response::versions},
        utility::codec::field{"total", &list_trade_additional_field_versions_response::total},
    };
}

/**
 * @brief The members of get_trade_additional_field_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_trade_additional_field_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_additional_field_version_request::key},
    };
}

/**
 * @brief The members of get_trade_additional_field_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_trade_additional_field_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_additional_field_version_response::result},
        utility::codec::field{"version", &get_trade_additional_field_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_additional_field_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_additional_field_key& v);
    static ores::trading::messaging::trade_additional_field_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_additional_field_key& v);
    static ores::trading::messaging::trade_additional_field_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_additional_field_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_additional_field_write& v);
    static ores::trading::messaging::trade_additional_field_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_additional_field_write& v);
    static ores::trading::messaging::trade_additional_field_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_additional_field_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_additional_field_change& v);
    static ores::trading::messaging::trade_additional_field_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_additional_field_change& v);
    static ores::trading::messaging::trade_additional_field_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_additional_field_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_additional_field_removal& v);
    static ores::trading::messaging::trade_additional_field_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_additional_field_removal& v);
    static ores::trading::messaging::trade_additional_field_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_additional_field_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_additional_field_lookup& v);
    static ores::trading::messaging::trade_additional_field_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_additional_field_lookup& v);
    static ores::trading::messaging::trade_additional_field_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_additional_fields_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_additional_fields_filter& v);
    static ores::trading::messaging::trade_additional_fields_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_additional_fields_filter& v);
    static ores::trading::messaging::trade_additional_fields_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_additional_field_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_additional_field_event& v);
    static ores::trading::messaging::trade_additional_field_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_additional_field_event& v);
    static ores::trading::messaging::trade_additional_field_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_additional_field_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_additional_field_version_key& v);
    static ores::trading::messaging::trade_additional_field_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_additional_field_version_key& v);
    static ores::trading::messaging::trade_additional_field_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_additional_field_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_additional_field_versions_filter& v);
    static ores::trading::messaging::trade_additional_field_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_additional_field_versions_filter& v);
    static ores::trading::messaging::trade_additional_field_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_additional_fields_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_additional_fields_request& v);
    static ores::trading::messaging::list_trade_additional_fields_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_additional_fields_request& v);
    static ores::trading::messaging::list_trade_additional_fields_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_additional_fields_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_additional_fields_response& v);
    static ores::trading::messaging::list_trade_additional_fields_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_additional_fields_response& v);
    static ores::trading::messaging::list_trade_additional_fields_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_additional_field_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_additional_field_request& v);
    static ores::trading::messaging::get_trade_additional_field_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_additional_field_request& v);
    static ores::trading::messaging::get_trade_additional_field_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_additional_field_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_additional_field_response& v);
    static ores::trading::messaging::get_trade_additional_field_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_additional_field_response& v);
    static ores::trading::messaging::get_trade_additional_field_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_additional_fields_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_additional_fields_request& v);
    static ores::trading::messaging::get_many_trade_additional_fields_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_additional_fields_request& v);
    static ores::trading::messaging::get_many_trade_additional_fields_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_additional_fields_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_additional_fields_response& v);
    static ores::trading::messaging::get_many_trade_additional_fields_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_additional_fields_response& v);
    static ores::trading::messaging::get_many_trade_additional_fields_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_trade_additional_field_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_trade_additional_field_request& v);
    static ores::trading::messaging::put_trade_additional_field_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_trade_additional_field_request& v);
    static ores::trading::messaging::put_trade_additional_field_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_trade_additional_field_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_trade_additional_field_response& v);
    static ores::trading::messaging::put_trade_additional_field_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_trade_additional_field_response& v);
    static ores::trading::messaging::put_trade_additional_field_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_additional_fields_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_additional_fields_request& v);
    static ores::trading::messaging::put_many_trade_additional_fields_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_additional_fields_request& v);
    static ores::trading::messaging::put_many_trade_additional_fields_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_additional_fields_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_additional_fields_response& v);
    static ores::trading::messaging::put_many_trade_additional_fields_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_additional_fields_response& v);
    static ores::trading::messaging::put_many_trade_additional_fields_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_trade_additional_field_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_trade_additional_field_request& v);
    static ores::trading::messaging::delete_trade_additional_field_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_additional_field_request& v);
    static ores::trading::messaging::delete_trade_additional_field_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_trade_additional_field_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_trade_additional_field_response& v);
    static ores::trading::messaging::delete_trade_additional_field_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_additional_field_response& v);
    static ores::trading::messaging::delete_trade_additional_field_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_additional_fields_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_additional_fields_request& v);
    static ores::trading::messaging::delete_many_trade_additional_fields_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_additional_fields_request& v);
    static ores::trading::messaging::delete_many_trade_additional_fields_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_additional_fields_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_additional_fields_response& v);
    static ores::trading::messaging::delete_many_trade_additional_fields_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_additional_fields_response& v);
    static ores::trading::messaging::delete_many_trade_additional_fields_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_trade_additional_fields_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_trade_additional_fields_request& v);
    static ores::trading::messaging::list_by_trade_id_trade_additional_fields_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_trade_additional_fields_request& v);
    static ores::trading::messaging::list_by_trade_id_trade_additional_fields_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_trade_additional_fields_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_trade_additional_fields_response& v);
    static ores::trading::messaging::list_by_trade_id_trade_additional_fields_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_trade_additional_fields_response& v);
    static ores::trading::messaging::list_by_trade_id_trade_additional_fields_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_additional_field_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_additional_field_versions_request& v);
    static ores::trading::messaging::list_trade_additional_field_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_additional_field_versions_request& v);
    static ores::trading::messaging::list_trade_additional_field_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_additional_field_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_additional_field_versions_response& v);
    static ores::trading::messaging::list_trade_additional_field_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_additional_field_versions_response& v);
    static ores::trading::messaging::list_trade_additional_field_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_additional_field_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_additional_field_version_request& v);
    static ores::trading::messaging::get_trade_additional_field_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_additional_field_version_request& v);
    static ores::trading::messaging::get_trade_additional_field_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_additional_field_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_additional_field_version_response& v);
    static ores::trading::messaging::get_trade_additional_field_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_additional_field_version_response& v);
    static ores::trading::messaging::get_trade_additional_field_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_BARRIER_LEVEL_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_BARRIER_LEVEL_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_barrier_level_codec.hpp"
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

struct instrument_barrier_level_key {
    boost::uuids::uuid trade_id;
    std::string barrier_role;
    int barrier_sequence_number{};
    int sequence_number{};

    friend bool operator==(const instrument_barrier_level_key&,
                           const instrument_barrier_level_key&) = default;
};

struct instrument_barrier_level_write {
    boost::uuids::uuid trade_id;
    std::string barrier_role;
    int barrier_sequence_number{};
    int sequence_number{};
    boost::uuids::uuid trade_activity_id;
    ores::utility::decimal::decimal level;

    friend bool operator==(const instrument_barrier_level_write&,
                           const instrument_barrier_level_write&) = default;
};

struct instrument_barrier_level_change {
    instrument_barrier_level_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const instrument_barrier_level_change&,
                           const instrument_barrier_level_change&) = default;
};

struct instrument_barrier_level_removal {
    instrument_barrier_level_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const instrument_barrier_level_removal&,
                           const instrument_barrier_level_removal&) = default;
};

struct instrument_barrier_level_lookup {
    instrument_barrier_level_key key;
    std::optional<ores::trading::domain::instrument_barrier_level> instrument_barrier_level;

    friend bool operator==(const instrument_barrier_level_lookup&,
                           const instrument_barrier_level_lookup&) = default;
};

struct instrument_barrier_levels_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const instrument_barrier_levels_filter&,
                           const instrument_barrier_levels_filter&) = default;
};

struct instrument_barrier_level_event {
    boost::uuids::uuid event_id;
    instrument_barrier_level_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const instrument_barrier_level_event&,
                           const instrument_barrier_level_event&) = default;
};

struct instrument_barrier_level_version_key {
    instrument_barrier_level_key instrument_barrier_level;
    std::uint32_t version{};

    friend bool operator==(const instrument_barrier_level_version_key&,
                           const instrument_barrier_level_version_key&) = default;
};

struct instrument_barrier_level_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const instrument_barrier_level_versions_filter&,
                           const instrument_barrier_level_versions_filter&) = default;
};

struct list_instrument_barrier_levels_request {
    using response_type = struct list_instrument_barrier_levels_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_barrier_levels.list";
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
    std::optional<instrument_barrier_levels_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_instrument_barrier_levels_request&,
                           const list_instrument_barrier_levels_request&) = default;
};

struct list_instrument_barrier_levels_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_barrier_level> instrument_barrier_levels;
    std::uint64_t total{};

    friend bool operator==(const list_instrument_barrier_levels_response&,
                           const list_instrument_barrier_levels_response&) = default;
};

struct get_instrument_barrier_level_request {
    using response_type = struct get_instrument_barrier_level_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_barrier_levels.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_barrier_level_key key;

    friend bool operator==(const get_instrument_barrier_level_request&,
                           const get_instrument_barrier_level_request&) = default;
};

struct get_instrument_barrier_level_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_barrier_level> instrument_barrier_level;

    friend bool operator==(const get_instrument_barrier_level_response&,
                           const get_instrument_barrier_level_response&) = default;
};

struct get_many_instrument_barrier_levels_request {
    using response_type = struct get_many_instrument_barrier_levels_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_barrier_levels.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_barrier_level_key> keys;

    friend bool operator==(const get_many_instrument_barrier_levels_request&,
                           const get_many_instrument_barrier_levels_request&) = default;
};

struct get_many_instrument_barrier_levels_response {
    ores::utility::domain::result result;
    std::vector<instrument_barrier_level_lookup> entries;

    friend bool operator==(const get_many_instrument_barrier_levels_response&,
                           const get_many_instrument_barrier_levels_response&) = default;
};

struct put_instrument_barrier_level_request {
    using response_type = struct put_instrument_barrier_level_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_barrier_levels.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_barrier_level_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_instrument_barrier_level_request&,
                           const put_instrument_barrier_level_request&) = default;
};

struct put_instrument_barrier_level_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_barrier_level> instrument_barrier_level;

    friend bool operator==(const put_instrument_barrier_level_response&,
                           const put_instrument_barrier_level_response&) = default;
};

struct put_many_instrument_barrier_levels_request {
    using response_type = struct put_many_instrument_barrier_levels_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_barrier_levels.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_barrier_level_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_instrument_barrier_levels_request&,
                           const put_many_instrument_barrier_levels_request&) = default;
};

struct put_many_instrument_barrier_levels_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_barrier_level> instrument_barrier_levels;

    friend bool operator==(const put_many_instrument_barrier_levels_response&,
                           const put_many_instrument_barrier_levels_response&) = default;
};

struct delete_instrument_barrier_level_request {
    using response_type = struct delete_instrument_barrier_level_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_barrier_levels.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_barrier_level_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_instrument_barrier_level_request&,
                           const delete_instrument_barrier_level_request&) = default;
};

struct delete_instrument_barrier_level_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_instrument_barrier_level_response&,
                           const delete_instrument_barrier_level_response&) = default;
};

struct delete_many_instrument_barrier_levels_request {
    using response_type = struct delete_many_instrument_barrier_levels_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_barrier_levels.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_barrier_level_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_instrument_barrier_levels_request&,
                           const delete_many_instrument_barrier_levels_request&) = default;
};

struct delete_many_instrument_barrier_levels_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_instrument_barrier_levels_response&,
                           const delete_many_instrument_barrier_levels_response&) = default;
};

struct list_by_trade_id_instrument_barrier_levels_request {
    using response_type = struct list_by_trade_id_instrument_barrier_levels_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_barrier_levels.list_by_trade_id";
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
    std::optional<instrument_barrier_levels_filter> filter;

    friend bool operator==(const list_by_trade_id_instrument_barrier_levels_request&,
                           const list_by_trade_id_instrument_barrier_levels_request&) = default;
};

struct list_by_trade_id_instrument_barrier_levels_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_barrier_level> instrument_barrier_levels;
    std::uint64_t total{};

    friend bool operator==(const list_by_trade_id_instrument_barrier_levels_response&,
                           const list_by_trade_id_instrument_barrier_levels_response&) = default;
};

struct list_instrument_barrier_level_versions_request {
    using response_type = struct list_instrument_barrier_level_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_barrier_levels_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_barrier_level_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_barrier_level_versions_filter> filter;

    friend bool operator==(const list_instrument_barrier_level_versions_request&,
                           const list_instrument_barrier_level_versions_request&) = default;
};

struct list_instrument_barrier_level_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_barrier_level> versions;
    std::uint64_t total{};

    friend bool operator==(const list_instrument_barrier_level_versions_response&,
                           const list_instrument_barrier_level_versions_response&) = default;
};

struct get_instrument_barrier_level_version_request {
    using response_type = struct get_instrument_barrier_level_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_barrier_levels_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_barrier_level_version_key key;

    friend bool operator==(const get_instrument_barrier_level_version_request&,
                           const get_instrument_barrier_level_version_request&) = default;
};

struct get_instrument_barrier_level_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_barrier_level> version;

    friend bool operator==(const get_instrument_barrier_level_version_response&,
                           const get_instrument_barrier_level_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace instrument_barrier_level_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.instrument_barrier_levels.created";
inline constexpr std::string_view updated = "trading.v1.events.instrument_barrier_levels.updated";
inline constexpr std::string_view deleted = "trading.v1.events.instrument_barrier_levels.deleted";
}
/**
 * @brief The members of instrument_barrier_level_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier_level_key>) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_barrier_level_key::trade_id},
        utility::codec::field{"barrier_role", &instrument_barrier_level_key::barrier_role},
        utility::codec::field{"barrier_sequence_number",
                              &instrument_barrier_level_key::barrier_sequence_number},
        utility::codec::field{"sequence_number", &instrument_barrier_level_key::sequence_number},
    };
}

/**
 * @brief The members of instrument_barrier_level_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier_level_write>) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_barrier_level_write::trade_id},
        utility::codec::field{"barrier_role", &instrument_barrier_level_write::barrier_role},
        utility::codec::field{"barrier_sequence_number",
                              &instrument_barrier_level_write::barrier_sequence_number},
        utility::codec::field{"sequence_number", &instrument_barrier_level_write::sequence_number},
        utility::codec::field{"trade_activity_id",
                              &instrument_barrier_level_write::trade_activity_id},
        utility::codec::field{"level", &instrument_barrier_level_write::level},
    };
}

/**
 * @brief The members of instrument_barrier_level_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier_level_change>) {
    return std::tuple{
        utility::codec::field{"write", &instrument_barrier_level_change::write},
        utility::codec::field{"precondition", &instrument_barrier_level_change::precondition},
    };
}

/**
 * @brief The members of instrument_barrier_level_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier_level_removal>) {
    return std::tuple{
        utility::codec::field{"key", &instrument_barrier_level_removal::key},
        utility::codec::field{"precondition", &instrument_barrier_level_removal::precondition},
    };
}

/**
 * @brief The members of instrument_barrier_level_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier_level_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &instrument_barrier_level_lookup::key},
        utility::codec::field{"instrument_barrier_level",
                              &instrument_barrier_level_lookup::instrument_barrier_level},
    };
}

/**
 * @brief The members of instrument_barrier_levels_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier_levels_filter>) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_barrier_levels_filter::trade_id},
        utility::codec::field{"trade_id_one_of",
                              &instrument_barrier_levels_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of instrument_barrier_level_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier_level_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &instrument_barrier_level_event::event_id},
        utility::codec::field{"key", &instrument_barrier_level_event::key},
        utility::codec::field{"action", &instrument_barrier_level_event::action},
        utility::codec::field{"version", &instrument_barrier_level_event::version},
        utility::codec::field{"occurred_at", &instrument_barrier_level_event::occurred_at},
        utility::codec::field{"correlation_id", &instrument_barrier_level_event::correlation_id},
    };
}

/**
 * @brief The members of instrument_barrier_level_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier_level_version_key>) {
    return std::tuple{
        utility::codec::field{"instrument_barrier_level",
                              &instrument_barrier_level_version_key::instrument_barrier_level},
        utility::codec::field{"version", &instrument_barrier_level_version_key::version},
    };
}

/**
 * @brief The members of instrument_barrier_level_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<instrument_barrier_level_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &instrument_barrier_level_versions_filter::version},
        utility::codec::field{"from_version",
                              &instrument_barrier_level_versions_filter::from_version},
        utility::codec::field{"to_version", &instrument_barrier_level_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_instrument_barrier_levels_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_barrier_levels_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_instrument_barrier_levels_request::offset},
        utility::codec::field{"limit", &list_instrument_barrier_levels_request::limit},
        utility::codec::field{"order", &list_instrument_barrier_levels_request::order},
        utility::codec::field{"filter", &list_instrument_barrier_levels_request::filter},
        utility::codec::field{"as_of", &list_instrument_barrier_levels_request::as_of},
    };
}

/**
 * @brief The members of list_instrument_barrier_levels_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_barrier_levels_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_instrument_barrier_levels_response::result},
        utility::codec::field{"instrument_barrier_levels",
                              &list_instrument_barrier_levels_response::instrument_barrier_levels},
        utility::codec::field{"total", &list_instrument_barrier_levels_response::total},
    };
}

/**
 * @brief The members of get_instrument_barrier_level_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_instrument_barrier_level_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_instrument_barrier_level_request::key},
    };
}

/**
 * @brief The members of get_instrument_barrier_level_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_barrier_level_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_instrument_barrier_level_response::result},
        utility::codec::field{"instrument_barrier_level",
                              &get_instrument_barrier_level_response::instrument_barrier_level},
    };
}

/**
 * @brief The members of get_many_instrument_barrier_levels_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_instrument_barrier_levels_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_instrument_barrier_levels_request::keys},
    };
}

/**
 * @brief The members of get_many_instrument_barrier_levels_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_instrument_barrier_levels_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_instrument_barrier_levels_response::result},
        utility::codec::field{"entries", &get_many_instrument_barrier_levels_response::entries},
    };
}

/**
 * @brief The members of put_instrument_barrier_level_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_instrument_barrier_level_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_instrument_barrier_level_request::change},
        utility::codec::field{"intent", &put_instrument_barrier_level_request::intent},
    };
}

/**
 * @brief The members of put_instrument_barrier_level_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_instrument_barrier_level_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_instrument_barrier_level_response::result},
        utility::codec::field{"instrument_barrier_level",
                              &put_instrument_barrier_level_response::instrument_barrier_level},
    };
}

/**
 * @brief The members of put_many_instrument_barrier_levels_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_instrument_barrier_levels_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_instrument_barrier_levels_request::changes},
        utility::codec::field{"intent", &put_many_instrument_barrier_levels_request::intent},
    };
}

/**
 * @brief The members of put_many_instrument_barrier_levels_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_instrument_barrier_levels_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_instrument_barrier_levels_response::result},
        utility::codec::field{
            "instrument_barrier_levels",
            &put_many_instrument_barrier_levels_response::instrument_barrier_levels},
    };
}

/**
 * @brief The members of delete_instrument_barrier_level_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_instrument_barrier_level_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_instrument_barrier_level_request::removal},
        utility::codec::field{"intent", &delete_instrument_barrier_level_request::intent},
    };
}

/**
 * @brief The members of delete_instrument_barrier_level_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_instrument_barrier_level_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_instrument_barrier_level_response::result},
    };
}

/**
 * @brief The members of delete_many_instrument_barrier_levels_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_instrument_barrier_levels_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_instrument_barrier_levels_request::removals},
        utility::codec::field{"intent", &delete_many_instrument_barrier_levels_request::intent},
    };
}

/**
 * @brief The members of delete_many_instrument_barrier_levels_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_instrument_barrier_levels_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_instrument_barrier_levels_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_instrument_barrier_levels_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_instrument_barrier_levels_request>) {
    return std::tuple{
        utility::codec::field{"trade_id",
                              &list_by_trade_id_instrument_barrier_levels_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_instrument_barrier_levels_request::scope},
        utility::codec::field{"offset",
                              &list_by_trade_id_instrument_barrier_levels_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_instrument_barrier_levels_request::limit},
        utility::codec::field{"order", &list_by_trade_id_instrument_barrier_levels_request::order},
        utility::codec::field{"filter",
                              &list_by_trade_id_instrument_barrier_levels_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_instrument_barrier_levels_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_instrument_barrier_levels_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_trade_id_instrument_barrier_levels_response::result},
        utility::codec::field{
            "instrument_barrier_levels",
            &list_by_trade_id_instrument_barrier_levels_response::instrument_barrier_levels},
        utility::codec::field{"total", &list_by_trade_id_instrument_barrier_levels_response::total},
    };
}

/**
 * @brief The members of list_instrument_barrier_level_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_barrier_level_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_instrument_barrier_level_versions_request::key},
        utility::codec::field{"offset", &list_instrument_barrier_level_versions_request::offset},
        utility::codec::field{"limit", &list_instrument_barrier_level_versions_request::limit},
        utility::codec::field{"order", &list_instrument_barrier_level_versions_request::order},
        utility::codec::field{"filter", &list_instrument_barrier_level_versions_request::filter},
    };
}

/**
 * @brief The members of list_instrument_barrier_level_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_barrier_level_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_instrument_barrier_level_versions_response::result},
        utility::codec::field{"versions",
                              &list_instrument_barrier_level_versions_response::versions},
        utility::codec::field{"total", &list_instrument_barrier_level_versions_response::total},
    };
}

/**
 * @brief The members of get_instrument_barrier_level_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_barrier_level_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_instrument_barrier_level_version_request::key},
    };
}

/**
 * @brief The members of get_instrument_barrier_level_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_barrier_level_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_instrument_barrier_level_version_response::result},
        utility::codec::field{"version", &get_instrument_barrier_level_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_barrier_level_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_barrier_level_key& v);
    static ores::trading::messaging::instrument_barrier_level_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_level_key& v);
    static ores::trading::messaging::instrument_barrier_level_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_barrier_level_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_barrier_level_write& v);
    static ores::trading::messaging::instrument_barrier_level_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_level_write& v);
    static ores::trading::messaging::instrument_barrier_level_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_barrier_level_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_barrier_level_change& v);
    static ores::trading::messaging::instrument_barrier_level_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_level_change& v);
    static ores::trading::messaging::instrument_barrier_level_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_barrier_level_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_barrier_level_removal& v);
    static ores::trading::messaging::instrument_barrier_level_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_level_removal& v);
    static ores::trading::messaging::instrument_barrier_level_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_barrier_level_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_barrier_level_lookup& v);
    static ores::trading::messaging::instrument_barrier_level_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_level_lookup& v);
    static ores::trading::messaging::instrument_barrier_level_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_barrier_levels_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_barrier_levels_filter& v);
    static ores::trading::messaging::instrument_barrier_levels_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_levels_filter& v);
    static ores::trading::messaging::instrument_barrier_levels_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_barrier_level_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_barrier_level_event& v);
    static ores::trading::messaging::instrument_barrier_level_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_level_event& v);
    static ores::trading::messaging::instrument_barrier_level_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_barrier_level_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_barrier_level_version_key& v);
    static ores::trading::messaging::instrument_barrier_level_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_level_version_key& v);
    static ores::trading::messaging::instrument_barrier_level_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_barrier_level_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_barrier_level_versions_filter& v);
    static ores::trading::messaging::instrument_barrier_level_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_barrier_level_versions_filter& v);
    static ores::trading::messaging::instrument_barrier_level_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_barrier_levels_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_barrier_levels_request& v);
    static ores::trading::messaging::list_instrument_barrier_levels_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_barrier_levels_request& v);
    static ores::trading::messaging::list_instrument_barrier_levels_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_barrier_levels_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_barrier_levels_response& v);
    static ores::trading::messaging::list_instrument_barrier_levels_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_barrier_levels_response& v);
    static ores::trading::messaging::list_instrument_barrier_levels_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_barrier_level_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_barrier_level_request& v);
    static ores::trading::messaging::get_instrument_barrier_level_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_barrier_level_request& v);
    static ores::trading::messaging::get_instrument_barrier_level_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_barrier_level_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_barrier_level_response& v);
    static ores::trading::messaging::get_instrument_barrier_level_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_barrier_level_response& v);
    static ores::trading::messaging::get_instrument_barrier_level_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_barrier_levels_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_barrier_levels_request& v);
    static ores::trading::messaging::get_many_instrument_barrier_levels_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_barrier_levels_request& v);
    static ores::trading::messaging::get_many_instrument_barrier_levels_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_barrier_levels_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_barrier_levels_response& v);
    static ores::trading::messaging::get_many_instrument_barrier_levels_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_barrier_levels_response& v);
    static ores::trading::messaging::get_many_instrument_barrier_levels_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_barrier_level_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_barrier_level_request& v);
    static ores::trading::messaging::put_instrument_barrier_level_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_barrier_level_request& v);
    static ores::trading::messaging::put_instrument_barrier_level_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_barrier_level_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_barrier_level_response& v);
    static ores::trading::messaging::put_instrument_barrier_level_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_barrier_level_response& v);
    static ores::trading::messaging::put_instrument_barrier_level_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_barrier_levels_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_barrier_levels_request& v);
    static ores::trading::messaging::put_many_instrument_barrier_levels_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_barrier_levels_request& v);
    static ores::trading::messaging::put_many_instrument_barrier_levels_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_barrier_levels_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_barrier_levels_response& v);
    static ores::trading::messaging::put_many_instrument_barrier_levels_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_barrier_levels_response& v);
    static ores::trading::messaging::put_many_instrument_barrier_levels_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_barrier_level_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_barrier_level_request& v);
    static ores::trading::messaging::delete_instrument_barrier_level_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_barrier_level_request& v);
    static ores::trading::messaging::delete_instrument_barrier_level_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_barrier_level_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_barrier_level_response& v);
    static ores::trading::messaging::delete_instrument_barrier_level_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_barrier_level_response& v);
    static ores::trading::messaging::delete_instrument_barrier_level_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_barrier_levels_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_barrier_levels_request& v);
    static ores::trading::messaging::delete_many_instrument_barrier_levels_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_barrier_levels_request& v);
    static ores::trading::messaging::delete_many_instrument_barrier_levels_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_barrier_levels_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_barrier_levels_response& v);
    static ores::trading::messaging::delete_many_instrument_barrier_levels_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_instrument_barrier_levels_response& v);
    static ores::trading::messaging::delete_many_instrument_barrier_levels_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_request& v);
    static ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_request& v);
    static ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_response& v);
    static ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_response& v);
    static ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_barrier_level_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_barrier_level_versions_request& v);
    static ores::trading::messaging::list_instrument_barrier_level_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_instrument_barrier_level_versions_request& v);
    static ores::trading::messaging::list_instrument_barrier_level_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_barrier_level_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_barrier_level_versions_response& v);
    static ores::trading::messaging::list_instrument_barrier_level_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_instrument_barrier_level_versions_response& v);
    static ores::trading::messaging::list_instrument_barrier_level_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_barrier_level_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_barrier_level_version_request& v);
    static ores::trading::messaging::get_instrument_barrier_level_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_barrier_level_version_request& v);
    static ores::trading::messaging::get_instrument_barrier_level_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_barrier_level_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_barrier_level_version_response& v);
    static ores::trading::messaging::get_instrument_barrier_level_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_barrier_level_version_response& v);
    static ores::trading::messaging::get_instrument_barrier_level_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

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
#ifndef ORES_TRADING_API_MESSAGING_SWAP_LEG_INDEXING_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_SWAP_LEG_INDEXING_PROTOCOL_HPP

#include "ores.trading.api/domain/swap_leg_indexing.hpp"
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

struct swap_leg_indexing_key {
    boost::uuids::uuid trade_id;
    int leg_number;
    int sequence_number;

    friend bool operator==(const swap_leg_indexing_key&, const swap_leg_indexing_key&) = default;
};

struct swap_leg_indexing_write {
    boost::uuids::uuid trade_id;
    int leg_number;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::string index_name;
    std::optional<double> quantity;
    std::optional<ores::utility::decimal::decimal> initial_fixing;
    std::optional<int> fixing_days;
    std::optional<std::string> fixing_calendar;
    std::optional<std::string> fixing_convention;
    std::optional<bool> is_in_arrears;

    friend bool operator==(const swap_leg_indexing_write&,
                           const swap_leg_indexing_write&) = default;
};

struct swap_leg_indexing_change {
    swap_leg_indexing_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const swap_leg_indexing_change&,
                           const swap_leg_indexing_change&) = default;
};

struct swap_leg_indexing_removal {
    swap_leg_indexing_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const swap_leg_indexing_removal&,
                           const swap_leg_indexing_removal&) = default;
};

struct swap_leg_indexing_lookup {
    swap_leg_indexing_key key;
    std::optional<ores::trading::domain::swap_leg_indexing> swap_leg_indexing;

    friend bool operator==(const swap_leg_indexing_lookup&,
                           const swap_leg_indexing_lookup&) = default;
};

struct swap_leg_indexings_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const swap_leg_indexings_filter&,
                           const swap_leg_indexings_filter&) = default;
};

struct swap_leg_indexing_event {
    boost::uuids::uuid event_id;
    swap_leg_indexing_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const swap_leg_indexing_event&,
                           const swap_leg_indexing_event&) = default;
};

struct swap_leg_indexing_version_key {
    swap_leg_indexing_key swap_leg_indexing;
    std::uint32_t version;

    friend bool operator==(const swap_leg_indexing_version_key&,
                           const swap_leg_indexing_version_key&) = default;
};

struct swap_leg_indexing_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const swap_leg_indexing_versions_filter&,
                           const swap_leg_indexing_versions_filter&) = default;
};

struct list_swap_leg_indexings_request {
    using response_type = struct list_swap_leg_indexings_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings.list";
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
    std::optional<swap_leg_indexings_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_swap_leg_indexings_request&,
                           const list_swap_leg_indexings_request&) = default;
};

struct list_swap_leg_indexings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::swap_leg_indexing> swap_leg_indexings;
    std::uint64_t total;

    friend bool operator==(const list_swap_leg_indexings_response&,
                           const list_swap_leg_indexings_response&) = default;
};

struct get_swap_leg_indexing_request {
    using response_type = struct get_swap_leg_indexing_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_indexing_key key;

    friend bool operator==(const get_swap_leg_indexing_request&,
                           const get_swap_leg_indexing_request&) = default;
};

struct get_swap_leg_indexing_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::swap_leg_indexing> swap_leg_indexing;

    friend bool operator==(const get_swap_leg_indexing_response&,
                           const get_swap_leg_indexing_response&) = default;
};

struct get_many_swap_leg_indexings_request {
    using response_type = struct get_many_swap_leg_indexings_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swap_leg_indexing_key> keys;

    friend bool operator==(const get_many_swap_leg_indexings_request&,
                           const get_many_swap_leg_indexings_request&) = default;
};

struct get_many_swap_leg_indexings_response {
    ores::utility::domain::result result;
    std::vector<swap_leg_indexing_lookup> entries;

    friend bool operator==(const get_many_swap_leg_indexings_response&,
                           const get_many_swap_leg_indexings_response&) = default;
};

struct put_swap_leg_indexing_request {
    using response_type = struct put_swap_leg_indexing_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_indexing_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_swap_leg_indexing_request&,
                           const put_swap_leg_indexing_request&) = default;
};

struct put_swap_leg_indexing_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::swap_leg_indexing> swap_leg_indexing;

    friend bool operator==(const put_swap_leg_indexing_response&,
                           const put_swap_leg_indexing_response&) = default;
};

struct put_many_swap_leg_indexings_request {
    using response_type = struct put_many_swap_leg_indexings_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swap_leg_indexing_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_swap_leg_indexings_request&,
                           const put_many_swap_leg_indexings_request&) = default;
};

struct put_many_swap_leg_indexings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::swap_leg_indexing> swap_leg_indexings;

    friend bool operator==(const put_many_swap_leg_indexings_response&,
                           const put_many_swap_leg_indexings_response&) = default;
};

struct delete_swap_leg_indexing_request {
    using response_type = struct delete_swap_leg_indexing_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_indexing_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_swap_leg_indexing_request&,
                           const delete_swap_leg_indexing_request&) = default;
};

struct delete_swap_leg_indexing_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_swap_leg_indexing_response&,
                           const delete_swap_leg_indexing_response&) = default;
};

struct delete_many_swap_leg_indexings_request {
    using response_type = struct delete_many_swap_leg_indexings_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swap_leg_indexing_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_swap_leg_indexings_request&,
                           const delete_many_swap_leg_indexings_request&) = default;
};

struct delete_many_swap_leg_indexings_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_swap_leg_indexings_response&,
                           const delete_many_swap_leg_indexings_response&) = default;
};

struct list_by_trade_id_swap_leg_indexings_request {
    using response_type = struct list_by_trade_id_swap_leg_indexings_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.swap_leg_indexings.list_by_trade_id";
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
    std::optional<swap_leg_indexings_filter> filter;

    friend bool operator==(const list_by_trade_id_swap_leg_indexings_request&,
                           const list_by_trade_id_swap_leg_indexings_request&) = default;
};

struct list_by_trade_id_swap_leg_indexings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::swap_leg_indexing> swap_leg_indexings;
    std::uint64_t total;

    friend bool operator==(const list_by_trade_id_swap_leg_indexings_response&,
                           const list_by_trade_id_swap_leg_indexings_response&) = default;
};

struct list_swap_leg_indexing_versions_request {
    using response_type = struct list_swap_leg_indexing_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_indexing_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<swap_leg_indexing_versions_filter> filter;

    friend bool operator==(const list_swap_leg_indexing_versions_request&,
                           const list_swap_leg_indexing_versions_request&) = default;
};

struct list_swap_leg_indexing_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::swap_leg_indexing> versions;
    std::uint64_t total;

    friend bool operator==(const list_swap_leg_indexing_versions_response&,
                           const list_swap_leg_indexing_versions_response&) = default;
};

struct get_swap_leg_indexing_version_request {
    using response_type = struct get_swap_leg_indexing_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_leg_indexings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_indexing_version_key key;

    friend bool operator==(const get_swap_leg_indexing_version_request&,
                           const get_swap_leg_indexing_version_request&) = default;
};

struct get_swap_leg_indexing_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::swap_leg_indexing> version;

    friend bool operator==(const get_swap_leg_indexing_version_response&,
                           const get_swap_leg_indexing_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace swap_leg_indexing_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.swap_leg_indexings.created";
inline constexpr std::string_view updated = "trading.v1.events.swap_leg_indexings.updated";
inline constexpr std::string_view deleted = "trading.v1.events.swap_leg_indexings.deleted";
}
/**
 * @brief The members of swap_leg_indexing_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexing_key>) {
    return std::tuple{
        utility::codec::field{"trade_id", &swap_leg_indexing_key::trade_id},
        utility::codec::field{"leg_number", &swap_leg_indexing_key::leg_number},
        utility::codec::field{"sequence_number", &swap_leg_indexing_key::sequence_number},
    };
}

/**
 * @brief The members of swap_leg_indexing_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexing_write>) {
    return std::tuple{
        utility::codec::field{"trade_id", &swap_leg_indexing_write::trade_id},
        utility::codec::field{"leg_number", &swap_leg_indexing_write::leg_number},
        utility::codec::field{"sequence_number", &swap_leg_indexing_write::sequence_number},
        utility::codec::field{"trade_activity_id", &swap_leg_indexing_write::trade_activity_id},
        utility::codec::field{"index_name", &swap_leg_indexing_write::index_name},
        utility::codec::field{"quantity", &swap_leg_indexing_write::quantity},
        utility::codec::field{"initial_fixing", &swap_leg_indexing_write::initial_fixing},
        utility::codec::field{"fixing_days", &swap_leg_indexing_write::fixing_days},
        utility::codec::field{"fixing_calendar", &swap_leg_indexing_write::fixing_calendar},
        utility::codec::field{"fixing_convention", &swap_leg_indexing_write::fixing_convention},
        utility::codec::field{"is_in_arrears", &swap_leg_indexing_write::is_in_arrears},
    };
}

/**
 * @brief The members of swap_leg_indexing_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexing_change>) {
    return std::tuple{
        utility::codec::field{"write", &swap_leg_indexing_change::write},
        utility::codec::field{"precondition", &swap_leg_indexing_change::precondition},
    };
}

/**
 * @brief The members of swap_leg_indexing_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexing_removal>) {
    return std::tuple{
        utility::codec::field{"key", &swap_leg_indexing_removal::key},
        utility::codec::field{"precondition", &swap_leg_indexing_removal::precondition},
    };
}

/**
 * @brief The members of swap_leg_indexing_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexing_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &swap_leg_indexing_lookup::key},
        utility::codec::field{"swap_leg_indexing", &swap_leg_indexing_lookup::swap_leg_indexing},
    };
}

/**
 * @brief The members of swap_leg_indexings_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexings_filter>) {
    return std::tuple{
        utility::codec::field{"trade_id", &swap_leg_indexings_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &swap_leg_indexings_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of swap_leg_indexing_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexing_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &swap_leg_indexing_event::event_id},
        utility::codec::field{"key", &swap_leg_indexing_event::key},
        utility::codec::field{"action", &swap_leg_indexing_event::action},
        utility::codec::field{"version", &swap_leg_indexing_event::version},
        utility::codec::field{"occurred_at", &swap_leg_indexing_event::occurred_at},
        utility::codec::field{"correlation_id", &swap_leg_indexing_event::correlation_id},
    };
}

/**
 * @brief The members of swap_leg_indexing_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexing_version_key>) {
    return std::tuple{
        utility::codec::field{"swap_leg_indexing",
                              &swap_leg_indexing_version_key::swap_leg_indexing},
        utility::codec::field{"version", &swap_leg_indexing_version_key::version},
    };
}

/**
 * @brief The members of swap_leg_indexing_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_indexing_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &swap_leg_indexing_versions_filter::version},
        utility::codec::field{"from_version", &swap_leg_indexing_versions_filter::from_version},
        utility::codec::field{"to_version", &swap_leg_indexing_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_swap_leg_indexings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_swap_leg_indexings_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_swap_leg_indexings_request::offset},
        utility::codec::field{"limit", &list_swap_leg_indexings_request::limit},
        utility::codec::field{"order", &list_swap_leg_indexings_request::order},
        utility::codec::field{"filter", &list_swap_leg_indexings_request::filter},
        utility::codec::field{"as_of", &list_swap_leg_indexings_request::as_of},
    };
}

/**
 * @brief The members of list_swap_leg_indexings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_swap_leg_indexings_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_swap_leg_indexings_response::result},
        utility::codec::field{"swap_leg_indexings",
                              &list_swap_leg_indexings_response::swap_leg_indexings},
        utility::codec::field{"total", &list_swap_leg_indexings_response::total},
    };
}

/**
 * @brief The members of get_swap_leg_indexing_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_swap_leg_indexing_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_swap_leg_indexing_request::key},
    };
}

/**
 * @brief The members of get_swap_leg_indexing_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_swap_leg_indexing_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_swap_leg_indexing_response::result},
        utility::codec::field{"swap_leg_indexing",
                              &get_swap_leg_indexing_response::swap_leg_indexing},
    };
}

/**
 * @brief The members of get_many_swap_leg_indexings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_swap_leg_indexings_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_swap_leg_indexings_request::keys},
    };
}

/**
 * @brief The members of get_many_swap_leg_indexings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_swap_leg_indexings_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_swap_leg_indexings_response::result},
        utility::codec::field{"entries", &get_many_swap_leg_indexings_response::entries},
    };
}

/**
 * @brief The members of put_swap_leg_indexing_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_swap_leg_indexing_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_swap_leg_indexing_request::change},
        utility::codec::field{"intent", &put_swap_leg_indexing_request::intent},
    };
}

/**
 * @brief The members of put_swap_leg_indexing_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_swap_leg_indexing_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_swap_leg_indexing_response::result},
        utility::codec::field{"swap_leg_indexing",
                              &put_swap_leg_indexing_response::swap_leg_indexing},
    };
}

/**
 * @brief The members of put_many_swap_leg_indexings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_swap_leg_indexings_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_swap_leg_indexings_request::changes},
        utility::codec::field{"intent", &put_many_swap_leg_indexings_request::intent},
    };
}

/**
 * @brief The members of put_many_swap_leg_indexings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_swap_leg_indexings_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_swap_leg_indexings_response::result},
        utility::codec::field{"swap_leg_indexings",
                              &put_many_swap_leg_indexings_response::swap_leg_indexings},
    };
}

/**
 * @brief The members of delete_swap_leg_indexing_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_swap_leg_indexing_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_swap_leg_indexing_request::removal},
        utility::codec::field{"intent", &delete_swap_leg_indexing_request::intent},
    };
}

/**
 * @brief The members of delete_swap_leg_indexing_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_swap_leg_indexing_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_swap_leg_indexing_response::result},
    };
}

/**
 * @brief The members of delete_many_swap_leg_indexings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_swap_leg_indexings_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_swap_leg_indexings_request::removals},
        utility::codec::field{"intent", &delete_many_swap_leg_indexings_request::intent},
    };
}

/**
 * @brief The members of delete_many_swap_leg_indexings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_swap_leg_indexings_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_swap_leg_indexings_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_swap_leg_indexings_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_swap_leg_indexings_request>) {
    return std::tuple{
        utility::codec::field{"trade_id", &list_by_trade_id_swap_leg_indexings_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_swap_leg_indexings_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_swap_leg_indexings_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_swap_leg_indexings_request::limit},
        utility::codec::field{"order", &list_by_trade_id_swap_leg_indexings_request::order},
        utility::codec::field{"filter", &list_by_trade_id_swap_leg_indexings_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_swap_leg_indexings_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_swap_leg_indexings_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_trade_id_swap_leg_indexings_response::result},
        utility::codec::field{"swap_leg_indexings",
                              &list_by_trade_id_swap_leg_indexings_response::swap_leg_indexings},
        utility::codec::field{"total", &list_by_trade_id_swap_leg_indexings_response::total},
    };
}

/**
 * @brief The members of list_swap_leg_indexing_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_swap_leg_indexing_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_swap_leg_indexing_versions_request::key},
        utility::codec::field{"offset", &list_swap_leg_indexing_versions_request::offset},
        utility::codec::field{"limit", &list_swap_leg_indexing_versions_request::limit},
        utility::codec::field{"order", &list_swap_leg_indexing_versions_request::order},
        utility::codec::field{"filter", &list_swap_leg_indexing_versions_request::filter},
    };
}

/**
 * @brief The members of list_swap_leg_indexing_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_swap_leg_indexing_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_swap_leg_indexing_versions_response::result},
        utility::codec::field{"versions", &list_swap_leg_indexing_versions_response::versions},
        utility::codec::field{"total", &list_swap_leg_indexing_versions_response::total},
    };
}

/**
 * @brief The members of get_swap_leg_indexing_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_swap_leg_indexing_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_swap_leg_indexing_version_request::key},
    };
}

/**
 * @brief The members of get_swap_leg_indexing_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_swap_leg_indexing_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_swap_leg_indexing_version_response::result},
        utility::codec::field{"version", &get_swap_leg_indexing_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_indexing_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_indexing_key& v);
    static ores::trading::messaging::swap_leg_indexing_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_indexing_key& v);
    static ores::trading::messaging::swap_leg_indexing_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_indexing_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_indexing_write& v);
    static ores::trading::messaging::swap_leg_indexing_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_indexing_write& v);
    static ores::trading::messaging::swap_leg_indexing_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_indexing_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_indexing_change& v);
    static ores::trading::messaging::swap_leg_indexing_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_indexing_change& v);
    static ores::trading::messaging::swap_leg_indexing_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_indexing_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_indexing_removal& v);
    static ores::trading::messaging::swap_leg_indexing_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_indexing_removal& v);
    static ores::trading::messaging::swap_leg_indexing_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_indexing_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_indexing_lookup& v);
    static ores::trading::messaging::swap_leg_indexing_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_indexing_lookup& v);
    static ores::trading::messaging::swap_leg_indexing_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_indexings_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_indexings_filter& v);
    static ores::trading::messaging::swap_leg_indexings_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_indexings_filter& v);
    static ores::trading::messaging::swap_leg_indexings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_indexing_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_indexing_event& v);
    static ores::trading::messaging::swap_leg_indexing_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_indexing_event& v);
    static ores::trading::messaging::swap_leg_indexing_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::swap_leg_indexing_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_indexing_version_key& v);
    static ores::trading::messaging::swap_leg_indexing_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::swap_leg_indexing_version_key& v);
    static ores::trading::messaging::swap_leg_indexing_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::swap_leg_indexing_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::swap_leg_indexing_versions_filter& v);
    static ores::trading::messaging::swap_leg_indexing_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::swap_leg_indexing_versions_filter& v);
    static ores::trading::messaging::swap_leg_indexing_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_swap_leg_indexings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_swap_leg_indexings_request& v);
    static ores::trading::messaging::list_swap_leg_indexings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_swap_leg_indexings_request& v);
    static ores::trading::messaging::list_swap_leg_indexings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_swap_leg_indexings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_swap_leg_indexings_response& v);
    static ores::trading::messaging::list_swap_leg_indexings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_swap_leg_indexings_response& v);
    static ores::trading::messaging::list_swap_leg_indexings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_swap_leg_indexing_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_swap_leg_indexing_request& v);
    static ores::trading::messaging::get_swap_leg_indexing_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_swap_leg_indexing_request& v);
    static ores::trading::messaging::get_swap_leg_indexing_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_swap_leg_indexing_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_swap_leg_indexing_response& v);
    static ores::trading::messaging::get_swap_leg_indexing_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_swap_leg_indexing_response& v);
    static ores::trading::messaging::get_swap_leg_indexing_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_swap_leg_indexings_request& v);
    static ores::trading::messaging::get_many_swap_leg_indexings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_swap_leg_indexings_request& v);
    static ores::trading::messaging::get_many_swap_leg_indexings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_swap_leg_indexings_response& v);
    static ores::trading::messaging::get_many_swap_leg_indexings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_swap_leg_indexings_response& v);
    static ores::trading::messaging::get_many_swap_leg_indexings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_swap_leg_indexing_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_swap_leg_indexing_request& v);
    static ores::trading::messaging::put_swap_leg_indexing_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_swap_leg_indexing_request& v);
    static ores::trading::messaging::put_swap_leg_indexing_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_swap_leg_indexing_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_swap_leg_indexing_response& v);
    static ores::trading::messaging::put_swap_leg_indexing_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_swap_leg_indexing_response& v);
    static ores::trading::messaging::put_swap_leg_indexing_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_swap_leg_indexings_request& v);
    static ores::trading::messaging::put_many_swap_leg_indexings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_swap_leg_indexings_request& v);
    static ores::trading::messaging::put_many_swap_leg_indexings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_swap_leg_indexings_response& v);
    static ores::trading::messaging::put_many_swap_leg_indexings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_swap_leg_indexings_response& v);
    static ores::trading::messaging::put_many_swap_leg_indexings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_swap_leg_indexing_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_swap_leg_indexing_request& v);
    static ores::trading::messaging::delete_swap_leg_indexing_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_swap_leg_indexing_request& v);
    static ores::trading::messaging::delete_swap_leg_indexing_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_swap_leg_indexing_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_swap_leg_indexing_response& v);
    static ores::trading::messaging::delete_swap_leg_indexing_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_swap_leg_indexing_response& v);
    static ores::trading::messaging::delete_swap_leg_indexing_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_swap_leg_indexings_request& v);
    static ores::trading::messaging::delete_many_swap_leg_indexings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_swap_leg_indexings_request& v);
    static ores::trading::messaging::delete_many_swap_leg_indexings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_swap_leg_indexings_response& v);
    static ores::trading::messaging::delete_many_swap_leg_indexings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_swap_leg_indexings_response& v);
    static ores::trading::messaging::delete_many_swap_leg_indexings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request& v);
    static ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request& v);
    static ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response& v);
    static ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response& v);
    static ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_swap_leg_indexing_versions_request& v);
    static ores::trading::messaging::list_swap_leg_indexing_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_swap_leg_indexing_versions_request& v);
    static ores::trading::messaging::list_swap_leg_indexing_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_swap_leg_indexing_versions_response& v);
    static ores::trading::messaging::list_swap_leg_indexing_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_swap_leg_indexing_versions_response& v);
    static ores::trading::messaging::list_swap_leg_indexing_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_swap_leg_indexing_version_request& v);
    static ores::trading::messaging::get_swap_leg_indexing_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_swap_leg_indexing_version_request& v);
    static ores::trading::messaging::get_swap_leg_indexing_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_swap_leg_indexing_version_response& v);
    static ores::trading::messaging::get_swap_leg_indexing_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_swap_leg_indexing_version_response& v);
    static ores::trading::messaging::get_swap_leg_indexing_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_UNDERLYING_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_UNDERLYING_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_underlying_codec.hpp"
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

struct instrument_underlying_key {
    boost::uuids::uuid trade_id;
    std::string underlying_role;
    int sequence_number{};

    friend bool operator==(const instrument_underlying_key&,
                           const instrument_underlying_key&) = default;
};

struct instrument_underlying_write {
    boost::uuids::uuid trade_id;
    std::string underlying_role;
    int sequence_number{};
    boost::uuids::uuid trade_activity_id;
    std::string underlying_type;
    std::string name;
    std::optional<std::string> identifier_type;
    std::optional<std::string> currency;
    std::optional<std::string> exchange;
    std::optional<double> weight;
    std::optional<std::string> price_type;
    std::optional<int> future_month_offset;
    std::optional<int> delivery_roll_days;
    std::optional<std::string> delivery_roll_calendar;
    std::optional<std::string> future_expiry_date;
    std::optional<std::string> future_contract_month;
    std::optional<std::string> interpolation;
    std::optional<ores::utility::decimal::decimal> bid_ask_adjustment;

    friend bool operator==(const instrument_underlying_write&,
                           const instrument_underlying_write&) = default;
};

struct instrument_underlying_change {
    instrument_underlying_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const instrument_underlying_change&,
                           const instrument_underlying_change&) = default;
};

struct instrument_underlying_removal {
    instrument_underlying_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const instrument_underlying_removal&,
                           const instrument_underlying_removal&) = default;
};

struct instrument_underlying_lookup {
    instrument_underlying_key key;
    std::optional<ores::trading::domain::instrument_underlying> instrument_underlying;

    friend bool operator==(const instrument_underlying_lookup&,
                           const instrument_underlying_lookup&) = default;
};

struct instrument_underlyings_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const instrument_underlyings_filter&,
                           const instrument_underlyings_filter&) = default;
};

struct instrument_underlying_event {
    boost::uuids::uuid event_id;
    instrument_underlying_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const instrument_underlying_event&,
                           const instrument_underlying_event&) = default;
};

struct instrument_underlying_version_key {
    instrument_underlying_key instrument_underlying;
    std::uint32_t version{};

    friend bool operator==(const instrument_underlying_version_key&,
                           const instrument_underlying_version_key&) = default;
};

struct instrument_underlying_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const instrument_underlying_versions_filter&,
                           const instrument_underlying_versions_filter&) = default;
};

struct list_instrument_underlyings_request {
    using response_type = struct list_instrument_underlyings_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_underlyings.list";
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
    std::optional<instrument_underlyings_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_instrument_underlyings_request&,
                           const list_instrument_underlyings_request&) = default;
};

struct list_instrument_underlyings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_underlying> instrument_underlyings;
    std::uint64_t total{};

    friend bool operator==(const list_instrument_underlyings_response&,
                           const list_instrument_underlyings_response&) = default;
};

struct get_instrument_underlying_request {
    using response_type = struct get_instrument_underlying_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_underlyings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_underlying_key key;

    friend bool operator==(const get_instrument_underlying_request&,
                           const get_instrument_underlying_request&) = default;
};

struct get_instrument_underlying_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_underlying> instrument_underlying;

    friend bool operator==(const get_instrument_underlying_response&,
                           const get_instrument_underlying_response&) = default;
};

struct get_many_instrument_underlyings_request {
    using response_type = struct get_many_instrument_underlyings_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_underlyings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_underlying_key> keys;

    friend bool operator==(const get_many_instrument_underlyings_request&,
                           const get_many_instrument_underlyings_request&) = default;
};

struct get_many_instrument_underlyings_response {
    ores::utility::domain::result result;
    std::vector<instrument_underlying_lookup> entries;

    friend bool operator==(const get_many_instrument_underlyings_response&,
                           const get_many_instrument_underlyings_response&) = default;
};

struct put_instrument_underlying_request {
    using response_type = struct put_instrument_underlying_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_underlyings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_underlying_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_instrument_underlying_request&,
                           const put_instrument_underlying_request&) = default;
};

struct put_instrument_underlying_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_underlying> instrument_underlying;

    friend bool operator==(const put_instrument_underlying_response&,
                           const put_instrument_underlying_response&) = default;
};

struct put_many_instrument_underlyings_request {
    using response_type = struct put_many_instrument_underlyings_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_underlyings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_underlying_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_instrument_underlyings_request&,
                           const put_many_instrument_underlyings_request&) = default;
};

struct put_many_instrument_underlyings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_underlying> instrument_underlyings;

    friend bool operator==(const put_many_instrument_underlyings_response&,
                           const put_many_instrument_underlyings_response&) = default;
};

struct delete_instrument_underlying_request {
    using response_type = struct delete_instrument_underlying_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_underlyings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_underlying_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_instrument_underlying_request&,
                           const delete_instrument_underlying_request&) = default;
};

struct delete_instrument_underlying_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_instrument_underlying_response&,
                           const delete_instrument_underlying_response&) = default;
};

struct delete_many_instrument_underlyings_request {
    using response_type = struct delete_many_instrument_underlyings_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_underlyings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_underlying_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_instrument_underlyings_request&,
                           const delete_many_instrument_underlyings_request&) = default;
};

struct delete_many_instrument_underlyings_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_instrument_underlyings_response&,
                           const delete_many_instrument_underlyings_response&) = default;
};

struct list_by_trade_id_instrument_underlyings_request {
    using response_type = struct list_by_trade_id_instrument_underlyings_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_underlyings.list_by_trade_id";
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
    std::optional<instrument_underlyings_filter> filter;

    friend bool operator==(const list_by_trade_id_instrument_underlyings_request&,
                           const list_by_trade_id_instrument_underlyings_request&) = default;
};

struct list_by_trade_id_instrument_underlyings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_underlying> instrument_underlyings;
    std::uint64_t total{};

    friend bool operator==(const list_by_trade_id_instrument_underlyings_response&,
                           const list_by_trade_id_instrument_underlyings_response&) = default;
};

struct list_instrument_underlying_versions_request {
    using response_type = struct list_instrument_underlying_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_underlyings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_underlying_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_underlying_versions_filter> filter;

    friend bool operator==(const list_instrument_underlying_versions_request&,
                           const list_instrument_underlying_versions_request&) = default;
};

struct list_instrument_underlying_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_underlying> versions;
    std::uint64_t total{};

    friend bool operator==(const list_instrument_underlying_versions_response&,
                           const list_instrument_underlying_versions_response&) = default;
};

struct get_instrument_underlying_version_request {
    using response_type = struct get_instrument_underlying_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_underlyings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_underlying_version_key key;

    friend bool operator==(const get_instrument_underlying_version_request&,
                           const get_instrument_underlying_version_request&) = default;
};

struct get_instrument_underlying_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_underlying> version;

    friend bool operator==(const get_instrument_underlying_version_response&,
                           const get_instrument_underlying_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace instrument_underlying_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.instrument_underlyings.created";
inline constexpr std::string_view updated = "trading.v1.events.instrument_underlyings.updated";
inline constexpr std::string_view deleted = "trading.v1.events.instrument_underlyings.deleted";
}
/**
 * @brief The members of instrument_underlying_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlying_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &instrument_underlying_key::trade_id},
        utility::codec::field{"underlying_role", &instrument_underlying_key::underlying_role},
        utility::codec::field{"sequence_number", &instrument_underlying_key::sequence_number},
    };
}

/**
 * @brief The members of instrument_underlying_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlying_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &instrument_underlying_write::trade_id},
        utility::codec::field{"underlying_role", &instrument_underlying_write::underlying_role},
        utility::codec::field{"sequence_number", &instrument_underlying_write::sequence_number},
        utility::codec::field{"trade_activity_id", &instrument_underlying_write::trade_activity_id},
        utility::codec::field{"underlying_type", &instrument_underlying_write::underlying_type},
        utility::codec::field{"name", &instrument_underlying_write::name},
        utility::codec::field{"identifier_type", &instrument_underlying_write::identifier_type},
        utility::codec::field{"currency", &instrument_underlying_write::currency},
        utility::codec::field{"exchange", &instrument_underlying_write::exchange},
        utility::codec::field{"weight", &instrument_underlying_write::weight},
        utility::codec::field{"price_type", &instrument_underlying_write::price_type},
        utility::codec::field{"future_month_offset",
                              &instrument_underlying_write::future_month_offset},
        utility::codec::field{"delivery_roll_days",
                              &instrument_underlying_write::delivery_roll_days},
        utility::codec::field{"delivery_roll_calendar",
                              &instrument_underlying_write::delivery_roll_calendar},
        utility::codec::field{"future_expiry_date",
                              &instrument_underlying_write::future_expiry_date},
        utility::codec::field{"future_contract_month",
                              &instrument_underlying_write::future_contract_month},
        utility::codec::field{"interpolation", &instrument_underlying_write::interpolation},
        utility::codec::field{"bid_ask_adjustment",
                              &instrument_underlying_write::bid_ask_adjustment},
    };
}

/**
 * @brief The members of instrument_underlying_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlying_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &instrument_underlying_change::write},
        utility::codec::field{"precondition", &instrument_underlying_change::precondition},
    };
}

/**
 * @brief The members of instrument_underlying_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlying_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &instrument_underlying_removal::key},
        utility::codec::field{"precondition", &instrument_underlying_removal::precondition},
    };
}

/**
 * @brief The members of instrument_underlying_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlying_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &instrument_underlying_lookup::key},
        utility::codec::field{"instrument_underlying",
                              &instrument_underlying_lookup::instrument_underlying},
    };
}

/**
 * @brief The members of instrument_underlyings_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlyings_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &instrument_underlyings_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &instrument_underlyings_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of instrument_underlying_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlying_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &instrument_underlying_event::event_id},
        utility::codec::field{"key", &instrument_underlying_event::key},
        utility::codec::field{"action", &instrument_underlying_event::action},
        utility::codec::field{"version", &instrument_underlying_event::version},
        utility::codec::field{"occurred_at", &instrument_underlying_event::occurred_at},
        utility::codec::field{"correlation_id", &instrument_underlying_event::correlation_id},
    };
}

/**
 * @brief The members of instrument_underlying_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlying_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"instrument_underlying",
                              &instrument_underlying_version_key::instrument_underlying},
        utility::codec::field{"version", &instrument_underlying_version_key::version},
    };
}

/**
 * @brief The members of instrument_underlying_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<instrument_underlying_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &instrument_underlying_versions_filter::version},
        utility::codec::field{"from_version", &instrument_underlying_versions_filter::from_version},
        utility::codec::field{"to_version", &instrument_underlying_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_instrument_underlyings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_instrument_underlyings_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_instrument_underlyings_request::offset},
        utility::codec::field{"limit", &list_instrument_underlyings_request::limit},
        utility::codec::field{"order", &list_instrument_underlyings_request::order},
        utility::codec::field{"filter", &list_instrument_underlyings_request::filter},
        utility::codec::field{"as_of", &list_instrument_underlyings_request::as_of},
    };
}

/**
 * @brief The members of list_instrument_underlyings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_instrument_underlyings_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_instrument_underlyings_response::result},
        utility::codec::field{"instrument_underlyings",
                              &list_instrument_underlyings_response::instrument_underlyings},
        utility::codec::field{"total", &list_instrument_underlyings_response::total},
    };
}

/**
 * @brief The members of get_instrument_underlying_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_instrument_underlying_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_instrument_underlying_request::key},
    };
}

/**
 * @brief The members of get_instrument_underlying_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_instrument_underlying_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_instrument_underlying_response::result},
        utility::codec::field{"instrument_underlying",
                              &get_instrument_underlying_response::instrument_underlying},
    };
}

/**
 * @brief The members of get_many_instrument_underlyings_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_instrument_underlyings_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_instrument_underlyings_request::keys},
    };
}

/**
 * @brief The members of get_many_instrument_underlyings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_instrument_underlyings_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_instrument_underlyings_response::result},
        utility::codec::field{"entries", &get_many_instrument_underlyings_response::entries},
    };
}

/**
 * @brief The members of put_instrument_underlying_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_instrument_underlying_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_instrument_underlying_request::change},
        utility::codec::field{"intent", &put_instrument_underlying_request::intent},
    };
}

/**
 * @brief The members of put_instrument_underlying_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_instrument_underlying_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_instrument_underlying_response::result},
        utility::codec::field{"instrument_underlying",
                              &put_instrument_underlying_response::instrument_underlying},
    };
}

/**
 * @brief The members of put_many_instrument_underlyings_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_instrument_underlyings_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_instrument_underlyings_request::changes},
        utility::codec::field{"intent", &put_many_instrument_underlyings_request::intent},
    };
}

/**
 * @brief The members of put_many_instrument_underlyings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_instrument_underlyings_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_instrument_underlyings_response::result},
        utility::codec::field{"instrument_underlyings",
                              &put_many_instrument_underlyings_response::instrument_underlyings},
    };
}

/**
 * @brief The members of delete_instrument_underlying_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_instrument_underlying_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_instrument_underlying_request::removal},
        utility::codec::field{"intent", &delete_instrument_underlying_request::intent},
    };
}

/**
 * @brief The members of delete_instrument_underlying_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_instrument_underlying_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_instrument_underlying_response::result},
    };
}

/**
 * @brief The members of delete_many_instrument_underlyings_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_instrument_underlyings_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_instrument_underlyings_request::removals},
        utility::codec::field{"intent", &delete_many_instrument_underlyings_request::intent},
    };
}

/**
 * @brief The members of delete_many_instrument_underlyings_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_instrument_underlyings_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_instrument_underlyings_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_instrument_underlyings_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_instrument_underlyings_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id",
                              &list_by_trade_id_instrument_underlyings_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_instrument_underlyings_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_instrument_underlyings_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_instrument_underlyings_request::limit},
        utility::codec::field{"order", &list_by_trade_id_instrument_underlyings_request::order},
        utility::codec::field{"filter", &list_by_trade_id_instrument_underlyings_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_instrument_underlyings_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_instrument_underlyings_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_by_trade_id_instrument_underlyings_response::result},
        utility::codec::field{
            "instrument_underlyings",
            &list_by_trade_id_instrument_underlyings_response::instrument_underlyings},
        utility::codec::field{"total", &list_by_trade_id_instrument_underlyings_response::total},
    };
}

/**
 * @brief The members of list_instrument_underlying_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_underlying_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_instrument_underlying_versions_request::key},
        utility::codec::field{"offset", &list_instrument_underlying_versions_request::offset},
        utility::codec::field{"limit", &list_instrument_underlying_versions_request::limit},
        utility::codec::field{"order", &list_instrument_underlying_versions_request::order},
        utility::codec::field{"filter", &list_instrument_underlying_versions_request::filter},
    };
}

/**
 * @brief The members of list_instrument_underlying_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_underlying_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_instrument_underlying_versions_response::result},
        utility::codec::field{"versions", &list_instrument_underlying_versions_response::versions},
        utility::codec::field{"total", &list_instrument_underlying_versions_response::total},
    };
}

/**
 * @brief The members of get_instrument_underlying_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_underlying_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_instrument_underlying_version_request::key},
    };
}

/**
 * @brief The members of get_instrument_underlying_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_underlying_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_instrument_underlying_version_response::result},
        utility::codec::field{"version", &get_instrument_underlying_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_underlying_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_underlying_key& v);
    static ores::trading::messaging::instrument_underlying_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_underlying_key& v);
    static ores::trading::messaging::instrument_underlying_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_underlying_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_underlying_write& v);
    static ores::trading::messaging::instrument_underlying_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_underlying_write& v);
    static ores::trading::messaging::instrument_underlying_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_underlying_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_underlying_change& v);
    static ores::trading::messaging::instrument_underlying_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_underlying_change& v);
    static ores::trading::messaging::instrument_underlying_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_underlying_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_underlying_removal& v);
    static ores::trading::messaging::instrument_underlying_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_underlying_removal& v);
    static ores::trading::messaging::instrument_underlying_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_underlying_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_underlying_lookup& v);
    static ores::trading::messaging::instrument_underlying_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_underlying_lookup& v);
    static ores::trading::messaging::instrument_underlying_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_underlyings_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_underlyings_filter& v);
    static ores::trading::messaging::instrument_underlyings_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_underlyings_filter& v);
    static ores::trading::messaging::instrument_underlyings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_underlying_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_underlying_event& v);
    static ores::trading::messaging::instrument_underlying_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_underlying_event& v);
    static ores::trading::messaging::instrument_underlying_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_underlying_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_underlying_version_key& v);
    static ores::trading::messaging::instrument_underlying_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_underlying_version_key& v);
    static ores::trading::messaging::instrument_underlying_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_underlying_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_underlying_versions_filter& v);
    static ores::trading::messaging::instrument_underlying_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_underlying_versions_filter& v);
    static ores::trading::messaging::instrument_underlying_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_underlyings_request& v);
    static ores::trading::messaging::list_instrument_underlyings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_underlyings_request& v);
    static ores::trading::messaging::list_instrument_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_underlyings_response& v);
    static ores::trading::messaging::list_instrument_underlyings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_underlyings_response& v);
    static ores::trading::messaging::list_instrument_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_underlying_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_underlying_request& v);
    static ores::trading::messaging::get_instrument_underlying_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_underlying_request& v);
    static ores::trading::messaging::get_instrument_underlying_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_underlying_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_underlying_response& v);
    static ores::trading::messaging::get_instrument_underlying_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_underlying_response& v);
    static ores::trading::messaging::get_instrument_underlying_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_underlyings_request& v);
    static ores::trading::messaging::get_many_instrument_underlyings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_underlyings_request& v);
    static ores::trading::messaging::get_many_instrument_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_underlyings_response& v);
    static ores::trading::messaging::get_many_instrument_underlyings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_underlyings_response& v);
    static ores::trading::messaging::get_many_instrument_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_underlying_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_underlying_request& v);
    static ores::trading::messaging::put_instrument_underlying_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_underlying_request& v);
    static ores::trading::messaging::put_instrument_underlying_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_underlying_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_underlying_response& v);
    static ores::trading::messaging::put_instrument_underlying_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_underlying_response& v);
    static ores::trading::messaging::put_instrument_underlying_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_underlyings_request& v);
    static ores::trading::messaging::put_many_instrument_underlyings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_underlyings_request& v);
    static ores::trading::messaging::put_many_instrument_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_underlyings_response& v);
    static ores::trading::messaging::put_many_instrument_underlyings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_underlyings_response& v);
    static ores::trading::messaging::put_many_instrument_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_underlying_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_underlying_request& v);
    static ores::trading::messaging::delete_instrument_underlying_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_underlying_request& v);
    static ores::trading::messaging::delete_instrument_underlying_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_underlying_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_underlying_response& v);
    static ores::trading::messaging::delete_instrument_underlying_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_underlying_response& v);
    static ores::trading::messaging::delete_instrument_underlying_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_underlyings_request& v);
    static ores::trading::messaging::delete_many_instrument_underlyings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_underlyings_request& v);
    static ores::trading::messaging::delete_many_instrument_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_underlyings_response& v);
    static ores::trading::messaging::delete_many_instrument_underlyings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_underlyings_response& v);
    static ores::trading::messaging::delete_many_instrument_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_instrument_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_instrument_underlyings_request& v);
    static ores::trading::messaging::list_by_trade_id_instrument_underlyings_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_underlyings_request& v);
    static ores::trading::messaging::list_by_trade_id_instrument_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_instrument_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_instrument_underlyings_response& v);
    static ores::trading::messaging::list_by_trade_id_instrument_underlyings_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_underlyings_response& v);
    static ores::trading::messaging::list_by_trade_id_instrument_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_underlying_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_underlying_versions_request& v);
    static ores::trading::messaging::list_instrument_underlying_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_underlying_versions_request& v);
    static ores::trading::messaging::list_instrument_underlying_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_underlying_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_underlying_versions_response& v);
    static ores::trading::messaging::list_instrument_underlying_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_underlying_versions_response& v);
    static ores::trading::messaging::list_instrument_underlying_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_underlying_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_underlying_version_request& v);
    static ores::trading::messaging::get_instrument_underlying_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_underlying_version_request& v);
    static ores::trading::messaging::get_instrument_underlying_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_underlying_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_underlying_version_response& v);
    static ores::trading::messaging::get_instrument_underlying_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_underlying_version_response& v);
    static ores::trading::messaging::get_instrument_underlying_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

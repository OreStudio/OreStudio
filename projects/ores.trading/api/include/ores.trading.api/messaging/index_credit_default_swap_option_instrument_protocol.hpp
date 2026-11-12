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
#ifndef ORES_TRADING_API_MESSAGING_INDEX_CREDIT_DEFAULT_SWAP_OPTION_INSTRUMENT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INDEX_CREDIT_DEFAULT_SWAP_OPTION_INSTRUMENT_PROTOCOL_HPP

#include "ores.trading.api/domain/index_credit_default_swap_option_instrument_codec.hpp"
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

struct index_credit_default_swap_option_instrument_key {
    boost::uuids::uuid trade_id;

    friend bool operator==(const index_credit_default_swap_option_instrument_key&,
                           const index_credit_default_swap_option_instrument_key&) = default;
};

struct index_credit_default_swap_option_instrument_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    std::optional<bool> knock_out;
    std::optional<std::string> index_term;
    std::optional<std::string> strike_type;
    std::optional<std::chrono::year_month_day> trade_date;
    std::optional<std::chrono::year_month_day> front_end_protection_start_date;

    friend bool operator==(const index_credit_default_swap_option_instrument_write&,
                           const index_credit_default_swap_option_instrument_write&) = default;
};

struct index_credit_default_swap_option_instrument_change {
    index_credit_default_swap_option_instrument_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const index_credit_default_swap_option_instrument_change&,
                           const index_credit_default_swap_option_instrument_change&) = default;
};

struct index_credit_default_swap_option_instrument_removal {
    index_credit_default_swap_option_instrument_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const index_credit_default_swap_option_instrument_removal&,
                           const index_credit_default_swap_option_instrument_removal&) = default;
};

struct index_credit_default_swap_option_instrument_lookup {
    index_credit_default_swap_option_instrument_key key;
    std::optional<ores::trading::domain::index_credit_default_swap_option_instrument>
        index_credit_default_swap_option_instrument;

    friend bool operator==(const index_credit_default_swap_option_instrument_lookup&,
                           const index_credit_default_swap_option_instrument_lookup&) = default;
};

struct index_credit_default_swap_option_instruments_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const index_credit_default_swap_option_instruments_filter&,
                           const index_credit_default_swap_option_instruments_filter&) = default;
};

struct index_credit_default_swap_option_instrument_event {
    boost::uuids::uuid event_id;
    index_credit_default_swap_option_instrument_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const index_credit_default_swap_option_instrument_event&,
                           const index_credit_default_swap_option_instrument_event&) = default;
};

struct index_credit_default_swap_option_instrument_version_key {
    index_credit_default_swap_option_instrument_key index_credit_default_swap_option_instrument;
    std::uint32_t version{};

    friend bool
    operator==(const index_credit_default_swap_option_instrument_version_key&,
               const index_credit_default_swap_option_instrument_version_key&) = default;
};

struct index_credit_default_swap_option_instrument_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool
    operator==(const index_credit_default_swap_option_instrument_versions_filter&,
               const index_credit_default_swap_option_instrument_versions_filter&) = default;
};

struct list_index_credit_default_swap_option_instruments_request {
    using response_type = struct list_index_credit_default_swap_option_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments.list";
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
    std::optional<index_credit_default_swap_option_instruments_filter> filter;
    std::optional<std::string> as_of;

    friend bool
    operator==(const list_index_credit_default_swap_option_instruments_request&,
               const list_index_credit_default_swap_option_instruments_request&) = default;
};

struct list_index_credit_default_swap_option_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::index_credit_default_swap_option_instrument>
        index_cds_option_instruments;
    std::uint64_t total{};

    friend bool
    operator==(const list_index_credit_default_swap_option_instruments_response&,
               const list_index_credit_default_swap_option_instruments_response&) = default;
};

struct get_index_credit_default_swap_option_instrument_request {
    using response_type = struct get_index_credit_default_swap_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    index_credit_default_swap_option_instrument_key key;

    friend bool
    operator==(const get_index_credit_default_swap_option_instrument_request&,
               const get_index_credit_default_swap_option_instrument_request&) = default;
};

struct get_index_credit_default_swap_option_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::index_credit_default_swap_option_instrument>
        index_credit_default_swap_option_instrument;

    friend bool
    operator==(const get_index_credit_default_swap_option_instrument_response&,
               const get_index_credit_default_swap_option_instrument_response&) = default;
};

struct get_many_index_credit_default_swap_option_instruments_request {
    using response_type = struct get_many_index_credit_default_swap_option_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<index_credit_default_swap_option_instrument_key> keys;

    friend bool
    operator==(const get_many_index_credit_default_swap_option_instruments_request&,
               const get_many_index_credit_default_swap_option_instruments_request&) = default;
};

struct get_many_index_credit_default_swap_option_instruments_response {
    ores::utility::domain::result result;
    std::vector<index_credit_default_swap_option_instrument_lookup> entries;

    friend bool
    operator==(const get_many_index_credit_default_swap_option_instruments_response&,
               const get_many_index_credit_default_swap_option_instruments_response&) = default;
};

struct put_index_credit_default_swap_option_instrument_request {
    using response_type = struct put_index_credit_default_swap_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    index_credit_default_swap_option_instrument_change change;
    ores::utility::domain::change_intent intent;

    friend bool
    operator==(const put_index_credit_default_swap_option_instrument_request&,
               const put_index_credit_default_swap_option_instrument_request&) = default;
};

struct put_index_credit_default_swap_option_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::index_credit_default_swap_option_instrument>
        index_credit_default_swap_option_instrument;

    friend bool
    operator==(const put_index_credit_default_swap_option_instrument_response&,
               const put_index_credit_default_swap_option_instrument_response&) = default;
};

struct put_many_index_credit_default_swap_option_instruments_request {
    using response_type = struct put_many_index_credit_default_swap_option_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<index_credit_default_swap_option_instrument_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool
    operator==(const put_many_index_credit_default_swap_option_instruments_request&,
               const put_many_index_credit_default_swap_option_instruments_request&) = default;
};

struct put_many_index_credit_default_swap_option_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::index_credit_default_swap_option_instrument>
        index_cds_option_instruments;

    friend bool
    operator==(const put_many_index_credit_default_swap_option_instruments_response&,
               const put_many_index_credit_default_swap_option_instruments_response&) = default;
};

struct delete_index_credit_default_swap_option_instrument_request {
    using response_type = struct delete_index_credit_default_swap_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    index_credit_default_swap_option_instrument_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool
    operator==(const delete_index_credit_default_swap_option_instrument_request&,
               const delete_index_credit_default_swap_option_instrument_request&) = default;
};

struct delete_index_credit_default_swap_option_instrument_response {
    ores::utility::domain::result result;

    friend bool
    operator==(const delete_index_credit_default_swap_option_instrument_response&,
               const delete_index_credit_default_swap_option_instrument_response&) = default;
};

struct delete_many_index_credit_default_swap_option_instruments_request {
    using response_type = struct delete_many_index_credit_default_swap_option_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<index_credit_default_swap_option_instrument_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool
    operator==(const delete_many_index_credit_default_swap_option_instruments_request&,
               const delete_many_index_credit_default_swap_option_instruments_request&) = default;
};

struct delete_many_index_credit_default_swap_option_instruments_response {
    ores::utility::domain::result result;

    friend bool
    operator==(const delete_many_index_credit_default_swap_option_instruments_response&,
               const delete_many_index_credit_default_swap_option_instruments_response&) = default;
};

struct list_index_credit_default_swap_option_instrument_versions_request {
    using response_type = struct list_index_credit_default_swap_option_instrument_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    index_credit_default_swap_option_instrument_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<index_credit_default_swap_option_instrument_versions_filter> filter;

    friend bool
    operator==(const list_index_credit_default_swap_option_instrument_versions_request&,
               const list_index_credit_default_swap_option_instrument_versions_request&) = default;
};

struct list_index_credit_default_swap_option_instrument_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::index_credit_default_swap_option_instrument> versions;
    std::uint64_t total{};

    friend bool
    operator==(const list_index_credit_default_swap_option_instrument_versions_response&,
               const list_index_credit_default_swap_option_instrument_versions_response&) = default;
};

struct get_index_credit_default_swap_option_instrument_version_request {
    using response_type = struct get_index_credit_default_swap_option_instrument_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.index_credit_default_swap_option_instruments_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    index_credit_default_swap_option_instrument_version_key key;

    friend bool
    operator==(const get_index_credit_default_swap_option_instrument_version_request&,
               const get_index_credit_default_swap_option_instrument_version_request&) = default;
};

struct get_index_credit_default_swap_option_instrument_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::index_credit_default_swap_option_instrument> version;

    friend bool
    operator==(const get_index_credit_default_swap_option_instrument_version_response&,
               const get_index_credit_default_swap_option_instrument_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace index_credit_default_swap_option_instrument_event_subjects {
inline constexpr std::string_view created =
    "trading.v1.events.index_credit_default_swap_option_instruments.created";
inline constexpr std::string_view updated =
    "trading.v1.events.index_credit_default_swap_option_instruments.updated";
inline constexpr std::string_view deleted =
    "trading.v1.events.index_credit_default_swap_option_instruments.deleted";
}
/**
 * @brief The members of index_credit_default_swap_option_instrument_key, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instrument_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id",
                              &index_credit_default_swap_option_instrument_key::trade_id},
    };
}

/**
 * @brief The members of index_credit_default_swap_option_instrument_write, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instrument_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id",
                              &index_credit_default_swap_option_instrument_write::trade_id},
        utility::codec::field{
            "trade_activity_id",
            &index_credit_default_swap_option_instrument_write::trade_activity_id},
        utility::codec::field{"knock_out",
                              &index_credit_default_swap_option_instrument_write::knock_out},
        utility::codec::field{"index_term",
                              &index_credit_default_swap_option_instrument_write::index_term},
        utility::codec::field{"strike_type",
                              &index_credit_default_swap_option_instrument_write::strike_type},
        utility::codec::field{"trade_date",
                              &index_credit_default_swap_option_instrument_write::trade_date},
        utility::codec::field{
            "front_end_protection_start_date",
            &index_credit_default_swap_option_instrument_write::front_end_protection_start_date},
    };
}

/**
 * @brief The members of index_credit_default_swap_option_instrument_change, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instrument_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &index_credit_default_swap_option_instrument_change::write},
        utility::codec::field{"precondition",
                              &index_credit_default_swap_option_instrument_change::precondition},
    };
}

/**
 * @brief The members of index_credit_default_swap_option_instrument_removal, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instrument_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &index_credit_default_swap_option_instrument_removal::key},
        utility::codec::field{"precondition",
                              &index_credit_default_swap_option_instrument_removal::precondition},
    };
}

/**
 * @brief The members of index_credit_default_swap_option_instrument_lookup, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instrument_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &index_credit_default_swap_option_instrument_lookup::key},
        utility::codec::field{"index_credit_default_swap_option_instrument",
                              &index_credit_default_swap_option_instrument_lookup::
                                  index_credit_default_swap_option_instrument},
    };
}

/**
 * @brief The members of index_credit_default_swap_option_instruments_filter, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instruments_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "trade_id_one_of",
            &index_credit_default_swap_option_instruments_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of index_credit_default_swap_option_instrument_event, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instrument_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id",
                              &index_credit_default_swap_option_instrument_event::event_id},
        utility::codec::field{"key", &index_credit_default_swap_option_instrument_event::key},
        utility::codec::field{"action", &index_credit_default_swap_option_instrument_event::action},
        utility::codec::field{"version",
                              &index_credit_default_swap_option_instrument_event::version},
        utility::codec::field{"occurred_at",
                              &index_credit_default_swap_option_instrument_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &index_credit_default_swap_option_instrument_event::correlation_id},
    };
}

/**
 * @brief The members of index_credit_default_swap_option_instrument_version_key, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instrument_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"index_credit_default_swap_option_instrument",
                              &index_credit_default_swap_option_instrument_version_key::
                                  index_credit_default_swap_option_instrument},
        utility::codec::field{"version",
                              &index_credit_default_swap_option_instrument_version_key::version},
    };
}

/**
 * @brief The members of index_credit_default_swap_option_instrument_versions_filter, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<index_credit_default_swap_option_instrument_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "version", &index_credit_default_swap_option_instrument_versions_filter::version},
        utility::codec::field{
            "from_version",
            &index_credit_default_swap_option_instrument_versions_filter::from_version},
        utility::codec::field{
            "to_version", &index_credit_default_swap_option_instrument_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_index_credit_default_swap_option_instruments_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_index_credit_default_swap_option_instruments_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset",
                              &list_index_credit_default_swap_option_instruments_request::offset},
        utility::codec::field{"limit",
                              &list_index_credit_default_swap_option_instruments_request::limit},
        utility::codec::field{"order",
                              &list_index_credit_default_swap_option_instruments_request::order},
        utility::codec::field{"filter",
                              &list_index_credit_default_swap_option_instruments_request::filter},
        utility::codec::field{"as_of",
                              &list_index_credit_default_swap_option_instruments_request::as_of},
    };
}

/**
 * @brief The members of list_index_credit_default_swap_option_instruments_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_index_credit_default_swap_option_instruments_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_index_credit_default_swap_option_instruments_response::result},
        utility::codec::field{"index_cds_option_instruments",
                              &list_index_credit_default_swap_option_instruments_response::
                                  index_cds_option_instruments},
        utility::codec::field{"total",
                              &list_index_credit_default_swap_option_instruments_response::total},
    };
}

/**
 * @brief The members of get_index_credit_default_swap_option_instrument_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_index_credit_default_swap_option_instrument_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_index_credit_default_swap_option_instrument_request::key},
    };
}

/**
 * @brief The members of get_index_credit_default_swap_option_instrument_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_index_credit_default_swap_option_instrument_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &get_index_credit_default_swap_option_instrument_response::result},
        utility::codec::field{"index_credit_default_swap_option_instrument",
                              &get_index_credit_default_swap_option_instrument_response::
                                  index_credit_default_swap_option_instrument},
    };
}

/**
 * @brief The members of get_many_index_credit_default_swap_option_instruments_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_index_credit_default_swap_option_instruments_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys",
                              &get_many_index_credit_default_swap_option_instruments_request::keys},
    };
}

/**
 * @brief The members of get_many_index_credit_default_swap_option_instruments_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_index_credit_default_swap_option_instruments_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "result", &get_many_index_credit_default_swap_option_instruments_response::result},
        utility::codec::field{
            "entries", &get_many_index_credit_default_swap_option_instruments_response::entries},
    };
}

/**
 * @brief The members of put_index_credit_default_swap_option_instrument_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_index_credit_default_swap_option_instrument_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change",
                              &put_index_credit_default_swap_option_instrument_request::change},
        utility::codec::field{"intent",
                              &put_index_credit_default_swap_option_instrument_request::intent},
    };
}

/**
 * @brief The members of put_index_credit_default_swap_option_instrument_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_index_credit_default_swap_option_instrument_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &put_index_credit_default_swap_option_instrument_response::result},
        utility::codec::field{"index_credit_default_swap_option_instrument",
                              &put_index_credit_default_swap_option_instrument_response::
                                  index_credit_default_swap_option_instrument},
    };
}

/**
 * @brief The members of put_many_index_credit_default_swap_option_instruments_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_index_credit_default_swap_option_instruments_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "changes", &put_many_index_credit_default_swap_option_instruments_request::changes},
        utility::codec::field{
            "intent", &put_many_index_credit_default_swap_option_instruments_request::intent},
    };
}

/**
 * @brief The members of put_many_index_credit_default_swap_option_instruments_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_index_credit_default_swap_option_instruments_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "result", &put_many_index_credit_default_swap_option_instruments_response::result},
        utility::codec::field{"index_cds_option_instruments",
                              &put_many_index_credit_default_swap_option_instruments_response::
                                  index_cds_option_instruments},
    };
}

/**
 * @brief The members of delete_index_credit_default_swap_option_instrument_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_index_credit_default_swap_option_instrument_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal",
                              &delete_index_credit_default_swap_option_instrument_request::removal},
        utility::codec::field{"intent",
                              &delete_index_credit_default_swap_option_instrument_request::intent},
    };
}

/**
 * @brief The members of delete_index_credit_default_swap_option_instrument_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_index_credit_default_swap_option_instrument_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &delete_index_credit_default_swap_option_instrument_response::result},
    };
}

/**
 * @brief The members of delete_many_index_credit_default_swap_option_instruments_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_index_credit_default_swap_option_instruments_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "removals",
            &delete_many_index_credit_default_swap_option_instruments_request::removals},
        utility::codec::field{
            "intent", &delete_many_index_credit_default_swap_option_instruments_request::intent},
    };
}

/**
 * @brief The members of delete_many_index_credit_default_swap_option_instruments_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_index_credit_default_swap_option_instruments_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "result", &delete_many_index_credit_default_swap_option_instruments_response::result},
    };
}

/**
 * @brief The members of list_index_credit_default_swap_option_instrument_versions_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_index_credit_default_swap_option_instrument_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "key", &list_index_credit_default_swap_option_instrument_versions_request::key},
        utility::codec::field{
            "offset", &list_index_credit_default_swap_option_instrument_versions_request::offset},
        utility::codec::field{
            "limit", &list_index_credit_default_swap_option_instrument_versions_request::limit},
        utility::codec::field{
            "order", &list_index_credit_default_swap_option_instrument_versions_request::order},
        utility::codec::field{
            "filter", &list_index_credit_default_swap_option_instrument_versions_request::filter},
    };
}

/**
 * @brief The members of list_index_credit_default_swap_option_instrument_versions_response, named
 * as the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(
    std::type_identity<list_index_credit_default_swap_option_instrument_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "result", &list_index_credit_default_swap_option_instrument_versions_response::result},
        utility::codec::field{
            "versions",
            &list_index_credit_default_swap_option_instrument_versions_response::versions},
        utility::codec::field{
            "total", &list_index_credit_default_swap_option_instrument_versions_response::total},
    };
}

/**
 * @brief The members of get_index_credit_default_swap_option_instrument_version_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_index_credit_default_swap_option_instrument_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "key", &get_index_credit_default_swap_option_instrument_version_request::key},
    };
}

/**
 * @brief The members of get_index_credit_default_swap_option_instrument_version_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_index_credit_default_swap_option_instrument_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "result", &get_index_credit_default_swap_option_instrument_version_response::result},
        utility::codec::field{
            "version", &get_index_credit_default_swap_option_instrument_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::index_credit_default_swap_option_instrument_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::index_credit_default_swap_option_instrument_key& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_key
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_key& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::index_credit_default_swap_option_instrument_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_write& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_write
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_write& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::index_credit_default_swap_option_instrument_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_change& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_change
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_change& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::index_credit_default_swap_option_instrument_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_removal& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_removal
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_removal& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::index_credit_default_swap_option_instrument_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_lookup& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_lookup
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_lookup& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::index_credit_default_swap_option_instruments_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::index_credit_default_swap_option_instruments_filter& v);
    static ores::trading::messaging::index_credit_default_swap_option_instruments_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instruments_filter& v);
    static ores::trading::messaging::index_credit_default_swap_option_instruments_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::index_credit_default_swap_option_instrument_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_event& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_event
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_event& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::index_credit_default_swap_option_instrument_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_version_key& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_version_key
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_version_key& v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::index_credit_default_swap_option_instrument_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_versions_filter&
            v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::index_credit_default_swap_option_instrument_versions_filter&
            v);
    static ores::trading::messaging::index_credit_default_swap_option_instrument_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::list_index_credit_default_swap_option_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_index_credit_default_swap_option_instruments_request&
            v);
    static ores::trading::messaging::list_index_credit_default_swap_option_instruments_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_index_credit_default_swap_option_instruments_request&
            v);
    static ores::trading::messaging::list_index_credit_default_swap_option_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::list_index_credit_default_swap_option_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_index_credit_default_swap_option_instruments_response&
            v);
    static ores::trading::messaging::list_index_credit_default_swap_option_instruments_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_index_credit_default_swap_option_instruments_response&
            v);
    static ores::trading::messaging::list_index_credit_default_swap_option_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_index_credit_default_swap_option_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::get_index_credit_default_swap_option_instrument_request& v);
    static ores::trading::messaging::get_index_credit_default_swap_option_instrument_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_index_credit_default_swap_option_instrument_request& v);
    static ores::trading::messaging::get_index_credit_default_swap_option_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::get_index_credit_default_swap_option_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::get_index_credit_default_swap_option_instrument_response&
            v);
    static ores::trading::messaging::get_index_credit_default_swap_option_instrument_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_index_credit_default_swap_option_instrument_response&
            v);
    static ores::trading::messaging::get_index_credit_default_swap_option_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::get_many_index_credit_default_swap_option_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   get_many_index_credit_default_swap_option_instruments_request& v);
    static ores::trading::messaging::get_many_index_credit_default_swap_option_instruments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      get_many_index_credit_default_swap_option_instruments_request& v);
    static ores::trading::messaging::get_many_index_credit_default_swap_option_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::get_many_index_credit_default_swap_option_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   get_many_index_credit_default_swap_option_instruments_response& v);
    static ores::trading::messaging::get_many_index_credit_default_swap_option_instruments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      get_many_index_credit_default_swap_option_instruments_response& v);
    static ores::trading::messaging::get_many_index_credit_default_swap_option_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_index_credit_default_swap_option_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::put_index_credit_default_swap_option_instrument_request& v);
    static ores::trading::messaging::put_index_credit_default_swap_option_instrument_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::put_index_credit_default_swap_option_instrument_request& v);
    static ores::trading::messaging::put_index_credit_default_swap_option_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::put_index_credit_default_swap_option_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::put_index_credit_default_swap_option_instrument_response&
            v);
    static ores::trading::messaging::put_index_credit_default_swap_option_instrument_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::put_index_credit_default_swap_option_instrument_response&
            v);
    static ores::trading::messaging::put_index_credit_default_swap_option_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::put_many_index_credit_default_swap_option_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   put_many_index_credit_default_swap_option_instruments_request& v);
    static ores::trading::messaging::put_many_index_credit_default_swap_option_instruments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      put_many_index_credit_default_swap_option_instruments_request& v);
    static ores::trading::messaging::put_many_index_credit_default_swap_option_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::put_many_index_credit_default_swap_option_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   put_many_index_credit_default_swap_option_instruments_response& v);
    static ores::trading::messaging::put_many_index_credit_default_swap_option_instruments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      put_many_index_credit_default_swap_option_instruments_response& v);
    static ores::trading::messaging::put_many_index_credit_default_swap_option_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::delete_index_credit_default_swap_option_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::delete_index_credit_default_swap_option_instrument_request&
            v);
    static ores::trading::messaging::delete_index_credit_default_swap_option_instrument_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_index_credit_default_swap_option_instrument_request&
            v);
    static ores::trading::messaging::delete_index_credit_default_swap_option_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::delete_index_credit_default_swap_option_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::delete_index_credit_default_swap_option_instrument_response&
            v);
    static ores::trading::messaging::delete_index_credit_default_swap_option_instrument_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_index_credit_default_swap_option_instrument_response&
            v);
    static ores::trading::messaging::delete_index_credit_default_swap_option_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::delete_many_index_credit_default_swap_option_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   delete_many_index_credit_default_swap_option_instruments_request& v);
    static ores::trading::messaging::
        delete_many_index_credit_default_swap_option_instruments_request
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      delete_many_index_credit_default_swap_option_instruments_request& v);
    static ores::trading::messaging::
        delete_many_index_credit_default_swap_option_instruments_request
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::delete_many_index_credit_default_swap_option_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   delete_many_index_credit_default_swap_option_instruments_response& v);
    static ores::trading::messaging::
        delete_many_index_credit_default_swap_option_instruments_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      delete_many_index_credit_default_swap_option_instruments_response& v);
    static ores::trading::messaging::
        delete_many_index_credit_default_swap_option_instruments_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::list_index_credit_default_swap_option_instrument_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   list_index_credit_default_swap_option_instrument_versions_request& v);
    static ores::trading::messaging::
        list_index_credit_default_swap_option_instrument_versions_request
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      list_index_credit_default_swap_option_instrument_versions_request& v);
    static ores::trading::messaging::
        list_index_credit_default_swap_option_instrument_versions_request
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::list_index_credit_default_swap_option_instrument_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   list_index_credit_default_swap_option_instrument_versions_response& v);
    static ores::trading::messaging::
        list_index_credit_default_swap_option_instrument_versions_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      list_index_credit_default_swap_option_instrument_versions_response& v);
    static ores::trading::messaging::
        list_index_credit_default_swap_option_instrument_versions_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::get_index_credit_default_swap_option_instrument_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   get_index_credit_default_swap_option_instrument_version_request& v);
    static ores::trading::messaging::get_index_credit_default_swap_option_instrument_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      get_index_credit_default_swap_option_instrument_version_request& v);
    static ores::trading::messaging::get_index_credit_default_swap_option_instrument_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::get_index_credit_default_swap_option_instrument_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::
                   get_index_credit_default_swap_option_instrument_version_response& v);
    static ores::trading::messaging::
        get_index_credit_default_swap_option_instrument_version_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::
                      get_index_credit_default_swap_option_instrument_version_response& v);
    static ores::trading::messaging::
        get_index_credit_default_swap_option_instrument_version_response
        read_msgpack(std::span<const std::byte> data);
};
}


#endif

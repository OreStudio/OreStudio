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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_SUMMARY_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_SUMMARY_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_summary.hpp"
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

struct trade_summary_key {
    boost::uuids::uuid id;
};

struct trade_summary_lookup {
    trade_summary_key key;
    std::optional<ores::trading::domain::trade_summary> trade_summary;
};

struct trade_summaries_filter {
    std::optional<std::string> row_kind;
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::optional<boost::uuids::uuid>> counterparty_id;
    std::optional<std::optional<boost::uuids::uuid>> book_id;
    std::optional<std::string> instrument_type;
    std::optional<std::optional<std::string>> status;
    std::optional<std::string> authorisation_status;
    std::optional<std::optional<std::chrono::year_month_day>> trade_date;
    std::optional<std::optional<boost::uuids::uuid>> structure_id;
    std::optional<bool> is_leg;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> row_kind_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> book_id_one_of;
    std::optional<std::vector<std::string>> instrument_type_one_of;
    std::optional<std::vector<std::string>> status_one_of;
    std::optional<std::vector<std::string>> authorisation_status_one_of;
    std::optional<std::vector<std::chrono::year_month_day>> trade_date_one_of;
    std::optional<std::vector<boost::uuids::uuid>> structure_id_one_of;
    std::optional<std::vector<bool>> is_leg_one_of;
    std::optional<std::string> trade_date_from;
    std::optional<std::string> trade_date_to;
    std::optional<std::string> search;
};

struct trade_summary_event {
    boost::uuids::uuid event_id;
    trade_summary_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_trade_summaries_request {
    using response_type = struct list_trade_summaries_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_summaries.list";
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
    std::optional<trade_summaries_filter> filter;
};

struct list_trade_summaries_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_summary> trade_summaries;
    std::uint64_t total;
};

struct get_trade_summary_request {
    using response_type = struct get_trade_summary_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_summaries.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_summary_key key;
};

struct get_trade_summary_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_summary> trade_summary;
};

struct get_many_trade_summaries_request {
    using response_type = struct get_many_trade_summaries_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_summaries.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_summary_key> keys;
};

struct get_many_trade_summaries_response {
    ores::utility::domain::result result;
    std::vector<trade_summary_lookup> entries;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_summary_event_subjects {
inline constexpr std::string_view created = "trading.v1.trade_summaries_events.created";
inline constexpr std::string_view updated = "trading.v1.trade_summaries_events.updated";
inline constexpr std::string_view deleted = "trading.v1.trade_summaries_events.deleted";
}
/**
 * @brief The members of trade_summary_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_summary_key&) {
    return std::tuple{
        utility::codec::field{"id", &trade_summary_key::id},
    };
}

/**
 * @brief The members of trade_summary_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_summary_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &trade_summary_lookup::key},
        utility::codec::field{"trade_summary", &trade_summary_lookup::trade_summary},
    };
}

/**
 * @brief The members of trade_summaries_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_summaries_filter&) {
    return std::tuple{
        utility::codec::field{"row_kind", &trade_summaries_filter::row_kind},
        utility::codec::field{"party_id", &trade_summaries_filter::party_id},
        utility::codec::field{"counterparty_id", &trade_summaries_filter::counterparty_id},
        utility::codec::field{"book_id", &trade_summaries_filter::book_id},
        utility::codec::field{"instrument_type", &trade_summaries_filter::instrument_type},
        utility::codec::field{"status", &trade_summaries_filter::status},
        utility::codec::field{"authorisation_status",
                              &trade_summaries_filter::authorisation_status},
        utility::codec::field{"trade_date", &trade_summaries_filter::trade_date},
        utility::codec::field{"structure_id", &trade_summaries_filter::structure_id},
        utility::codec::field{"is_leg", &trade_summaries_filter::is_leg},
        utility::codec::field{"id_one_of", &trade_summaries_filter::id_one_of},
        utility::codec::field{"row_kind_one_of", &trade_summaries_filter::row_kind_one_of},
        utility::codec::field{"party_id_one_of", &trade_summaries_filter::party_id_one_of},
        utility::codec::field{"counterparty_id_one_of",
                              &trade_summaries_filter::counterparty_id_one_of},
        utility::codec::field{"book_id_one_of", &trade_summaries_filter::book_id_one_of},
        utility::codec::field{"instrument_type_one_of",
                              &trade_summaries_filter::instrument_type_one_of},
        utility::codec::field{"status_one_of", &trade_summaries_filter::status_one_of},
        utility::codec::field{"authorisation_status_one_of",
                              &trade_summaries_filter::authorisation_status_one_of},
        utility::codec::field{"trade_date_one_of", &trade_summaries_filter::trade_date_one_of},
        utility::codec::field{"structure_id_one_of", &trade_summaries_filter::structure_id_one_of},
        utility::codec::field{"is_leg_one_of", &trade_summaries_filter::is_leg_one_of},
        utility::codec::field{"trade_date_from", &trade_summaries_filter::trade_date_from},
        utility::codec::field{"trade_date_to", &trade_summaries_filter::trade_date_to},
        utility::codec::field{"search", &trade_summaries_filter::search},
    };
}

/**
 * @brief The members of trade_summary_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_summary_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &trade_summary_event::event_id},
        utility::codec::field{"key", &trade_summary_event::key},
        utility::codec::field{"action", &trade_summary_event::action},
        utility::codec::field{"version", &trade_summary_event::version},
        utility::codec::field{"occurred_at", &trade_summary_event::occurred_at},
        utility::codec::field{"correlation_id", &trade_summary_event::correlation_id},
    };
}

/**
 * @brief The members of list_trade_summaries_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_summaries_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_trade_summaries_request::offset},
        utility::codec::field{"limit", &list_trade_summaries_request::limit},
        utility::codec::field{"order", &list_trade_summaries_request::order},
        utility::codec::field{"filter", &list_trade_summaries_request::filter},
    };
}

/**
 * @brief The members of list_trade_summaries_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_summaries_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_summaries_response::result},
        utility::codec::field{"trade_summaries", &list_trade_summaries_response::trade_summaries},
        utility::codec::field{"total", &list_trade_summaries_response::total},
    };
}

/**
 * @brief The members of get_trade_summary_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_summary_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_summary_request::key},
    };
}

/**
 * @brief The members of get_trade_summary_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_summary_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_summary_response::result},
        utility::codec::field{"trade_summary", &get_trade_summary_response::trade_summary},
    };
}

/**
 * @brief The members of get_many_trade_summaries_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_summaries_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_trade_summaries_request::keys},
    };
}

/**
 * @brief The members of get_many_trade_summaries_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_summaries_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_trade_summaries_response::result},
        utility::codec::field{"entries", &get_many_trade_summaries_response::entries},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_summary_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_summary_key& v);
    static ores::trading::messaging::trade_summary_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_summary_key& v);
    static ores::trading::messaging::trade_summary_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_summary_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_summary_lookup& v);
    static ores::trading::messaging::trade_summary_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_summary_lookup& v);
    static ores::trading::messaging::trade_summary_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_summaries_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_summaries_filter& v);
    static ores::trading::messaging::trade_summaries_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_summaries_filter& v);
    static ores::trading::messaging::trade_summaries_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_summary_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_summary_event& v);
    static ores::trading::messaging::trade_summary_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_summary_event& v);
    static ores::trading::messaging::trade_summary_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_trade_summaries_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_trade_summaries_request& v);
    static ores::trading::messaging::list_trade_summaries_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_summaries_request& v);
    static ores::trading::messaging::list_trade_summaries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_summaries_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_trade_summaries_response& v);
    static ores::trading::messaging::list_trade_summaries_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_summaries_response& v);
    static ores::trading::messaging::list_trade_summaries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_summary_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_summary_request& v);
    static ores::trading::messaging::get_trade_summary_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_trade_summary_request& v);
    static ores::trading::messaging::get_trade_summary_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_summary_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_summary_response& v);
    static ores::trading::messaging::get_trade_summary_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_trade_summary_response& v);
    static ores::trading::messaging::get_trade_summary_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_summaries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_summaries_request& v);
    static ores::trading::messaging::get_many_trade_summaries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_summaries_request& v);
    static ores::trading::messaging::get_many_trade_summaries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_summaries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_summaries_response& v);
    static ores::trading::messaging::get_many_trade_summaries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_summaries_response& v);
    static ores::trading::messaging::get_many_trade_summaries_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

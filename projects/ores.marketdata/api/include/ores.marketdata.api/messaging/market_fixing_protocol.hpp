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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_FIXING_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_FIXING_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct market_fixing_key {
    boost::uuids::uuid id;
};

struct market_fixing_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid series_id;
    std::chrono::year_month_day fixing_date;
    std::string value;
    std::string source;
};

struct market_fixing_change {
    market_fixing_write write;
    ores::utility::domain::precondition precondition;
};

struct market_fixing_removal {
    market_fixing_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct market_fixing_lookup {
    market_fixing_key key;
    std::optional<ores::marketdata::domain::market_fixing> market_fixing;
};

struct market_fixings_filter {
    std::optional<boost::uuids::uuid> series_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> series_id_one_of;
};

struct market_fixing_event {
    boost::uuids::uuid event_id;
    market_fixing_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_market_fixings_request {
    using response_type = struct list_market_fixings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_fixings.list";
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
    std::optional<market_fixings_filter> filter;
    std::optional<std::string> as_of;
};

struct list_market_fixings_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_fixing> market_fixings;
    std::uint64_t total;
};

struct get_market_fixing_request {
    using response_type = struct get_market_fixing_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_fixings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_fixing_key key;
};

struct get_market_fixing_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_fixing> market_fixing;
};

struct get_many_market_fixings_request {
    using response_type = struct get_many_market_fixings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_fixings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_fixing_key> keys;
};

struct get_many_market_fixings_response {
    ores::utility::domain::result result;
    std::vector<market_fixing_lookup> entries;
};

struct put_market_fixing_request {
    using response_type = struct put_market_fixing_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_fixings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_fixing_change change;
    ores::utility::domain::change_intent intent;
};

struct put_market_fixing_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_fixing> market_fixing;
};

struct put_many_market_fixings_request {
    using response_type = struct put_many_market_fixings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_fixings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_fixing_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_market_fixings_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_fixing> market_fixings;
};

struct delete_market_fixing_request {
    using response_type = struct delete_market_fixing_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_fixings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_fixing_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_market_fixing_response {
    ores::utility::domain::result result;
};

struct delete_many_market_fixings_request {
    using response_type = struct delete_many_market_fixings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_fixings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_fixing_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_market_fixings_response {
    ores::utility::domain::result result;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace market_fixing_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.market_fixings_events.created";
inline constexpr std::string_view updated = "marketdata.v1.market_fixings_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.market_fixings_events.deleted";
}
/**
 * @brief The members of market_fixing_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_fixing_key&) {
    return std::tuple{
        utility::codec::field{"id", &market_fixing_key::id},
    };
}

/**
 * @brief The members of market_fixing_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_fixing_write&) {
    return std::tuple{
        utility::codec::field{"id", &market_fixing_write::id},
        utility::codec::field{"party_id", &market_fixing_write::party_id},
        utility::codec::field{"series_id", &market_fixing_write::series_id},
        utility::codec::field{"fixing_date", &market_fixing_write::fixing_date},
        utility::codec::field{"value", &market_fixing_write::value},
        utility::codec::field{"source", &market_fixing_write::source},
    };
}

/**
 * @brief The members of market_fixing_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_fixing_change&) {
    return std::tuple{
        utility::codec::field{"write", &market_fixing_change::write},
        utility::codec::field{"precondition", &market_fixing_change::precondition},
    };
}

/**
 * @brief The members of market_fixing_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_fixing_removal&) {
    return std::tuple{
        utility::codec::field{"key", &market_fixing_removal::key},
        utility::codec::field{"precondition", &market_fixing_removal::precondition},
    };
}

/**
 * @brief The members of market_fixing_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_fixing_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &market_fixing_lookup::key},
        utility::codec::field{"market_fixing", &market_fixing_lookup::market_fixing},
    };
}

/**
 * @brief The members of market_fixings_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_fixings_filter&) {
    return std::tuple{
        utility::codec::field{"series_id", &market_fixings_filter::series_id},
        utility::codec::field{"id_one_of", &market_fixings_filter::id_one_of},
        utility::codec::field{"series_id_one_of", &market_fixings_filter::series_id_one_of},
    };
}

/**
 * @brief The members of market_fixing_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_fixing_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &market_fixing_event::event_id},
        utility::codec::field{"key", &market_fixing_event::key},
        utility::codec::field{"action", &market_fixing_event::action},
        utility::codec::field{"version", &market_fixing_event::version},
        utility::codec::field{"occurred_at", &market_fixing_event::occurred_at},
        utility::codec::field{"correlation_id", &market_fixing_event::correlation_id},
    };
}

/**
 * @brief The members of list_market_fixings_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_market_fixings_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_market_fixings_request::offset},
        utility::codec::field{"limit", &list_market_fixings_request::limit},
        utility::codec::field{"order", &list_market_fixings_request::order},
        utility::codec::field{"filter", &list_market_fixings_request::filter},
        utility::codec::field{"as_of", &list_market_fixings_request::as_of},
    };
}

/**
 * @brief The members of list_market_fixings_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_market_fixings_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_market_fixings_response::result},
        utility::codec::field{"market_fixings", &list_market_fixings_response::market_fixings},
        utility::codec::field{"total", &list_market_fixings_response::total},
    };
}

/**
 * @brief The members of get_market_fixing_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_market_fixing_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_market_fixing_request::key},
    };
}

/**
 * @brief The members of get_market_fixing_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_market_fixing_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_market_fixing_response::result},
        utility::codec::field{"market_fixing", &get_market_fixing_response::market_fixing},
    };
}

/**
 * @brief The members of get_many_market_fixings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_market_fixings_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_market_fixings_request::keys},
    };
}

/**
 * @brief The members of get_many_market_fixings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_market_fixings_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_market_fixings_response::result},
        utility::codec::field{"entries", &get_many_market_fixings_response::entries},
    };
}

/**
 * @brief The members of put_market_fixing_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_market_fixing_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_market_fixing_request::change},
        utility::codec::field{"intent", &put_market_fixing_request::intent},
    };
}

/**
 * @brief The members of put_market_fixing_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_market_fixing_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_market_fixing_response::result},
        utility::codec::field{"market_fixing", &put_market_fixing_response::market_fixing},
    };
}

/**
 * @brief The members of put_many_market_fixings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_market_fixings_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_market_fixings_request::changes},
        utility::codec::field{"intent", &put_many_market_fixings_request::intent},
    };
}

/**
 * @brief The members of put_many_market_fixings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_market_fixings_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_market_fixings_response::result},
        utility::codec::field{"market_fixings", &put_many_market_fixings_response::market_fixings},
    };
}

/**
 * @brief The members of delete_market_fixing_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_market_fixing_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_market_fixing_request::removal},
        utility::codec::field{"intent", &delete_market_fixing_request::intent},
    };
}

/**
 * @brief The members of delete_market_fixing_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_market_fixing_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_market_fixing_response::result},
    };
}

/**
 * @brief The members of delete_many_market_fixings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_market_fixings_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_market_fixings_request::removals},
        utility::codec::field{"intent", &delete_many_market_fixings_request::intent},
    };
}

/**
 * @brief The members of delete_many_market_fixings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_market_fixings_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_market_fixings_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_fixing_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_fixing_key& v);
    static ores::marketdata::messaging::market_fixing_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_fixing_key& v);
    static ores::marketdata::messaging::market_fixing_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_fixing_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_fixing_write& v);
    static ores::marketdata::messaging::market_fixing_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_fixing_write& v);
    static ores::marketdata::messaging::market_fixing_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_fixing_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_fixing_change& v);
    static ores::marketdata::messaging::market_fixing_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_fixing_change& v);
    static ores::marketdata::messaging::market_fixing_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_fixing_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_fixing_removal& v);
    static ores::marketdata::messaging::market_fixing_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_fixing_removal& v);
    static ores::marketdata::messaging::market_fixing_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_fixing_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_fixing_lookup& v);
    static ores::marketdata::messaging::market_fixing_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_fixing_lookup& v);
    static ores::marketdata::messaging::market_fixing_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_fixings_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_fixings_filter& v);
    static ores::marketdata::messaging::market_fixings_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_fixings_filter& v);
    static ores::marketdata::messaging::market_fixings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_fixing_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_fixing_event& v);
    static ores::marketdata::messaging::market_fixing_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_fixing_event& v);
    static ores::marketdata::messaging::market_fixing_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_fixings_request& v);
    static ores::marketdata::messaging::list_market_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_fixings_request& v);
    static ores::marketdata::messaging::list_market_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_fixings_response& v);
    static ores::marketdata::messaging::list_market_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_fixings_response& v);
    static ores::marketdata::messaging::list_market_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_fixing_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_market_fixing_request& v);
    static ores::marketdata::messaging::get_market_fixing_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_fixing_request& v);
    static ores::marketdata::messaging::get_market_fixing_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_fixing_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_market_fixing_response& v);
    static ores::marketdata::messaging::get_market_fixing_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_fixing_response& v);
    static ores::marketdata::messaging::get_market_fixing_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_fixings_request& v);
    static ores::marketdata::messaging::get_many_market_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_market_fixings_request& v);
    static ores::marketdata::messaging::get_many_market_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_fixings_response& v);
    static ores::marketdata::messaging::get_many_market_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_market_fixings_response& v);
    static ores::marketdata::messaging::get_many_market_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_fixing_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_market_fixing_request& v);
    static ores::marketdata::messaging::put_market_fixing_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_fixing_request& v);
    static ores::marketdata::messaging::put_market_fixing_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_fixing_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_market_fixing_response& v);
    static ores::marketdata::messaging::put_market_fixing_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_fixing_response& v);
    static ores::marketdata::messaging::put_market_fixing_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_fixings_request& v);
    static ores::marketdata::messaging::put_many_market_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_market_fixings_request& v);
    static ores::marketdata::messaging::put_many_market_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_fixings_response& v);
    static ores::marketdata::messaging::put_many_market_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_market_fixings_response& v);
    static ores::marketdata::messaging::put_many_market_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_fixing_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_fixing_request& v);
    static ores::marketdata::messaging::delete_market_fixing_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_fixing_request& v);
    static ores::marketdata::messaging::delete_market_fixing_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_fixing_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_fixing_response& v);
    static ores::marketdata::messaging::delete_market_fixing_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_fixing_response& v);
    static ores::marketdata::messaging::delete_market_fixing_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_market_fixings_request& v);
    static ores::marketdata::messaging::delete_many_market_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_market_fixings_request& v);
    static ores::marketdata::messaging::delete_many_market_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_market_fixings_response& v);
    static ores::marketdata::messaging::delete_many_market_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_market_fixings_response& v);
    static ores::marketdata::messaging::delete_many_market_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

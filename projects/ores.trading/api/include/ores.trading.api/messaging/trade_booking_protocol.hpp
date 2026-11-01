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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_BOOKING_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_BOOKING_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_booking.hpp"
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

struct trade_booking_key {
    boost::uuids::uuid trade_id;
};

struct trade_booking_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    boost::uuids::uuid book_id;
    std::optional<boost::uuids::uuid> netting_set_id;
    std::optional<boost::uuids::uuid> counterparty_identifier_id;
    std::optional<boost::uuids::uuid> netting_set_identifier_id;
    std::optional<std::chrono::year_month_day> trade_date;
    std::optional<std::chrono::system_clock::time_point> execution_timestamp;
};

struct trade_booking_change {
    trade_booking_write write;
    ores::utility::domain::precondition precondition;
};

struct trade_booking_removal {
    trade_booking_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct trade_booking_lookup {
    trade_booking_key key;
    std::optional<ores::trading::domain::trade_booking> trade_booking;
};

struct trade_bookings_filter {
    std::optional<std::optional<std::chrono::year_month_day>> trade_date;
    std::optional<std::optional<std::chrono::system_clock::time_point>> execution_timestamp;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
    std::optional<std::vector<std::chrono::year_month_day>> trade_date_one_of;
    std::optional<std::vector<std::chrono::system_clock::time_point>> execution_timestamp_one_of;
    std::optional<std::string> trade_date_from;
    std::optional<std::string> trade_date_to;
    std::optional<std::string> execution_timestamp_from;
    std::optional<std::string> execution_timestamp_to;
};

struct trade_booking_event {
    boost::uuids::uuid event_id;
    trade_booking_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct trade_booking_version_key {
    trade_booking_key trade_booking;
    std::uint32_t version;
};

struct trade_booking_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_trade_bookings_request {
    using response_type = struct list_trade_bookings_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings.list";
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
    std::optional<trade_bookings_filter> filter;
    std::optional<std::string> as_of;
};

struct list_trade_bookings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_booking> trade_bookings;
    std::uint64_t total;
};

struct get_trade_booking_request {
    using response_type = struct get_trade_booking_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_booking_key key;
};

struct get_trade_booking_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_booking> trade_booking;
};

struct get_many_trade_bookings_request {
    using response_type = struct get_many_trade_bookings_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_booking_key> keys;
};

struct get_many_trade_bookings_response {
    ores::utility::domain::result result;
    std::vector<trade_booking_lookup> entries;
};

struct put_trade_booking_request {
    using response_type = struct put_trade_booking_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_booking_change change;
    ores::utility::domain::change_intent intent;
};

struct put_trade_booking_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_booking> trade_booking;
};

struct put_many_trade_bookings_request {
    using response_type = struct put_many_trade_bookings_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_booking_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_trade_bookings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_booking> trade_bookings;
};

struct delete_trade_booking_request {
    using response_type = struct delete_trade_booking_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_booking_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_trade_booking_response {
    ores::utility::domain::result result;
};

struct delete_many_trade_bookings_request {
    using response_type = struct delete_many_trade_bookings_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_booking_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_trade_bookings_response {
    ores::utility::domain::result result;
};

struct list_trade_booking_versions_request {
    using response_type = struct list_trade_booking_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_booking_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_booking_versions_filter> filter;
};

struct list_trade_booking_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_booking> versions;
    std::uint64_t total;
};

struct get_trade_booking_version_request {
    using response_type = struct get_trade_booking_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_bookings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_booking_version_key key;
};

struct get_trade_booking_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_booking> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_booking_event_subjects {
inline constexpr std::string_view created = "trading.v1.trade_bookings_events.created";
inline constexpr std::string_view updated = "trading.v1.trade_bookings_events.updated";
inline constexpr std::string_view deleted = "trading.v1.trade_bookings_events.deleted";
}
/**
 * @brief The members of trade_booking_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_booking_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_booking_key::trade_id},
    };
}

/**
 * @brief The members of trade_booking_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_booking_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_booking_write::trade_id},
        utility::codec::field{"trade_activity_id", &trade_booking_write::trade_activity_id},
        utility::codec::field{"book_id", &trade_booking_write::book_id},
        utility::codec::field{"netting_set_id", &trade_booking_write::netting_set_id},
        utility::codec::field{"counterparty_identifier_id",
                              &trade_booking_write::counterparty_identifier_id},
        utility::codec::field{"netting_set_identifier_id",
                              &trade_booking_write::netting_set_identifier_id},
        utility::codec::field{"trade_date", &trade_booking_write::trade_date},
        utility::codec::field{"execution_timestamp", &trade_booking_write::execution_timestamp},
    };
}

/**
 * @brief The members of trade_booking_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_booking_change&) {
    return std::tuple{
        utility::codec::field{"write", &trade_booking_change::write},
        utility::codec::field{"precondition", &trade_booking_change::precondition},
    };
}

/**
 * @brief The members of trade_booking_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_booking_removal&) {
    return std::tuple{
        utility::codec::field{"key", &trade_booking_removal::key},
        utility::codec::field{"precondition", &trade_booking_removal::precondition},
    };
}

/**
 * @brief The members of trade_booking_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_booking_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &trade_booking_lookup::key},
        utility::codec::field{"trade_booking", &trade_booking_lookup::trade_booking},
    };
}

/**
 * @brief The members of trade_bookings_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_bookings_filter&) {
    return std::tuple{
        utility::codec::field{"trade_date", &trade_bookings_filter::trade_date},
        utility::codec::field{"execution_timestamp", &trade_bookings_filter::execution_timestamp},
        utility::codec::field{"trade_id_one_of", &trade_bookings_filter::trade_id_one_of},
        utility::codec::field{"trade_date_one_of", &trade_bookings_filter::trade_date_one_of},
        utility::codec::field{"execution_timestamp_one_of",
                              &trade_bookings_filter::execution_timestamp_one_of},
        utility::codec::field{"trade_date_from", &trade_bookings_filter::trade_date_from},
        utility::codec::field{"trade_date_to", &trade_bookings_filter::trade_date_to},
        utility::codec::field{"execution_timestamp_from",
                              &trade_bookings_filter::execution_timestamp_from},
        utility::codec::field{"execution_timestamp_to",
                              &trade_bookings_filter::execution_timestamp_to},
    };
}

/**
 * @brief The members of trade_booking_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_booking_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &trade_booking_event::event_id},
        utility::codec::field{"key", &trade_booking_event::key},
        utility::codec::field{"action", &trade_booking_event::action},
        utility::codec::field{"version", &trade_booking_event::version},
        utility::codec::field{"occurred_at", &trade_booking_event::occurred_at},
        utility::codec::field{"correlation_id", &trade_booking_event::correlation_id},
    };
}

/**
 * @brief The members of trade_booking_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_booking_version_key&) {
    return std::tuple{
        utility::codec::field{"trade_booking", &trade_booking_version_key::trade_booking},
        utility::codec::field{"version", &trade_booking_version_key::version},
    };
}

/**
 * @brief The members of trade_booking_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_booking_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &trade_booking_versions_filter::version},
        utility::codec::field{"from_version", &trade_booking_versions_filter::from_version},
        utility::codec::field{"to_version", &trade_booking_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_trade_bookings_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_bookings_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_trade_bookings_request::offset},
        utility::codec::field{"limit", &list_trade_bookings_request::limit},
        utility::codec::field{"order", &list_trade_bookings_request::order},
        utility::codec::field{"filter", &list_trade_bookings_request::filter},
        utility::codec::field{"as_of", &list_trade_bookings_request::as_of},
    };
}

/**
 * @brief The members of list_trade_bookings_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_bookings_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_bookings_response::result},
        utility::codec::field{"trade_bookings", &list_trade_bookings_response::trade_bookings},
        utility::codec::field{"total", &list_trade_bookings_response::total},
    };
}

/**
 * @brief The members of get_trade_booking_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_booking_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_booking_request::key},
    };
}

/**
 * @brief The members of get_trade_booking_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_booking_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_booking_response::result},
        utility::codec::field{"trade_booking", &get_trade_booking_response::trade_booking},
    };
}

/**
 * @brief The members of get_many_trade_bookings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_bookings_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_trade_bookings_request::keys},
    };
}

/**
 * @brief The members of get_many_trade_bookings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_bookings_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_trade_bookings_response::result},
        utility::codec::field{"entries", &get_many_trade_bookings_response::entries},
    };
}

/**
 * @brief The members of put_trade_booking_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_booking_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_trade_booking_request::change},
        utility::codec::field{"intent", &put_trade_booking_request::intent},
    };
}

/**
 * @brief The members of put_trade_booking_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_booking_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_trade_booking_response::result},
        utility::codec::field{"trade_booking", &put_trade_booking_response::trade_booking},
    };
}

/**
 * @brief The members of put_many_trade_bookings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_bookings_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_trade_bookings_request::changes},
        utility::codec::field{"intent", &put_many_trade_bookings_request::intent},
    };
}

/**
 * @brief The members of put_many_trade_bookings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_bookings_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_trade_bookings_response::result},
        utility::codec::field{"trade_bookings", &put_many_trade_bookings_response::trade_bookings},
    };
}

/**
 * @brief The members of delete_trade_booking_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_booking_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_trade_booking_request::removal},
        utility::codec::field{"intent", &delete_trade_booking_request::intent},
    };
}

/**
 * @brief The members of delete_trade_booking_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_booking_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_trade_booking_response::result},
    };
}

/**
 * @brief The members of delete_many_trade_bookings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_bookings_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_trade_bookings_request::removals},
        utility::codec::field{"intent", &delete_many_trade_bookings_request::intent},
    };
}

/**
 * @brief The members of delete_many_trade_bookings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_bookings_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_trade_bookings_response::result},
    };
}

/**
 * @brief The members of list_trade_booking_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_booking_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_trade_booking_versions_request::key},
        utility::codec::field{"offset", &list_trade_booking_versions_request::offset},
        utility::codec::field{"limit", &list_trade_booking_versions_request::limit},
        utility::codec::field{"order", &list_trade_booking_versions_request::order},
        utility::codec::field{"filter", &list_trade_booking_versions_request::filter},
    };
}

/**
 * @brief The members of list_trade_booking_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_booking_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_booking_versions_response::result},
        utility::codec::field{"versions", &list_trade_booking_versions_response::versions},
        utility::codec::field{"total", &list_trade_booking_versions_response::total},
    };
}

/**
 * @brief The members of get_trade_booking_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_booking_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_booking_version_request::key},
    };
}

/**
 * @brief The members of get_trade_booking_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_booking_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_booking_version_response::result},
        utility::codec::field{"version", &get_trade_booking_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_booking_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_booking_key& v);
    static ores::trading::messaging::trade_booking_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_booking_key& v);
    static ores::trading::messaging::trade_booking_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_booking_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_booking_write& v);
    static ores::trading::messaging::trade_booking_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_booking_write& v);
    static ores::trading::messaging::trade_booking_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_booking_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_booking_change& v);
    static ores::trading::messaging::trade_booking_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_booking_change& v);
    static ores::trading::messaging::trade_booking_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_booking_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_booking_removal& v);
    static ores::trading::messaging::trade_booking_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_booking_removal& v);
    static ores::trading::messaging::trade_booking_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_booking_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_booking_lookup& v);
    static ores::trading::messaging::trade_booking_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_booking_lookup& v);
    static ores::trading::messaging::trade_booking_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_bookings_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_bookings_filter& v);
    static ores::trading::messaging::trade_bookings_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_bookings_filter& v);
    static ores::trading::messaging::trade_bookings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_booking_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_booking_event& v);
    static ores::trading::messaging::trade_booking_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_booking_event& v);
    static ores::trading::messaging::trade_booking_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_booking_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_booking_version_key& v);
    static ores::trading::messaging::trade_booking_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_booking_version_key& v);
    static ores::trading::messaging::trade_booking_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_booking_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_booking_versions_filter& v);
    static ores::trading::messaging::trade_booking_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_booking_versions_filter& v);
    static ores::trading::messaging::trade_booking_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_trade_bookings_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_trade_bookings_request& v);
    static ores::trading::messaging::list_trade_bookings_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_bookings_request& v);
    static ores::trading::messaging::list_trade_bookings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_trade_bookings_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_trade_bookings_response& v);
    static ores::trading::messaging::list_trade_bookings_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_bookings_response& v);
    static ores::trading::messaging::list_trade_bookings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_booking_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_booking_request& v);
    static ores::trading::messaging::get_trade_booking_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_trade_booking_request& v);
    static ores::trading::messaging::get_trade_booking_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_booking_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_booking_response& v);
    static ores::trading::messaging::get_trade_booking_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_trade_booking_response& v);
    static ores::trading::messaging::get_trade_booking_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_bookings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_bookings_request& v);
    static ores::trading::messaging::get_many_trade_bookings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_bookings_request& v);
    static ores::trading::messaging::get_many_trade_bookings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_bookings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_bookings_response& v);
    static ores::trading::messaging::get_many_trade_bookings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_bookings_response& v);
    static ores::trading::messaging::get_many_trade_bookings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_trade_booking_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_trade_booking_request& v);
    static ores::trading::messaging::put_trade_booking_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_trade_booking_request& v);
    static ores::trading::messaging::put_trade_booking_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_trade_booking_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_trade_booking_response& v);
    static ores::trading::messaging::put_trade_booking_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_trade_booking_response& v);
    static ores::trading::messaging::put_trade_booking_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_bookings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_bookings_request& v);
    static ores::trading::messaging::put_many_trade_bookings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_bookings_request& v);
    static ores::trading::messaging::put_many_trade_bookings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_bookings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_bookings_response& v);
    static ores::trading::messaging::put_many_trade_bookings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_bookings_response& v);
    static ores::trading::messaging::put_many_trade_bookings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_trade_booking_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_trade_booking_request& v);
    static ores::trading::messaging::delete_trade_booking_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_booking_request& v);
    static ores::trading::messaging::delete_trade_booking_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_trade_booking_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_trade_booking_response& v);
    static ores::trading::messaging::delete_trade_booking_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_booking_response& v);
    static ores::trading::messaging::delete_trade_booking_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_bookings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_bookings_request& v);
    static ores::trading::messaging::delete_many_trade_bookings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_bookings_request& v);
    static ores::trading::messaging::delete_many_trade_bookings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_bookings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_bookings_response& v);
    static ores::trading::messaging::delete_many_trade_bookings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_bookings_response& v);
    static ores::trading::messaging::delete_many_trade_bookings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_booking_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_booking_versions_request& v);
    static ores::trading::messaging::list_trade_booking_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_booking_versions_request& v);
    static ores::trading::messaging::list_trade_booking_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_booking_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_booking_versions_response& v);
    static ores::trading::messaging::list_trade_booking_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_booking_versions_response& v);
    static ores::trading::messaging::list_trade_booking_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_booking_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_booking_version_request& v);
    static ores::trading::messaging::get_trade_booking_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_booking_version_request& v);
    static ores::trading::messaging::get_trade_booking_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_booking_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_booking_version_response& v);
    static ores::trading::messaging::get_trade_booking_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_booking_version_response& v);
    static ores::trading::messaging::get_trade_booking_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif

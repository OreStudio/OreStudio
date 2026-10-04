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
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct trade_booking_key {
    boost::uuids::uuid trade_id;
};

struct trade_booking_write {
    boost::uuids::uuid trade_id;
    std::optional<boost::uuids::uuid> counterparty_id;
    boost::uuids::uuid book_id;
    std::optional<boost::uuids::uuid> netting_set_id;
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
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
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

}

#endif

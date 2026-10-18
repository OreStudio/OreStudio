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
#ifndef ORES_TRADING_API_MESSAGING_RANGE_BOUND_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_RANGE_BOUND_PROTOCOL_HPP

#include "ores.trading.api/domain/range_bound.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct range_bound_key {
    boost::uuids::uuid trade_id;
    int sequence_number;
};

struct range_bound_write {
    boost::uuids::uuid trade_id;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::optional<std::chrono::year_month_day> start_date;
    std::optional<ores::utility::decimal::decimal> range_from;
    std::optional<ores::utility::decimal::decimal> range_to;
    std::optional<double> leverage;
    std::optional<ores::utility::decimal::decimal> strike;
    std::optional<ores::utility::decimal::decimal> strike_adjustment;
};

struct range_bound_change {
    range_bound_write write;
    ores::utility::domain::precondition precondition;
};

struct range_bound_removal {
    range_bound_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct range_bound_lookup {
    range_bound_key key;
    std::optional<ores::trading::domain::range_bound> range_bound;
};

struct range_bounds_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct range_bound_event {
    boost::uuids::uuid event_id;
    range_bound_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct range_bound_version_key {
    range_bound_key range_bound;
    std::uint32_t version;
};

struct range_bound_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_range_bounds_request {
    using response_type = struct list_range_bounds_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds.list";
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
    std::optional<range_bounds_filter> filter;
    std::optional<std::string> as_of;
};

struct list_range_bounds_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::range_bound> range_bounds;
    std::uint64_t total;
};

struct get_range_bound_request {
    using response_type = struct get_range_bound_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    range_bound_key key;
};

struct get_range_bound_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::range_bound> range_bound;
};

struct get_many_range_bounds_request {
    using response_type = struct get_many_range_bounds_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<range_bound_key> keys;
};

struct get_many_range_bounds_response {
    ores::utility::domain::result result;
    std::vector<range_bound_lookup> entries;
};

struct put_range_bound_request {
    using response_type = struct put_range_bound_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    range_bound_change change;
    ores::utility::domain::change_intent intent;
};

struct put_range_bound_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::range_bound> range_bound;
};

struct put_many_range_bounds_request {
    using response_type = struct put_many_range_bounds_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<range_bound_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_range_bounds_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::range_bound> range_bounds;
};

struct delete_range_bound_request {
    using response_type = struct delete_range_bound_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    range_bound_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_range_bound_response {
    ores::utility::domain::result result;
};

struct delete_many_range_bounds_request {
    using response_type = struct delete_many_range_bounds_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<range_bound_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_range_bounds_response {
    ores::utility::domain::result result;
};

struct list_by_trade_id_range_bounds_request {
    using response_type = struct list_by_trade_id_range_bounds_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds.list_by_trade_id";
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
    std::optional<range_bounds_filter> filter;
};

struct list_by_trade_id_range_bounds_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::range_bound> range_bounds;
    std::uint64_t total;
};

struct list_range_bound_versions_request {
    using response_type = struct list_range_bound_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    range_bound_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<range_bound_versions_filter> filter;
};

struct list_range_bound_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::range_bound> versions;
    std::uint64_t total;
};

struct get_range_bound_version_request {
    using response_type = struct get_range_bound_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.range_bounds_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    range_bound_version_key key;
};

struct get_range_bound_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::range_bound> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace range_bound_event_subjects {
inline constexpr std::string_view created = "trading.v1.range_bounds_events.created";
inline constexpr std::string_view updated = "trading.v1.range_bounds_events.updated";
inline constexpr std::string_view deleted = "trading.v1.range_bounds_events.deleted";
}

}

#endif

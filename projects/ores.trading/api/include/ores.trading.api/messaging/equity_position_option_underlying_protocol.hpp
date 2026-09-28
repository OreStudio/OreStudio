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
#ifndef ORES_TRADING_API_MESSAGING_EQUITY_POSITION_OPTION_UNDERLYING_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_EQUITY_POSITION_OPTION_UNDERLYING_PROTOCOL_HPP

#include "ores.trading.api/domain/equity_position_option_underlying.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct equity_position_option_underlying_key {
    boost::uuids::uuid instrument_id;
    int sequence_number;
};

struct equity_position_option_underlying_write {
    boost::uuids::uuid instrument_id;
    int sequence_number;
    std::string underlying_name;
    ores::utility::decimal::decimal strike;
    std::optional<ores::utility::decimal::decimal> weight;
    std::string long_short;
    std::string option_type;
    std::string exercise_type;
    std::string settlement_type;
};

struct equity_position_option_underlying_change {
    equity_position_option_underlying_write write;
    ores::utility::domain::precondition precondition;
};

struct equity_position_option_underlying_removal {
    equity_position_option_underlying_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct equity_position_option_underlying_lookup {
    equity_position_option_underlying_key key;
    std::optional<ores::trading::domain::equity_position_option_underlying>
        equity_position_option_underlying;
};

struct equity_position_option_underlying_event {
    boost::uuids::uuid event_id;
    equity_position_option_underlying_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct equity_position_option_underlying_version_key {
    equity_position_option_underlying_key equity_position_option_underlying;
    std::uint32_t version;
};

struct equity_position_option_underlying_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_equity_position_option_underlyings_request {
    using response_type = struct list_equity_position_option_underlyings_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings.list";
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
};

struct list_equity_position_option_underlyings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::equity_position_option_underlying>
        equity_position_option_underlyings;
    std::uint64_t total;
};

struct get_equity_position_option_underlying_request {
    using response_type = struct get_equity_position_option_underlying_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_position_option_underlying_key key;
};

struct get_equity_position_option_underlying_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::equity_position_option_underlying>
        equity_position_option_underlying;
};

struct get_many_equity_position_option_underlyings_request {
    using response_type = struct get_many_equity_position_option_underlyings_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_position_option_underlying_key> keys;
};

struct get_many_equity_position_option_underlyings_response {
    ores::utility::domain::result result;
    std::vector<equity_position_option_underlying_lookup> entries;
};

struct put_equity_position_option_underlying_request {
    using response_type = struct put_equity_position_option_underlying_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_position_option_underlying_change change;
    ores::utility::domain::change_intent intent;
};

struct put_equity_position_option_underlying_response {
    ores::utility::domain::result result;
    ores::trading::domain::equity_position_option_underlying equity_position_option_underlying;
};

struct put_many_equity_position_option_underlyings_request {
    using response_type = struct put_many_equity_position_option_underlyings_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_position_option_underlying_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_equity_position_option_underlyings_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::equity_position_option_underlying>
        equity_position_option_underlyings;
};

struct delete_equity_position_option_underlying_request {
    using response_type = struct delete_equity_position_option_underlying_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_position_option_underlying_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_equity_position_option_underlying_response {
    ores::utility::domain::result result;
};

struct delete_many_equity_position_option_underlyings_request {
    using response_type = struct delete_many_equity_position_option_underlyings_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_position_option_underlying_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_equity_position_option_underlyings_response {
    ores::utility::domain::result result;
};

struct list_equity_position_option_underlying_versions_request {
    using response_type = struct list_equity_position_option_underlying_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_position_option_underlying_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<equity_position_option_underlying_versions_filter> filter;
};

struct list_equity_position_option_underlying_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::equity_position_option_underlying> versions;
    std::uint64_t total;
};

struct get_equity_position_option_underlying_version_request {
    using response_type = struct get_equity_position_option_underlying_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_option_underlyings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_position_option_underlying_version_key key;
};

struct get_equity_position_option_underlying_version_response {
    ores::utility::domain::result result;
    ores::trading::domain::equity_position_option_underlying version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace equity_position_option_underlying_event_subjects {
inline constexpr std::string_view created =
    "trading.v1.equity_position_option_underlyings_events.created";
inline constexpr std::string_view updated =
    "trading.v1.equity_position_option_underlyings_events.updated";
inline constexpr std::string_view deleted =
    "trading.v1.equity_position_option_underlyings_events.deleted";
}

}

#endif

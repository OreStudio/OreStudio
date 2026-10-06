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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_ADDITIONAL_FIELD_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_ADDITIONAL_FIELD_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_additional_field.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct trade_additional_field_key {
    boost::uuids::uuid trade_id;
    int sequence_number;
};

struct trade_additional_field_write {
    boost::uuids::uuid trade_id;
    int sequence_number;
    std::string name;
    std::string value;
};

struct trade_additional_field_change {
    trade_additional_field_write write;
    ores::utility::domain::precondition precondition;
};

struct trade_additional_field_removal {
    trade_additional_field_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct trade_additional_field_lookup {
    trade_additional_field_key key;
    std::optional<ores::trading::domain::trade_additional_field> trade_additional_field;
};

struct trade_additional_field_event {
    boost::uuids::uuid event_id;
    trade_additional_field_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct trade_additional_field_version_key {
    trade_additional_field_key trade_additional_field;
    std::uint32_t version;
};

struct trade_additional_field_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_trade_additional_fields_request {
    using response_type = struct list_trade_additional_fields_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.list";
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
    std::optional<std::string> as_of;
};

struct list_trade_additional_fields_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_additional_field> trade_additional_fields;
    std::uint64_t total;
};

struct get_trade_additional_field_request {
    using response_type = struct get_trade_additional_field_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_key key;
};

struct get_trade_additional_field_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_additional_field> trade_additional_field;
};

struct get_many_trade_additional_fields_request {
    using response_type = struct get_many_trade_additional_fields_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_additional_field_key> keys;
};

struct get_many_trade_additional_fields_response {
    ores::utility::domain::result result;
    std::vector<trade_additional_field_lookup> entries;
};

struct put_trade_additional_field_request {
    using response_type = struct put_trade_additional_field_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_change change;
    ores::utility::domain::change_intent intent;
};

struct put_trade_additional_field_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_additional_field> trade_additional_field;
};

struct put_many_trade_additional_fields_request {
    using response_type = struct put_many_trade_additional_fields_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_additional_field_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_trade_additional_fields_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_additional_field> trade_additional_fields;
};

struct delete_trade_additional_field_request {
    using response_type = struct delete_trade_additional_field_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_additional_fields.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_trade_additional_field_response {
    ores::utility::domain::result result;
};

struct delete_many_trade_additional_fields_request {
    using response_type = struct delete_many_trade_additional_fields_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_additional_fields.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_additional_field_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_trade_additional_fields_response {
    ores::utility::domain::result result;
};

struct list_trade_additional_field_versions_request {
    using response_type = struct list_trade_additional_field_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_additional_fields_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_additional_field_versions_filter> filter;
};

struct list_trade_additional_field_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_additional_field> versions;
    std::uint64_t total;
};

struct get_trade_additional_field_version_request {
    using response_type = struct get_trade_additional_field_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_additional_fields_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_additional_field_version_key key;
};

struct get_trade_additional_field_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_additional_field> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_additional_field_event_subjects {
inline constexpr std::string_view created = "trading.v1.trade_additional_fields_events.created";
inline constexpr std::string_view updated = "trading.v1.trade_additional_fields_events.updated";
inline constexpr std::string_view deleted = "trading.v1.trade_additional_fields_events.deleted";
}

}

#endif

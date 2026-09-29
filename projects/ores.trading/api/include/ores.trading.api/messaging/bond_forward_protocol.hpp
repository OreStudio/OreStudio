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
#ifndef ORES_TRADING_API_MESSAGING_BOND_FORWARD_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_FORWARD_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_forward.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct bond_forward_key {
    boost::uuids::uuid trade_id;
};

struct bond_forward_write {
    boost::uuids::uuid trade_id;
    std::optional<std::string> long_in_forward;
    std::optional<std::string> forward_maturity_date;
    std::optional<std::string> forward_settlement_date;
    std::optional<std::string> settlement;
    std::optional<ores::utility::decimal::decimal> amount;
    std::optional<double> lock_rate;
    std::optional<ores::utility::decimal::decimal> dv01;
    std::optional<std::string> lock_rate_day_counter;
    std::optional<std::string> settlement_dirty;
    std::optional<std::string> premium_amount;
    std::optional<std::string> premium_date;
};

struct bond_forward_change {
    bond_forward_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_forward_removal {
    bond_forward_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_forward_lookup {
    bond_forward_key key;
    std::optional<ores::trading::domain::bond_forward> bond_forward;
};

struct bond_forward_event {
    boost::uuids::uuid event_id;
    bond_forward_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_forward_version_key {
    bond_forward_key bond_forward;
    std::uint32_t version;
};

struct bond_forward_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_forwards_request {
    using response_type = struct list_bond_forwards_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.list";
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

struct list_bond_forwards_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_forward> bond_forwards;
    std::uint64_t total;
};

struct get_bond_forward_request {
    using response_type = struct get_bond_forward_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_key key;
};

struct get_bond_forward_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_forward> bond_forward;
};

struct get_many_bond_forwards_request {
    using response_type = struct get_many_bond_forwards_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_forward_key> keys;
};

struct get_many_bond_forwards_response {
    ores::utility::domain::result result;
    std::vector<bond_forward_lookup> entries;
};

struct put_bond_forward_request {
    using response_type = struct put_bond_forward_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_forward_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_forward> bond_forward;
};

struct put_many_bond_forwards_request {
    using response_type = struct put_many_bond_forwards_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_forward_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_forwards_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_forward> bond_forwards;
};

struct delete_bond_forward_request {
    using response_type = struct delete_bond_forward_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_forward_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_forwards_request {
    using response_type = struct delete_many_bond_forwards_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_forward_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_forwards_response {
    ores::utility::domain::result result;
};

struct list_bond_forward_versions_request {
    using response_type = struct list_bond_forward_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_forward_versions_filter> filter;
};

struct list_bond_forward_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_forward> versions;
    std::uint64_t total;
};

struct get_bond_forward_version_request {
    using response_type = struct get_bond_forward_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_version_key key;
};

struct get_bond_forward_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_forward> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_forward_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_forwards_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_forwards_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_forwards_events.deleted";
}

}

#endif

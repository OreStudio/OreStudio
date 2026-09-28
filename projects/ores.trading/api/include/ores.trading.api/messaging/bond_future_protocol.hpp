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
#ifndef ORES_TRADING_API_MESSAGING_BOND_FUTURE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_FUTURE_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_future.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct bond_future_key {
    boost::uuids::uuid trade_id;
};

struct bond_future_write {
    boost::uuids::uuid trade_id;
    std::string contract_name;
    ores::utility::decimal::decimal contract_notional;
    std::string long_short;
    std::string currency;
    std::string contract_month;
    std::string deliverable_grade;
    ores::utility::decimal::decimal fair_price;
    std::string settlement;
    bool settlement_dirty;
    std::optional<std::chrono::year_month_day> root_date;
    std::string expiry_basis;
    std::string settlement_basis;
    int expiry_lag;
    int settlement_lag;
    std::chrono::year_month_day last_trading_date;
    std::chrono::year_month_day last_delivery_date;
};

struct bond_future_change {
    bond_future_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_future_removal {
    bond_future_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_future_lookup {
    bond_future_key key;
    std::optional<ores::trading::domain::bond_future> bond_future;
};

struct bond_future_event {
    boost::uuids::uuid event_id;
    bond_future_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_future_version_key {
    bond_future_key bond_future;
    std::uint32_t version;
};

struct bond_future_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_futures_request {
    using response_type = struct list_bond_futures_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.list";
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

struct list_bond_futures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_future> futures;
    std::uint64_t total;
};

struct get_bond_future_request {
    using response_type = struct get_bond_future_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_key key;
};

struct get_bond_future_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_future> bond_future;
};

struct get_many_bond_futures_request {
    using response_type = struct get_many_bond_futures_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_future_key> keys;
};

struct get_many_bond_futures_response {
    ores::utility::domain::result result;
    std::vector<bond_future_lookup> entries;
};

struct put_bond_future_request {
    using response_type = struct put_bond_future_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_future_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_future> bond_future;
};

struct put_many_bond_futures_request {
    using response_type = struct put_many_bond_futures_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_future_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_futures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_future> futures;
};

struct delete_bond_future_request {
    using response_type = struct delete_bond_future_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_future_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_futures_request {
    using response_type = struct delete_many_bond_futures_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_future_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_futures_response {
    ores::utility::domain::result result;
};

struct list_bond_future_versions_request {
    using response_type = struct list_bond_future_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_future_versions_filter> filter;
};

struct list_bond_future_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_future> versions;
    std::uint64_t total;
};

struct get_bond_future_version_request {
    using response_type = struct get_bond_future_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_version_key key;
};

struct get_bond_future_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_future> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_future_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_futures_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_futures_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_futures_events.deleted";
}

}

#endif

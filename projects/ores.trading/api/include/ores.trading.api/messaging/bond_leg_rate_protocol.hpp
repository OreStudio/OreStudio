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
#ifndef ORES_TRADING_API_MESSAGING_BOND_LEG_RATE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_LEG_RATE_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_leg_rate.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct bond_leg_rate_key {
    boost::uuids::uuid instrument_id;
    std::string leg_role;
    int leg_number;
};

struct bond_leg_rate_write {
    boost::uuids::uuid instrument_id;
    std::string leg_role;
    int leg_number;
    std::string rate_kind;
    std::optional<std::string> index;
    std::optional<bool> is_in_arrears;
    std::optional<std::int64_t> fixing_days;
    std::optional<std::string> fixing_calendar;
    std::optional<std::string> last_recent_period;
    std::optional<std::string> last_recent_period_calendar;
    std::optional<std::string> lookback;
    std::optional<std::int64_t> rate_cutoff;
    std::optional<bool> is_averaged;
    std::optional<bool> has_sub_periods;
    std::optional<bool> include_spread;
    std::optional<bool> is_not_resetting_xccy;
    std::optional<bool> naked_option;
    std::optional<bool> local_cap_floor;
    std::optional<bool> stub_use_original_curve;
    std::optional<bool> observation_shift;
    std::optional<std::string> front_stub_short_index;
    std::optional<std::string> front_stub_long_index;
    std::optional<std::string> front_stub_rounding_type;
    std::optional<std::int64_t> front_stub_rounding_precision;
    std::optional<std::string> back_stub_short_index;
    std::optional<std::string> back_stub_long_index;
    std::optional<std::string> back_stub_rounding_type;
    std::optional<std::int64_t> back_stub_rounding_precision;
};

struct bond_leg_rate_change {
    bond_leg_rate_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_leg_rate_removal {
    bond_leg_rate_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_leg_rate_lookup {
    bond_leg_rate_key key;
    std::optional<ores::trading::domain::bond_leg_rate> bond_leg_rate;
};

struct bond_leg_rate_event {
    boost::uuids::uuid event_id;
    bond_leg_rate_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_leg_rate_version_key {
    bond_leg_rate_key bond_leg_rate;
    std::uint32_t version;
};

struct bond_leg_rate_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_leg_rates_request {
    using response_type = struct list_bond_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.list";
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

struct list_bond_leg_rates_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_leg_rate> bond_leg_rates;
    std::uint64_t total;
};

struct get_bond_leg_rate_request {
    using response_type = struct get_bond_leg_rate_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_rate_key key;
};

struct get_bond_leg_rate_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_leg_rate> bond_leg_rate;
};

struct get_many_bond_leg_rates_request {
    using response_type = struct get_many_bond_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_leg_rate_key> keys;
};

struct get_many_bond_leg_rates_response {
    ores::utility::domain::result result;
    std::vector<bond_leg_rate_lookup> entries;
};

struct put_bond_leg_rate_request {
    using response_type = struct put_bond_leg_rate_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_rate_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_leg_rate_response {
    ores::utility::domain::result result;
    ores::trading::domain::bond_leg_rate bond_leg_rate;
};

struct put_many_bond_leg_rates_request {
    using response_type = struct put_many_bond_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_leg_rate_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_leg_rates_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_leg_rate> bond_leg_rates;
};

struct delete_bond_leg_rate_request {
    using response_type = struct delete_bond_leg_rate_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_rate_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_leg_rate_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_leg_rates_request {
    using response_type = struct delete_many_bond_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_leg_rate_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_leg_rates_response {
    ores::utility::domain::result result;
};

struct list_bond_leg_rate_versions_request {
    using response_type = struct list_bond_leg_rate_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_rate_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_leg_rate_versions_filter> filter;
};

struct list_bond_leg_rate_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_leg_rate> versions;
    std::uint64_t total;
};

struct get_bond_leg_rate_version_request {
    using response_type = struct get_bond_leg_rate_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_rate_version_key key;
};

struct get_bond_leg_rate_version_response {
    ores::utility::domain::result result;
    ores::trading::domain::bond_leg_rate version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_leg_rate_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_leg_rates_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_leg_rates_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_leg_rates_events.deleted";
}

}

#endif

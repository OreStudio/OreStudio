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
#ifndef ORES_TRADING_API_MESSAGING_BOND_OPTION_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_OPTION_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_option.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct bond_option_key {
    boost::uuids::uuid trade_id;
};

struct bond_option_write {
    boost::uuids::uuid trade_id;
    std::string option_type;
    ores::utility::decimal::decimal option_strike;
    std::optional<std::string> redemption;
    std::optional<std::string> price_type;
    std::optional<std::string> knocks_out;
};

struct bond_option_change {
    bond_option_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_option_removal {
    bond_option_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_option_lookup {
    bond_option_key key;
    std::optional<ores::trading::domain::bond_option> bond_option;
};

struct bond_option_event {
    boost::uuids::uuid event_id;
    bond_option_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_option_version_key {
    bond_option_key bond_option;
    std::uint32_t version;
};

struct bond_option_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_options_request {
    using response_type = struct list_bond_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.list";
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

struct list_bond_options_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_option> options;
    std::uint64_t total;
};

struct get_bond_option_request {
    using response_type = struct get_bond_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_key key;
};

struct get_bond_option_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_option> bond_option;
};

struct get_many_bond_options_request {
    using response_type = struct get_many_bond_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_option_key> keys;
};

struct get_many_bond_options_response {
    ores::utility::domain::result result;
    std::vector<bond_option_lookup> entries;
};

struct put_bond_option_request {
    using response_type = struct put_bond_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_option_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_option> bond_option;
};

struct put_many_bond_options_request {
    using response_type = struct put_many_bond_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_option_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_options_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_option> options;
};

struct delete_bond_option_request {
    using response_type = struct delete_bond_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_option_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_options_request {
    using response_type = struct delete_many_bond_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_option_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_options_response {
    ores::utility::domain::result result;
};

struct list_bond_option_versions_request {
    using response_type = struct list_bond_option_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_option_versions_filter> filter;
};

struct list_bond_option_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_option> versions;
    std::uint64_t total;
};

struct get_bond_option_version_request {
    using response_type = struct get_bond_option_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_version_key key;
};

struct get_bond_option_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_option> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_option_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_options_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_options_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_options_events.deleted";
}

}

#endif

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
#ifndef ORES_TRADING_API_MESSAGING_ASCOT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_ASCOT_PROTOCOL_HPP

#include "ores.trading.api/domain/ascot.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct ascot_key {
    boost::uuids::uuid instrument_id;
};

struct ascot_write {
    boost::uuids::uuid instrument_id;
    std::string ascot_option_type;
};

struct ascot_change {
    ascot_write write;
    ores::utility::domain::precondition precondition;
};

struct ascot_removal {
    ascot_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct ascot_lookup {
    ascot_key key;
    std::optional<ores::trading::domain::ascot> ascot;
};

struct ascot_event {
    boost::uuids::uuid event_id;
    ascot_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct ascot_version_key {
    ascot_key ascot;
    std::uint32_t version;
};

struct ascot_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_ascots_request {
    using response_type = struct list_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.list";
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

struct list_ascots_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::ascot> ascots;
    std::uint64_t total;
};

struct get_ascot_request {
    using response_type = struct get_ascot_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_key key;
};

struct get_ascot_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::ascot> ascot;
};

struct get_many_ascots_request {
    using response_type = struct get_many_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ascot_key> keys;
};

struct get_many_ascots_response {
    ores::utility::domain::result result;
    std::vector<ascot_lookup> entries;
};

struct put_ascot_request {
    using response_type = struct put_ascot_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_change change;
    ores::utility::domain::change_intent intent;
};

struct put_ascot_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::ascot> ascot;
};

struct put_many_ascots_request {
    using response_type = struct put_many_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ascot_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_ascots_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::ascot> ascots;
};

struct delete_ascot_request {
    using response_type = struct delete_ascot_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_ascot_response {
    ores::utility::domain::result result;
};

struct delete_many_ascots_request {
    using response_type = struct delete_many_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ascot_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_ascots_response {
    ores::utility::domain::result result;
};

struct list_ascot_versions_request {
    using response_type = struct list_ascot_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ascot_versions_filter> filter;
};

struct list_ascot_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::ascot> versions;
    std::uint64_t total;
};

struct get_ascot_version_request {
    using response_type = struct get_ascot_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ascot_version_key key;
};

struct get_ascot_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::ascot> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace ascot_event_subjects {
inline constexpr std::string_view created = "trading.v1.ascots_events.created";
inline constexpr std::string_view updated = "trading.v1.ascots_events.updated";
inline constexpr std::string_view deleted = "trading.v1.ascots_events.deleted";
}

}

#endif

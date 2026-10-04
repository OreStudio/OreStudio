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
#ifndef ORES_ANALYTICS_API_MESSAGING_TODAYS_MARKET_COLLECTION_KIND_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_TODAYS_MARKET_COLLECTION_KIND_PROTOCOL_HPP

#include "ores.analytics.api/domain/todays_market_collection_kind.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::messaging {

struct todays_market_collection_kind_key {
    std::string code;
};

struct todays_market_collection_kind_write {
    std::string code;
    std::string entry_element;
    std::string key_attribute;
    std::optional<std::string> key_attribute_2;
    std::string description;
};

struct todays_market_collection_kind_change {
    todays_market_collection_kind_write write;
    ores::utility::domain::precondition precondition;
};

struct todays_market_collection_kind_removal {
    todays_market_collection_kind_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct todays_market_collection_kind_lookup {
    todays_market_collection_kind_key key;
    std::optional<ores::analytics::domain::todays_market_collection_kind>
        todays_market_collection_kind;
};

struct todays_market_collection_kinds_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct todays_market_collection_kind_event {
    boost::uuids::uuid event_id;
    todays_market_collection_kind_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct todays_market_collection_kind_version_key {
    todays_market_collection_kind_key todays_market_collection_kind;
    std::uint32_t version;
};

struct todays_market_collection_kind_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_todays_market_collection_kinds_request {
    using response_type = struct list_todays_market_collection_kinds_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds.list";
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
    std::optional<todays_market_collection_kinds_filter> filter;
};

struct list_todays_market_collection_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_collection_kind> kinds;
    std::uint64_t total;
};

struct get_todays_market_collection_kind_request {
    using response_type = struct get_todays_market_collection_kind_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_kind_key key;
};

struct get_todays_market_collection_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_collection_kind>
        todays_market_collection_kind;
};

struct get_many_todays_market_collection_kinds_request {
    using response_type = struct get_many_todays_market_collection_kinds_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_collection_kind_key> keys;
};

struct get_many_todays_market_collection_kinds_response {
    ores::utility::domain::result result;
    std::vector<todays_market_collection_kind_lookup> entries;
};

struct put_todays_market_collection_kind_request {
    using response_type = struct put_todays_market_collection_kind_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_kind_change change;
    ores::utility::domain::change_intent intent;
};

struct put_todays_market_collection_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_collection_kind>
        todays_market_collection_kind;
};

struct put_many_todays_market_collection_kinds_request {
    using response_type = struct put_many_todays_market_collection_kinds_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_collection_kind_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_todays_market_collection_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_collection_kind> kinds;
};

struct delete_todays_market_collection_kind_request {
    using response_type = struct delete_todays_market_collection_kind_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_kind_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_todays_market_collection_kind_response {
    ores::utility::domain::result result;
};

struct delete_many_todays_market_collection_kinds_request {
    using response_type = struct delete_many_todays_market_collection_kinds_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_collection_kind_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_todays_market_collection_kinds_response {
    ores::utility::domain::result result;
};

struct list_todays_market_collection_kind_versions_request {
    using response_type = struct list_todays_market_collection_kind_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_kind_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<todays_market_collection_kind_versions_filter> filter;
};

struct list_todays_market_collection_kind_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_collection_kind> versions;
    std::uint64_t total;
};

struct get_todays_market_collection_kind_version_request {
    using response_type = struct get_todays_market_collection_kind_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collection_kinds_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_kind_version_key key;
};

struct get_todays_market_collection_kind_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_collection_kind> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace todays_market_collection_kind_event_subjects {
inline constexpr std::string_view created =
    "analytics.v1.todays_market_collection_kinds_events.created";
inline constexpr std::string_view updated =
    "analytics.v1.todays_market_collection_kinds_events.updated";
inline constexpr std::string_view deleted =
    "analytics.v1.todays_market_collection_kinds_events.deleted";
}

}

#endif

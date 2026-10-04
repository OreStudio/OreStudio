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
#ifndef ORES_ANALYTICS_API_MESSAGING_TODAYS_MARKET_ENTRY_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_TODAYS_MARKET_ENTRY_PROTOCOL_HPP

#include "ores.analytics.api/domain/todays_market_entry.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::messaging {

struct todays_market_entry_key {
    boost::uuids::uuid id;
};

struct todays_market_entry_write {
    boost::uuids::uuid id;
    boost::uuids::uuid todays_market_config_id;
    boost::uuids::uuid todays_market_collection_id;
    std::optional<std::string> key_value;
    std::optional<std::string> key_value_2;
    std::string target;
    std::optional<std::string> discounting;
    int position;
};

struct todays_market_entry_change {
    todays_market_entry_write write;
    ores::utility::domain::precondition precondition;
};

struct todays_market_entry_removal {
    todays_market_entry_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct todays_market_entry_lookup {
    todays_market_entry_key key;
    std::optional<ores::analytics::domain::todays_market_entry> todays_market_entry;
};

struct todays_market_entries_filter {
    std::optional<boost::uuids::uuid> todays_market_config_id;
    std::optional<boost::uuids::uuid> todays_market_collection_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> todays_market_config_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> todays_market_collection_id_one_of;
};

struct todays_market_entry_event {
    boost::uuids::uuid event_id;
    todays_market_entry_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct todays_market_entry_version_key {
    todays_market_entry_key todays_market_entry;
    std::uint32_t version;
};

struct todays_market_entry_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_todays_market_entries_request {
    using response_type = struct list_todays_market_entries_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_entries.list";
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
    std::optional<todays_market_entries_filter> filter;
};

struct list_todays_market_entries_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_entry> entries;
    std::uint64_t total;
};

struct get_todays_market_entry_request {
    using response_type = struct get_todays_market_entry_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_entries.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_entry_key key;
};

struct get_todays_market_entry_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_entry> todays_market_entry;
};

struct get_many_todays_market_entries_request {
    using response_type = struct get_many_todays_market_entries_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_entries.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_entry_key> keys;
};

struct get_many_todays_market_entries_response {
    ores::utility::domain::result result;
    std::vector<todays_market_entry_lookup> entries;
};

struct put_todays_market_entry_request {
    using response_type = struct put_todays_market_entry_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_entries.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_entry_change change;
    ores::utility::domain::change_intent intent;
};

struct put_todays_market_entry_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_entry> todays_market_entry;
};

struct put_many_todays_market_entries_request {
    using response_type = struct put_many_todays_market_entries_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_entries.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_entry_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_todays_market_entries_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_entry> entries;
};

struct delete_todays_market_entry_request {
    using response_type = struct delete_todays_market_entry_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_entries.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_entry_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_todays_market_entry_response {
    ores::utility::domain::result result;
};

struct delete_many_todays_market_entries_request {
    using response_type = struct delete_many_todays_market_entries_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_entries.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_entry_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_todays_market_entries_response {
    ores::utility::domain::result result;
};

struct list_by_todays_market_config_id_todays_market_entries_request {
    using response_type = struct list_by_todays_market_config_id_todays_market_entries_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_entries.list_by_todays_market_config_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid todays_market_config_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<todays_market_entries_filter> filter;
};

struct list_by_todays_market_config_id_todays_market_entries_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_entry> entries;
    std::uint64_t total;
};

struct list_by_todays_market_collection_id_todays_market_entries_request {
    using response_type = struct list_by_todays_market_collection_id_todays_market_entries_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_entries.list_by_todays_market_collection_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid todays_market_collection_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<todays_market_entries_filter> filter;
};

struct list_by_todays_market_collection_id_todays_market_entries_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_entry> entries;
    std::uint64_t total;
};

struct list_todays_market_entry_versions_request {
    using response_type = struct list_todays_market_entry_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_entries_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_entry_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<todays_market_entry_versions_filter> filter;
};

struct list_todays_market_entry_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_entry> versions;
    std::uint64_t total;
};

struct get_todays_market_entry_version_request {
    using response_type = struct get_todays_market_entry_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_entries_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_entry_version_key key;
};

struct get_todays_market_entry_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_entry> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace todays_market_entry_event_subjects {
inline constexpr std::string_view created = "analytics.v1.todays_market_entries_events.created";
inline constexpr std::string_view updated = "analytics.v1.todays_market_entries_events.updated";
inline constexpr std::string_view deleted = "analytics.v1.todays_market_entries_events.deleted";
}

}

#endif

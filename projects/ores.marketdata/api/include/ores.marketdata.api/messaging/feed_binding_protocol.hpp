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
#ifndef ORES_MARKETDATA_API_MESSAGING_FEED_BINDING_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FEED_BINDING_PROTOCOL_HPP

#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct feed_binding_key {
    std::string source_name;
};

struct feed_binding_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string source_name;
    bool enabled;
};

struct feed_binding_change {
    feed_binding_write write;
    ores::utility::domain::precondition precondition;
};

struct feed_binding_removal {
    feed_binding_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct feed_binding_lookup {
    feed_binding_key key;
    std::optional<ores::marketdata::domain::feed_binding> feed_binding;
};

struct feed_bindings_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct feed_binding_event {
    boost::uuids::uuid event_id;
    feed_binding_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct feed_binding_version_key {
    feed_binding_key feed_binding;
    std::uint32_t version;
};

struct feed_binding_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_feed_bindings_request {
    using response_type = struct list_feed_bindings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.list";
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
    std::optional<feed_bindings_filter> filter;
};

struct list_feed_bindings_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed_binding> feed_bindings;
    std::uint64_t total;
};

struct get_feed_binding_request {
    using response_type = struct get_feed_binding_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_binding_key key;
};

struct get_feed_binding_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed_binding> feed_binding;
};

struct get_many_feed_bindings_request {
    using response_type = struct get_many_feed_bindings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_binding_key> keys;
};

struct get_many_feed_bindings_response {
    ores::utility::domain::result result;
    std::vector<feed_binding_lookup> entries;
};

struct put_feed_binding_request {
    using response_type = struct put_feed_binding_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_binding_change change;
    ores::utility::domain::change_intent intent;
};

struct put_feed_binding_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed_binding> feed_binding;
};

struct put_many_feed_bindings_request {
    using response_type = struct put_many_feed_bindings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_binding_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_feed_bindings_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed_binding> feed_bindings;
};

struct delete_feed_binding_request {
    using response_type = struct delete_feed_binding_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_binding_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_feed_binding_response {
    ores::utility::domain::result result;
};

struct delete_many_feed_bindings_request {
    using response_type = struct delete_many_feed_bindings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_binding_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_feed_bindings_response {
    ores::utility::domain::result result;
};

struct list_feed_binding_versions_request {
    using response_type = struct list_feed_binding_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_binding_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<feed_binding_versions_filter> filter;
};

struct list_feed_binding_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed_binding> versions;
    std::uint64_t total;
};

struct get_feed_binding_version_request {
    using response_type = struct get_feed_binding_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_binding_version_key key;
};

struct get_feed_binding_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed_binding> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace feed_binding_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.feed_bindings_events.created";
inline constexpr std::string_view updated = "marketdata.v1.feed_bindings_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.feed_bindings_events.deleted";
}

}

#endif

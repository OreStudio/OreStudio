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
#ifndef ORES_ORE_API_MESSAGING_SERIES_KEY_SHAPE_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_SERIES_KEY_SHAPE_PROTOCOL_HPP

#include "ores.ore.api/domain/series_key_shape.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::ore::messaging {

struct series_key_shape_key {
    std::string series_type;
};

struct series_key_shape_write {
    std::string series_type;
    int qualifier_depth;
    bool has_point_dimension;
    std::string default_point;
    std::string description;
};

struct series_key_shape_change {
    series_key_shape_write write;
    ores::utility::domain::precondition precondition;
};

struct series_key_shape_removal {
    series_key_shape_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct series_key_shape_lookup {
    series_key_shape_key key;
    std::optional<ores::ore::domain::series_key_shape> series_key_shape;
};

struct series_key_shape_event {
    boost::uuids::uuid event_id;
    series_key_shape_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct series_key_shape_version_key {
    series_key_shape_key series_key_shape;
    std::uint32_t version;
};

struct series_key_shape_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_series_key_shapes_request {
    using response_type = struct list_series_key_shapes_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.list";
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

struct list_series_key_shapes_response {
    ores::utility::domain::result result;
    std::vector<ores::ore::domain::series_key_shape> shapes;
    std::uint64_t total;
};

struct get_series_key_shape_request {
    using response_type = struct get_series_key_shape_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_key_shape_key key;
};

struct get_series_key_shape_response {
    ores::utility::domain::result result;
    std::optional<ores::ore::domain::series_key_shape> series_key_shape;
};

struct get_many_series_key_shapes_request {
    using response_type = struct get_many_series_key_shapes_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_key_shape_key> keys;
};

struct get_many_series_key_shapes_response {
    ores::utility::domain::result result;
    std::vector<series_key_shape_lookup> entries;
};

struct put_series_key_shape_request {
    using response_type = struct put_series_key_shape_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_key_shape_change change;
    ores::utility::domain::change_intent intent;
};

struct put_series_key_shape_response {
    ores::utility::domain::result result;
    ores::ore::domain::series_key_shape series_key_shape;
};

struct put_many_series_key_shapes_request {
    using response_type = struct put_many_series_key_shapes_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_key_shape_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_series_key_shapes_response {
    ores::utility::domain::result result;
    std::vector<ores::ore::domain::series_key_shape> shapes;
};

struct delete_series_key_shape_request {
    using response_type = struct delete_series_key_shape_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_key_shape_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_series_key_shape_response {
    ores::utility::domain::result result;
};

struct delete_many_series_key_shapes_request {
    using response_type = struct delete_many_series_key_shapes_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_key_shape_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_series_key_shapes_response {
    ores::utility::domain::result result;
};

struct list_series_key_shape_versions_request {
    using response_type = struct list_series_key_shape_versions_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_key_shape_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<series_key_shape_versions_filter> filter;
};

struct list_series_key_shape_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::ore::domain::series_key_shape> versions;
    std::uint64_t total;
};

struct get_series_key_shape_version_request {
    using response_type = struct get_series_key_shape_version_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_key_shape_version_key key;
};

struct get_series_key_shape_version_response {
    ores::utility::domain::result result;
    ores::ore::domain::series_key_shape version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace series_key_shape_event_subjects {
inline constexpr std::string_view created = "ore.v1.series_key_shapes_events.created";
inline constexpr std::string_view updated = "ore.v1.series_key_shapes_events.updated";
inline constexpr std::string_view deleted = "ore.v1.series_key_shapes_events.deleted";
}

}

#endif

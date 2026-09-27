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
#ifndef ORES_DQ_API_MESSAGING_ORIGIN_DIMENSION_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_ORIGIN_DIMENSION_PROTOCOL_HPP

#include "ores.dq.api/domain/origin_dimension.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct origin_dimension_key {
    std::string code;
};

struct origin_dimension_write {
    std::string code;
    std::string name;
    std::string description;
};

struct origin_dimension_change {
    origin_dimension_write write;
    ores::utility::domain::precondition precondition;
};

struct origin_dimension_removal {
    origin_dimension_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct origin_dimension_lookup {
    origin_dimension_key key;
    std::optional<ores::dq::domain::origin_dimension> origin_dimension;
};

struct origin_dimension_event {
    boost::uuids::uuid event_id;
    origin_dimension_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct origin_dimension_version_key {
    origin_dimension_key origin_dimension;
    std::uint32_t version;
};

struct origin_dimension_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_origin_dimensions_request {
    using response_type = struct list_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.list";
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

struct list_origin_dimensions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::origin_dimension> dimensions;
    std::uint64_t total;
};

struct get_origin_dimension_request {
    using response_type = struct get_origin_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_key key;
};

struct get_origin_dimension_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::origin_dimension> origin_dimension;
};

struct get_many_origin_dimensions_request {
    using response_type = struct get_many_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<origin_dimension_key> keys;
};

struct get_many_origin_dimensions_response {
    ores::utility::domain::result result;
    std::vector<origin_dimension_lookup> entries;
};

struct put_origin_dimension_request {
    using response_type = struct put_origin_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_change change;
    ores::utility::domain::change_intent intent;
};

struct put_origin_dimension_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::origin_dimension> origin_dimension;
};

struct put_many_origin_dimensions_request {
    using response_type = struct put_many_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<origin_dimension_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_origin_dimensions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::origin_dimension> dimensions;
};

struct delete_origin_dimension_request {
    using response_type = struct delete_origin_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_origin_dimension_response {
    ores::utility::domain::result result;
};

struct delete_many_origin_dimensions_request {
    using response_type = struct delete_many_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<origin_dimension_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_origin_dimensions_response {
    ores::utility::domain::result result;
};

struct list_origin_dimension_versions_request {
    using response_type = struct list_origin_dimension_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<origin_dimension_versions_filter> filter;
};

struct list_origin_dimension_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::origin_dimension> versions;
    std::uint64_t total;
};

struct get_origin_dimension_version_request {
    using response_type = struct get_origin_dimension_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_version_key key;
};

struct get_origin_dimension_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::origin_dimension> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace origin_dimension_event_subjects {
inline constexpr std::string_view created = "dq.v1.origin_dimensions_events.created";
inline constexpr std::string_view updated = "dq.v1.origin_dimensions_events.updated";
inline constexpr std::string_view deleted = "dq.v1.origin_dimensions_events.deleted";
}

}

#endif

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
#ifndef ORES_DQ_API_MESSAGING_SUBJECT_AREA_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_SUBJECT_AREA_PROTOCOL_HPP

#include "ores.dq.api/domain/subject_area.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct subject_area_key {
    std::string name;
    std::string domain_name;
};

struct subject_area_write {
    std::string name;
    std::string domain_name;
    std::string description;
};

struct subject_area_change {
    subject_area_write write;
    ores::utility::domain::precondition precondition;
};

struct subject_area_removal {
    subject_area_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct subject_area_lookup {
    subject_area_key key;
    std::optional<ores::dq::domain::subject_area> subject_area;
};

struct subject_area_event {
    boost::uuids::uuid event_id;
    subject_area_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct subject_area_version_key {
    subject_area_key subject_area;
    std::uint32_t version;
};

struct subject_area_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_subject_areas_request {
    using response_type = struct list_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.list";
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
    std::optional<std::string> as_of;
};

struct list_subject_areas_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::subject_area> areas;
    std::uint64_t total;
};

struct get_subject_area_request {
    using response_type = struct get_subject_area_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_key key;
};

struct get_subject_area_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::subject_area> subject_area;
};

struct get_many_subject_areas_request {
    using response_type = struct get_many_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subject_area_key> keys;
};

struct get_many_subject_areas_response {
    ores::utility::domain::result result;
    std::vector<subject_area_lookup> entries;
};

struct put_subject_area_request {
    using response_type = struct put_subject_area_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_change change;
    ores::utility::domain::change_intent intent;
};

struct put_subject_area_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::subject_area> subject_area;
};

struct put_many_subject_areas_request {
    using response_type = struct put_many_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subject_area_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_subject_areas_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::subject_area> areas;
};

struct delete_subject_area_request {
    using response_type = struct delete_subject_area_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_subject_area_response {
    ores::utility::domain::result result;
};

struct delete_many_subject_areas_request {
    using response_type = struct delete_many_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subject_area_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_subject_areas_response {
    ores::utility::domain::result result;
};

struct list_subject_area_versions_request {
    using response_type = struct list_subject_area_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<subject_area_versions_filter> filter;
};

struct list_subject_area_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::subject_area> versions;
    std::uint64_t total;
};

struct get_subject_area_version_request {
    using response_type = struct get_subject_area_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_version_key key;
};

struct get_subject_area_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::subject_area> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace subject_area_event_subjects {
inline constexpr std::string_view created = "dq.v1.subject_areas_events.created";
inline constexpr std::string_view updated = "dq.v1.subject_areas_events.updated";
inline constexpr std::string_view deleted = "dq.v1.subject_areas_events.deleted";
}

}

#endif

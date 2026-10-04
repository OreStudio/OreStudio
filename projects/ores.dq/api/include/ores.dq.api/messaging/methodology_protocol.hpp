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
#ifndef ORES_DQ_API_MESSAGING_METHODOLOGY_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_METHODOLOGY_PROTOCOL_HPP

#include "ores.dq.api/domain/methodology.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct methodology_key {
    std::string name;
};

struct methodology_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    std::string logic_reference;
    std::string implementation_details;
};

struct methodology_change {
    methodology_write write;
    ores::utility::domain::precondition precondition;
};

struct methodology_removal {
    methodology_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct methodology_lookup {
    methodology_key key;
    std::optional<ores::dq::domain::methodology> methodology;
};

struct methodologies_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct methodology_event {
    boost::uuids::uuid event_id;
    methodology_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct methodology_version_key {
    methodology_key methodology;
    std::uint32_t version;
};

struct methodology_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_methodologies_request {
    using response_type = struct list_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.list";
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
    std::optional<methodologies_filter> filter;
};

struct list_methodologies_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::methodology> methodologies;
    std::uint64_t total;
};

struct get_methodology_request {
    using response_type = struct get_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_key key;
};

struct get_methodology_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::methodology> methodology;
};

struct get_many_methodologies_request {
    using response_type = struct get_many_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<methodology_key> keys;
};

struct get_many_methodologies_response {
    ores::utility::domain::result result;
    std::vector<methodology_lookup> entries;
};

struct put_methodology_request {
    using response_type = struct put_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_change change;
    ores::utility::domain::change_intent intent;
};

struct put_methodology_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::methodology> methodology;
};

struct put_many_methodologies_request {
    using response_type = struct put_many_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<methodology_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_methodologies_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::methodology> methodologies;
};

struct delete_methodology_request {
    using response_type = struct delete_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_methodology_response {
    ores::utility::domain::result result;
};

struct delete_many_methodologies_request {
    using response_type = struct delete_many_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<methodology_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_methodologies_response {
    ores::utility::domain::result result;
};

struct list_methodology_versions_request {
    using response_type = struct list_methodology_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<methodology_versions_filter> filter;
};

struct list_methodology_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::methodology> versions;
    std::uint64_t total;
};

struct get_methodology_version_request {
    using response_type = struct get_methodology_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_version_key key;
};

struct get_methodology_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::methodology> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace methodology_event_subjects {
inline constexpr std::string_view created = "dq.v1.methodologies_events.created";
inline constexpr std::string_view updated = "dq.v1.methodologies_events.updated";
inline constexpr std::string_view deleted = "dq.v1.methodologies_events.deleted";
}

}

#endif

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
#ifndef ORES_SYNTHETIC_API_MESSAGING_YIELD_CURVE_PROCESS_TYPE_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_YIELD_CURVE_PROCESS_TYPE_PROTOCOL_HPP

#include "ores.synthetic.api/domain/yield_curve_process_type.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct yield_curve_process_type_key {
    std::string code;
};

struct yield_curve_process_type_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct yield_curve_process_type_change {
    yield_curve_process_type_write write;
    ores::utility::domain::precondition precondition;
};

struct yield_curve_process_type_removal {
    yield_curve_process_type_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct yield_curve_process_type_lookup {
    yield_curve_process_type_key key;
    std::optional<ores::synthetic::domain::yield_curve_process_type> yield_curve_process_type;
};

struct yield_curve_process_type_event {
    boost::uuids::uuid event_id;
    yield_curve_process_type_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct yield_curve_process_type_version_key {
    yield_curve_process_type_key yield_curve_process_type;
    std::uint32_t version;
};

struct yield_curve_process_type_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_yield_curve_process_types_request {
    using response_type = struct list_yield_curve_process_types_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.yield_curve_process_types.list";
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

struct list_yield_curve_process_types_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::yield_curve_process_type> process_types;
    std::uint64_t total;
};

struct get_yield_curve_process_type_request {
    using response_type = struct get_yield_curve_process_type_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.yield_curve_process_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_type_key key;
};

struct get_yield_curve_process_type_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::yield_curve_process_type> yield_curve_process_type;
};

struct get_many_yield_curve_process_types_request {
    using response_type = struct get_many_yield_curve_process_types_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<yield_curve_process_type_key> keys;
};

struct get_many_yield_curve_process_types_response {
    ores::utility::domain::result result;
    std::vector<yield_curve_process_type_lookup> entries;
};

struct put_yield_curve_process_type_request {
    using response_type = struct put_yield_curve_process_type_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.yield_curve_process_types.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_type_change change;
    ores::utility::domain::change_intent intent;
};

struct put_yield_curve_process_type_response {
    ores::utility::domain::result result;
    ores::synthetic::domain::yield_curve_process_type yield_curve_process_type;
};

struct put_many_yield_curve_process_types_request {
    using response_type = struct put_many_yield_curve_process_types_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_types.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<yield_curve_process_type_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_yield_curve_process_types_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::yield_curve_process_type> process_types;
};

struct delete_yield_curve_process_type_request {
    using response_type = struct delete_yield_curve_process_type_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_types.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_type_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_yield_curve_process_type_response {
    ores::utility::domain::result result;
};

struct delete_many_yield_curve_process_types_request {
    using response_type = struct delete_many_yield_curve_process_types_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_types.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<yield_curve_process_type_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_yield_curve_process_types_response {
    ores::utility::domain::result result;
};

struct list_yield_curve_process_type_versions_request {
    using response_type = struct list_yield_curve_process_type_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_types_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_type_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<yield_curve_process_type_versions_filter> filter;
};

struct list_yield_curve_process_type_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::yield_curve_process_type> versions;
    std::uint64_t total;
};

struct get_yield_curve_process_type_version_request {
    using response_type = struct get_yield_curve_process_type_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_types_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_type_version_key key;
};

struct get_yield_curve_process_type_version_response {
    ores::utility::domain::result result;
    ores::synthetic::domain::yield_curve_process_type version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace yield_curve_process_type_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.yield_curve_process_types_events.created";
inline constexpr std::string_view updated = "synthetic.v1.yield_curve_process_types_events.updated";
inline constexpr std::string_view deleted = "synthetic.v1.yield_curve_process_types_events.deleted";
}

}

#endif

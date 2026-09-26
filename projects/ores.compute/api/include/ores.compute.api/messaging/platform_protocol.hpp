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
#ifndef ORES_COMPUTE_API_MESSAGING_PLATFORM_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_PLATFORM_PROTOCOL_HPP

#include "ores.compute.api/domain/platform.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct platform_key {
    std::string code;
};

struct platform_write {
    boost::uuids::uuid id;
    std::string code;
    std::string display_name;
    std::string description;
    std::string os_family;
    std::string cpu_arch;
    std::string abi;
    bool is_active;
};

struct platform_change {
    platform_write write;
    ores::utility::domain::precondition precondition;
};

struct platform_removal {
    platform_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct platform_lookup {
    platform_key key;
    std::optional<ores::compute::domain::platform> platform;
};

struct platform_event {
    boost::uuids::uuid event_id;
    platform_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct platform_version_key {
    platform_key platform;
    std::uint32_t version;
};

struct platform_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_platforms_request {
    using response_type = struct list_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.list";
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

struct list_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::platform> platforms;
    std::uint64_t total;
};

struct get_platform_request {
    using response_type = struct get_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_key key;
};

struct get_platform_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::platform> platform;
};

struct get_many_platforms_request {
    using response_type = struct get_many_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<platform_key> keys;
};

struct get_many_platforms_response {
    ores::utility::domain::result result;
    std::vector<platform_lookup> entries;
};

struct put_platform_request {
    using response_type = struct put_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_change change;
    ores::utility::domain::change_intent intent;
};

struct put_platform_response {
    ores::utility::domain::result result;
    ores::compute::domain::platform platform;
};

struct put_many_platforms_request {
    using response_type = struct put_many_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<platform_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::platform> platforms;
};

struct delete_platform_request {
    using response_type = struct delete_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_platform_response {
    ores::utility::domain::result result;
};

struct delete_many_platforms_request {
    using response_type = struct delete_many_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<platform_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_platforms_response {
    ores::utility::domain::result result;
};

struct list_platform_versions_request {
    using response_type = struct list_platform_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<platform_versions_filter> filter;
};

struct list_platform_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::platform> versions;
    std::uint64_t total;
};

struct get_platform_version_request {
    using response_type = struct get_platform_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_version_key key;
};

struct get_platform_version_response {
    ores::utility::domain::result result;
    ores::compute::domain::platform version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace platform_event_subjects {
inline constexpr std::string_view created = "compute.v1.platforms_events.created";
inline constexpr std::string_view updated = "compute.v1.platforms_events.updated";
inline constexpr std::string_view deleted = "compute.v1.platforms_events.deleted";
}

}

#endif

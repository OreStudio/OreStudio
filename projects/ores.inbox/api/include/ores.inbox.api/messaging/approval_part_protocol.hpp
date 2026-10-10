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
#ifndef ORES_INBOX_API_MESSAGING_APPROVAL_PART_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_APPROVAL_PART_PROTOCOL_HPP

#include "ores.inbox.api/domain/approval_part.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::messaging {

struct approval_part_key {
    std::string code;
};

struct approval_part_write {
    std::string code;
    std::string name;
    std::string description;
    std::string decide_permission_code;
    int answer_order;
    int display_order;
};

struct approval_part_change {
    approval_part_write write;
    ores::utility::domain::precondition precondition;
};

struct approval_part_removal {
    approval_part_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct approval_part_lookup {
    approval_part_key key;
    std::optional<ores::inbox::domain::approval_part> approval_part;
};

struct approval_parts_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct approval_part_event {
    boost::uuids::uuid event_id;
    approval_part_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct approval_part_version_key {
    approval_part_key approval_part;
    std::uint32_t version;
};

struct approval_part_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_approval_parts_request {
    using response_type = struct list_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.list";
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
    std::optional<approval_parts_filter> filter;
    std::optional<std::string> as_of;
};

struct list_approval_parts_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_part> parts;
    std::uint64_t total;
};

struct get_approval_part_request {
    using response_type = struct get_approval_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_key key;
};

struct get_approval_part_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_part> approval_part;
};

struct get_many_approval_parts_request {
    using response_type = struct get_many_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_part_key> keys;
};

struct get_many_approval_parts_response {
    ores::utility::domain::result result;
    std::vector<approval_part_lookup> entries;
};

struct put_approval_part_request {
    using response_type = struct put_approval_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_change change;
    ores::utility::domain::change_intent intent;
};

struct put_approval_part_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_part> approval_part;
};

struct put_many_approval_parts_request {
    using response_type = struct put_many_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_part_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_approval_parts_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_part> parts;
};

struct delete_approval_part_request {
    using response_type = struct delete_approval_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_approval_part_response {
    ores::utility::domain::result result;
};

struct delete_many_approval_parts_request {
    using response_type = struct delete_many_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_part_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_approval_parts_response {
    ores::utility::domain::result result;
};

struct list_approval_part_versions_request {
    using response_type = struct list_approval_part_versions_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<approval_part_versions_filter> filter;
};

struct list_approval_part_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_part> versions;
    std::uint64_t total;
};

struct get_approval_part_version_request {
    using response_type = struct get_approval_part_version_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_version_key key;
};

struct get_approval_part_version_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_part> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace approval_part_event_subjects {
inline constexpr std::string_view created = "inbox.v1.approval_parts_events.created";
inline constexpr std::string_view updated = "inbox.v1.approval_parts_events.updated";
inline constexpr std::string_view deleted = "inbox.v1.approval_parts_events.deleted";
}

}

#endif

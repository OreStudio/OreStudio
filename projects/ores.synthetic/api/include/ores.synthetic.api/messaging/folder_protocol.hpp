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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FOLDER_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FOLDER_PROTOCOL_HPP

#include "ores.synthetic.api/domain/folder.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct folder_key {
    boost::uuids::uuid id;
};

struct folder_write {
    boost::uuids::uuid id;
    std::optional<boost::uuids::uuid> parent_id;
    std::string name;
    std::string kind;
    std::optional<boost::uuids::uuid> collection_id;
};

struct folder_change {
    folder_write write;
    ores::utility::domain::precondition precondition;
};

struct folder_removal {
    folder_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct folder_lookup {
    folder_key key;
    std::optional<ores::synthetic::domain::folder> folder;
};

struct folder_event {
    boost::uuids::uuid event_id;
    folder_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct folder_version_key {
    folder_key folder;
    std::uint32_t version;
};

struct folder_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_folders_request {
    using response_type = struct list_folders_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.list";
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

struct list_folders_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::folder> folders;
    std::uint64_t total;
};

struct get_folder_request {
    using response_type = struct get_folder_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    folder_key key;
};

struct get_folder_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::folder> folder;
};

struct get_many_folders_request {
    using response_type = struct get_many_folders_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<folder_key> keys;
};

struct get_many_folders_response {
    ores::utility::domain::result result;
    std::vector<folder_lookup> entries;
};

struct put_folder_request {
    using response_type = struct put_folder_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    folder_change change;
    ores::utility::domain::change_intent intent;
};

struct put_folder_response {
    ores::utility::domain::result result;
    ores::synthetic::domain::folder folder;
};

struct put_many_folders_request {
    using response_type = struct put_many_folders_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<folder_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_folders_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::folder> folders;
};

struct delete_folder_request {
    using response_type = struct delete_folder_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    folder_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_folder_response {
    ores::utility::domain::result result;
};

struct delete_many_folders_request {
    using response_type = struct delete_many_folders_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<folder_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_folders_response {
    ores::utility::domain::result result;
};

struct list_folder_versions_request {
    using response_type = struct list_folder_versions_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    folder_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<folder_versions_filter> filter;
};

struct list_folder_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::folder> versions;
    std::uint64_t total;
};

struct get_folder_version_request {
    using response_type = struct get_folder_version_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    folder_version_key key;
};

struct get_folder_version_response {
    ores::utility::domain::result result;
    ores::synthetic::domain::folder version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace folder_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.folders_events.created";
inline constexpr std::string_view updated = "synthetic.v1.folders_events.updated";
inline constexpr std::string_view deleted = "synthetic.v1.folders_events.deleted";
}

}

#endif

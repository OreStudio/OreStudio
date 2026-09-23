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
#ifndef ORES_ASSETS_API_MESSAGING_TAG_PROTOCOL_HPP
#define ORES_ASSETS_API_MESSAGING_TAG_PROTOCOL_HPP

#include "ores.assets.api/domain/tag.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::assets::messaging {

struct tag_key {
    std::string name;
};

struct tag_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
};

struct tag_change {
    tag_write write;
    ores::utility::domain::precondition precondition;
};

struct tag_removal {
    tag_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tag_lookup {
    tag_key key;
    std::optional<ores::assets::domain::tag> tag;
};

struct tag_event {
    boost::uuids::uuid event_id;
    tag_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tag_version_key {
    tag_key tag;
    std::uint32_t version;
};

struct tag_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tags_request {
    using response_type = struct list_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.list";
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

struct list_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::tag> tags;
    std::uint64_t total;
};

struct get_tag_request {
    using response_type = struct get_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_key key;
};

struct get_tag_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::tag> tag;
};

struct get_many_tags_request {
    using response_type = struct get_many_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tag_key> keys;
};

struct get_many_tags_response {
    ores::utility::domain::result result;
    std::vector<tag_lookup> entries;
};

struct put_tag_request {
    using response_type = struct put_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tag_response {
    ores::utility::domain::result result;
    ores::assets::domain::tag tag;
};

struct put_many_tags_request {
    using response_type = struct put_many_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tag_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::tag> tags;
};

struct delete_tag_request {
    using response_type = struct delete_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tag_response {
    ores::utility::domain::result result;
};

struct delete_many_tags_request {
    using response_type = struct delete_many_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tag_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tags_response {
    ores::utility::domain::result result;
};

struct list_tag_versions_request {
    using response_type = struct list_tag_versions_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tag_versions_filter> filter;
};

struct list_tag_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::tag> versions;
    std::uint64_t total;
};

struct get_tag_version_request {
    using response_type = struct get_tag_version_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_version_key key;
};

struct get_tag_version_response {
    ores::utility::domain::result result;
    ores::assets::domain::tag version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tag_event_subjects {
inline constexpr std::string_view created = "assets.v1.tags_events.created";
inline constexpr std::string_view updated = "assets.v1.tags_events.updated";
inline constexpr std::string_view deleted = "assets.v1.tags_events.deleted";
}

}

#endif

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
#ifndef ORES_ASSETS_API_MESSAGING_IMAGE_PROTOCOL_HPP
#define ORES_ASSETS_API_MESSAGING_IMAGE_PROTOCOL_HPP

#include "ores.assets.api/domain/image.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::assets::messaging {

struct image_key {
    std::string key;
};

struct image_write {
    boost::uuids::uuid id;
    std::string key;
    std::string description;
    std::string mime_type;
    std::string data;
};

struct image_change {
    image_write write;
    ores::utility::domain::precondition precondition;
};

struct image_removal {
    image_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct image_lookup {
    image_key key;
    std::optional<ores::assets::domain::image> image;
};

struct image_event {
    boost::uuids::uuid event_id;
    image_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct image_version_key {
    image_key image;
    std::uint32_t version;
};

struct image_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_images_request {
    using response_type = struct list_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.list";
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

struct list_images_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image> images;
    std::uint64_t total;
};

struct get_image_request {
    using response_type = struct get_image_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_key key;
};

struct get_image_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::image> image;
};

struct get_many_images_request {
    using response_type = struct get_many_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_key> keys;
};

struct get_many_images_response {
    ores::utility::domain::result result;
    std::vector<image_lookup> entries;
};

struct put_image_request {
    using response_type = struct put_image_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_change change;
    ores::utility::domain::change_intent intent;
};

struct put_image_response {
    ores::utility::domain::result result;
    ores::assets::domain::image image;
};

struct put_many_images_request {
    using response_type = struct put_many_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_images_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image> images;
};

struct delete_image_request {
    using response_type = struct delete_image_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_image_response {
    ores::utility::domain::result result;
};

struct delete_many_images_request {
    using response_type = struct delete_many_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_images_response {
    ores::utility::domain::result result;
};

struct list_image_versions_request {
    using response_type = struct list_image_versions_response;
    static constexpr std::string_view nats_subject = "assets.v1.images_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<image_versions_filter> filter;
};

struct list_image_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image> versions;
    std::uint64_t total;
};

struct get_image_version_request {
    using response_type = struct get_image_version_response;
    static constexpr std::string_view nats_subject = "assets.v1.images_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_version_key key;
};

struct get_image_version_response {
    ores::utility::domain::result result;
    ores::assets::domain::image version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace image_event_subjects {
inline constexpr std::string_view created = "assets.v1.images_events.created";
inline constexpr std::string_view updated = "assets.v1.images_events.updated";
inline constexpr std::string_view deleted = "assets.v1.images_events.deleted";
}

}

#endif

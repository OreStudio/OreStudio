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
#ifndef ORES_ASSETS_API_MESSAGING_IMAGE_TAG_PROTOCOL_HPP
#define ORES_ASSETS_API_MESSAGING_IMAGE_TAG_PROTOCOL_HPP

#include "ores.assets.api/domain/image_tag.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::assets::messaging {

struct image_tag_key {
    boost::uuids::uuid image_id;
    boost::uuids::uuid tag_id;
};

struct image_tag_write {
    boost::uuids::uuid image_id;
    boost::uuids::uuid tag_id;
    std::string assigned_by;
    std::chrono::system_clock::time_point assigned_at;
};

struct image_tag_change {
    image_tag_write write;
    ores::utility::domain::precondition precondition;
};

struct image_tag_removal {
    image_tag_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct image_tag_lookup {
    image_tag_key key;
    std::optional<ores::assets::domain::image_tag> image_tag;
};

struct image_tags_filter {
    std::optional<boost::uuids::uuid> image_id;
};

struct list_image_tags_request {
    using response_type = struct list_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.list";
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
    std::optional<image_tags_filter> filter;
};

struct list_image_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image_tag> image_tags;
    std::uint64_t total;
};

struct get_image_tag_request {
    using response_type = struct get_image_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_tag_key key;
};

struct get_image_tag_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::image_tag> image_tag;
};

struct get_many_image_tags_request {
    using response_type = struct get_many_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_tag_key> keys;
};

struct get_many_image_tags_response {
    ores::utility::domain::result result;
    std::vector<image_tag_lookup> entries;
};

struct put_image_tag_request {
    using response_type = struct put_image_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_tag_change change;
    ores::utility::domain::change_intent intent;
};

struct put_image_tag_response {
    ores::utility::domain::result result;
    ores::assets::domain::image_tag image_tag;
};

struct put_many_image_tags_request {
    using response_type = struct put_many_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_tag_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_image_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image_tag> image_tags;
};

struct delete_image_tag_request {
    using response_type = struct delete_image_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_tag_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_image_tag_response {
    ores::utility::domain::result result;
};

struct delete_many_image_tags_request {
    using response_type = struct delete_many_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_tag_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_image_tags_response {
    ores::utility::domain::result result;
};

struct list_by_image_id_image_tags_request {
    using response_type = struct list_by_image_id_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.list_by_image_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid image_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<image_tags_filter> filter;
};

struct list_by_image_id_image_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image_tag> image_tags;
    std::uint64_t total;
};

}

#endif

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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ASSETS_DOMAIN_IMAGE_TAG_HPP
#define ORES_ASSETS_DOMAIN_IMAGE_TAG_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::assets::domain {

/**
 * @brief Links an image to a tag.
 *
 * Junction table linking images to tags. An image carries any number of
 * tags, and a tag classifies any number of images. The association itself
 * has no natural key: a row is identified by the image and tag pair it
 * links, and the row records who made the association and when.
 */
struct image_tag final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ID of the tagged image.
     *
     * References ores_assets_images_tbl.id (soft FK).
     */
    boost::uuids::uuid image_id;

    /**
     * @brief ID of the tag applied to the image.
     *
     * References ores_assets_tags_tbl.id (soft FK).
     */
    boost::uuids::uuid tag_id;

    /**
     * @brief Username of the account that made the association. The insert trigger stamps it from
     * the acting user, so a client never sets it.
     */
    std::string assigned_by;

    /**
     * @brief When the association was made. The insert trigger stamps it, so a client never sets
     * it.
     */
    std::chrono::system_clock::time_point assigned_at;

    /**
     * @brief Username of the person who last modified this image tag.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality, on the same terms as an entity's.
     */
    friend bool operator==(const image_tag&, const image_tag&) = default;
};

/**
 * @brief Dispatch-key identifier for image_tag, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const image_tag&) {
    return "ores.assets.image_tag";
}

}

#endif

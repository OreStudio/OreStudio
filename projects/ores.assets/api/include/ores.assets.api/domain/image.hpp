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
#ifndef ORES_ASSETS_API_DOMAIN_IMAGE_HPP
#define ORES_ASSETS_API_DOMAIN_IMAGE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::assets::domain {

/**
 * @brief An image published to the platform.
 *
 * An image is a named document the platform renders, such as a currency flag
 * or a commodity icon. The bytes are format-agnostic: an SVG document, a
 * JPEG, or any other media type mime_type names. The database stores them
 * base64-encoded in a text column, and the domain type carries the raw
 * bytes. Images carry tags through the image_tag junction, and an image
 * arrives either by upload or from the data-quality publish path.
 */
struct image final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate identifier for the image.
     */
    boost::uuids::uuid id;

    /**
     * @brief Human-meaningful image code, unique within the tenant. Callers ask for an image by
     * this code, for example GBP or gold. The column is named code rather than key because the
     * notify-trigger template derives a changed_<natural key> variable from the column name and
     * already holds a changed_key of its own, so a natural key called key declares the name twice
     * and the trigger fails to create.
     */
    std::string code;

    /**
     * @brief What the image depicts.
     */
    std::string description;

    /**
     * @brief Media type of the image data, for example image/svg+xml or image/jpeg. SVG is the
     * default because most published images are SVG, but an image may carry any media type. The
     * generator block restates the default because the generator template only honours
     * default_value when a column also declares a generator, and otherwise invents a random word.
     */
    std::string mime_type = "image/svg+xml";

    /**
     * @brief Raw image bytes: SVG markup as UTF-8 bytes, or a binary format such as JPEG. The
     * database column is text and holds the bytes base64-encoded; the domain member is the raw byte
     * vector, and the mapper performs the base64 hop in both directions.
     */
    std::vector<std::uint8_t> data;

    /**
     * @brief Username of the person who last modified this asset image.
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
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const image&, const image&) = default;
};

/**
 * @brief Dispatch-key identifier for image, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const image&) {
    return "ores.assets.image";
}

}

#endif

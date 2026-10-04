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
#ifndef ORES_ASSETS_CORE_VALIDATION_IMAGE_UPLOAD_VALIDATOR_HPP
#define ORES_ASSETS_CORE_VALIDATION_IMAGE_UPLOAD_VALIDATOR_HPP

#include "ores.assets.core/export.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::assets::validation {

/**
 * @brief The rules an uploaded image must satisfy, as data.
 *
 * The rules are stated once, here, and the validator enforces exactly what it
 * states. A caller that shows the rules to a person asks for this record
 * rather than keeping a copy of them, so the rules on screen cannot drift from
 * the rules the server applies.
 */
struct ORES_ASSETS_CORE_EXPORT image_upload_policy {
    /// The media types an upload accepts.
    std::vector<std::string> formats;
    /// The largest image an upload accepts, in bytes.
    std::size_t max_size_bytes = 0;
    /// The smallest width an upload accepts, in pixels.
    std::size_t min_width = 0;
    /// The smallest height an upload accepts, in pixels.
    std::size_t min_height = 0;
};

/**
 * @brief Why an upload was refused, in the terms a caller branches on.
 */
struct ORES_ASSETS_CORE_EXPORT image_upload_refusal {
    /// The stable code a caller branches on.
    std::string code;
    /// The request field the refusal is about: mime_type or data.
    std::string field;
    /// The refusal in words, built from the policy it enforces.
    std::string message;
};

/**
 * @brief Validates uploaded images against the image upload policy.
 *
 * The validator reads the image format from the bytes themselves, never from
 * the stated media type alone, so bytes that are not the stated type are
 * refused rather than stored.
 */
class ORES_ASSETS_CORE_EXPORT image_upload_validator {
public:
    /**
     * @brief The policy the validator enforces.
     *
     * The single statement of the rules: validate() applies this record and
     * nothing else, so a caller that reads it states what the server applies.
     */
    [[nodiscard]] static image_upload_policy policy();

    /**
     * @brief Validates an upload against the policy.
     *
     * @param mime_type The media type the caller states.
     * @param data The image bytes as uploaded, already decoded.
     * @return Nothing when the upload satisfies the policy, or the refusal.
     */
    [[nodiscard]] static std::optional<image_upload_refusal>
    validate(const std::string& mime_type, const std::vector<std::uint8_t>& data);
};

}

#endif

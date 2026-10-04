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
#ifndef ORES_ASSETS_API_MESSAGING_IMAGE_OPERATIONS_PROTOCOL_HPP
#define ORES_ASSETS_API_MESSAGING_IMAGE_OPERATIONS_PROTOCOL_HPP

#include "ores.utility/domain/protocol.hpp"
#include <string>
#include <vector>

namespace ores::assets::messaging {

/**
 * @brief A member's upload of an image.
 *
 * The session scopes the write: the image lands in the caller's tenant, and
 * any signed-in account may upload. The request carries no account id, because
 * the upload does not set a photo: the self write that references the returned
 * image id sets one.
 *
 * The stated media type must be one the image upload policy accepts, and one
 * the bytes themselves carry: bytes that are not the stated type are refused
 * rather than stored. A refused upload is refused with one of the codes the
 * picker branches on -- unsupported_media_type, image_too_large,
 * image_too_small or invalid_image -- and the refusal names the field it is
 * about.
 */
struct upload_image_request {
    using response_type = struct upload_image_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.upload";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string mime_type;
    /**
     * @brief The image bytes, base64-encoded.
     */
    std::string data;
};

struct upload_image_response {
    ores::utility::domain::result result;
    /**
     * @brief The id of the stored image, stated only when the outcome is ok.
     *
     * The bytes are not answered back: the caller holds them, and a reader that
     * wants them asks the image read by id.
     */
    std::string image_id;
};

/**
 * @brief The rule an uploaded image must satisfy, as the server applies it.
 *
 * Read from the validator that enforces it, so a screen that states the rule
 * states the one the server applies rather than a copy of it.
 */
struct get_image_upload_policy_request {
    using response_type = struct get_image_upload_policy_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.upload-policy";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_image_upload_policy_response {
    ores::utility::domain::result result;
    /**
     * @brief The media types an upload accepts, for example image/png.
     */
    std::vector<std::string> formats;
    /**
     * @brief The largest image an upload accepts, in bytes.
     */
    int max_size_bytes;
    /**
     * @brief The smallest width an upload accepts, in pixels.
     */
    int min_width;
    /**
     * @brief The smallest height an upload accepts, in pixels.
     */
    int min_height;
};

}

#endif

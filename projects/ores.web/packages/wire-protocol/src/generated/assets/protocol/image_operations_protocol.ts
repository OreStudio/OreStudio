/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { Result } from '../../../utility/protocol.js';

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
export interface UploadImageRequest {
    mime_type: string;
    /**
     * @brief The image bytes, base64-encoded.
     */
    data: string;
}

export interface UploadImageResponse {
    result: Result;
    /**
     * @brief The id of the stored image, stated only when the outcome is ok.
     *
     * The bytes are not answered back: the caller holds them, and a reader that
     * wants them asks the image read by id.
     */
    image_id: string;
}

/**
 * @brief The rule an uploaded image must satisfy, as the server applies it.
 *
 * Read from the validator that enforces it, so a screen that states the rule
 * states the one the server applies rather than a copy of it.
 */
export interface GetImageUploadPolicyRequest {}

export interface GetImageUploadPolicyResponse {
    result: Result;
    /**
     * @brief The media types an upload accepts, for example image/png.
     */
    formats: string[];
    /**
     * @brief The largest image an upload accepts, in bytes.
     */
    max_size_bytes: number;
    /**
     * @brief The smallest width an upload accepts, in pixels.
     */
    min_width: number;
    /**
     * @brief The smallest height an upload accepts, in pixels.
     */
    min_height: number;
}

export const subjects = {
    upload_image_request: 'assets.v1.images.upload',
    get_image_upload_policy_request: 'assets.v1.images.upload-policy',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    upload_image_request: true,
    get_image_upload_policy_request: true,
} as const;

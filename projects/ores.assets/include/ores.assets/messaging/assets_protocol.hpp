/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_ASSETS_MESSAGING_ASSETS_PROTOCOL_HPP
#define ORES_ASSETS_MESSAGING_ASSETS_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.assets/domain/image.hpp"

namespace ores::assets::messaging {

/**
 * @brief Maximum number of images that can be requested in a single get_images_request.
 */
constexpr std::uint32_t MAX_IMAGES_PER_REQUEST = 100;

/**
 * @brief Request to retrieve images by their IDs.
 *
 * Supports batched retrieval with a maximum of MAX_IMAGES_PER_REQUEST (100) IDs.
 */
struct get_images_request final {
    std::vector<std::string> image_ids;

    std::vector<std::byte> serialize() const;
    static std::expected<get_images_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_images_request& v);

/**
 * @brief Response containing requested images.
 */
struct get_images_response final {
    std::vector<domain::image> images;

    std::vector<std::byte> serialize() const;
    static std::expected<get_images_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_images_response& v);

/**
 * @brief Request to list all available images.
 *
 * Returns metadata for all images without the SVG data (to reduce payload size).
 */
struct list_images_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<list_images_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_images_request& v);

/**
 * @brief Metadata for an image (without SVG data).
 */
struct image_info final {
    std::string image_id;
    std::string key;
    std::string description;
};

std::ostream& operator<<(std::ostream& s, const image_info& v);

/**
 * @brief Response containing metadata for all available images.
 */
struct list_images_response final {
    std::vector<image_info> images;

    std::vector<std::byte> serialize() const;
    static std::expected<list_images_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_images_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for get_images_request.
 */
template<>
struct message_traits<assets::messaging::get_images_request> {
    using request_type = assets::messaging::get_images_request;
    using response_type = assets::messaging::get_images_response;
    static constexpr message_type request_message_type =
        message_type::get_images_request;
};

/**
 * @brief Message traits specialization for list_images_request.
 */
template<>
struct message_traits<assets::messaging::list_images_request> {
    using request_type = assets::messaging::list_images_request;
    using response_type = assets::messaging::list_images_response;
    static constexpr message_type request_message_type =
        message_type::list_images_request;
};

}

#endif

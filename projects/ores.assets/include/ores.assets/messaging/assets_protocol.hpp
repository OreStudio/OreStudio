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
#ifndef ORES_ASSETS_MESSAGING_ASSETS_PROTOCOL_HPP
#define ORES_ASSETS_MESSAGING_ASSETS_PROTOCOL_HPP

#include <chrono>
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include "ores.assets/domain/image.hpp"

namespace ores::assets::messaging {

/**
 * @brief Maximum number of images to request in a single batch.
 */
constexpr std::size_t MAX_IMAGES_PER_REQUEST = 50;

/**
 * @brief Lightweight image metadata (no SVG data) for list responses.
 */
struct image_info {
    std::string image_id;
    std::string key;
    std::string description;
};

struct get_images_request {
    using response_type = struct get_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.get";
    std::vector<std::string> image_ids;
};

struct get_images_response {
    std::vector<ores::assets::domain::image> images;
};

struct list_images_request {
    using response_type = struct list_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.list";
    std::optional<std::chrono::system_clock::time_point> modified_since;
};

/**
 * @brief Response for list_images_request.
 *
 * Returns lightweight metadata. Use get_images_request to fetch SVG data.
 */
struct list_images_response {
    std::vector<image_info> images;
};

struct save_image_request {
    using response_type = struct save_image_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.save";
    ores::assets::domain::image data;
};

struct save_image_response {
    bool success = false;
    std::string message;
};

}

#endif

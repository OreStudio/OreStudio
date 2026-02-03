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
#ifndef ORES_ASSETS_DOMAIN_IMAGE_TAG_HPP
#define ORES_ASSETS_DOMAIN_IMAGE_TAG_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::assets::domain {

/**
 * @brief Represents an association between an image and a tag.
 *
 * This is a junction type for the many-to-many relationship between images and tags.
 */
struct image_tag final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    boost::uuids::uuid tenant_id;

    /**
     * @brief The image identifier (UUID).
     */
    boost::uuids::uuid image_id;

    /**
     * @brief The tag identifier (UUID).
     */
    boost::uuids::uuid tag_id;

    /**
     * @brief Username of the person who assigned this tag to the image.
     */
    std::string assigned_by;

    /**
     * @brief Timestamp when this tag was assigned to the image.
     */
    std::chrono::system_clock::time_point assigned_at;
};

}

#endif

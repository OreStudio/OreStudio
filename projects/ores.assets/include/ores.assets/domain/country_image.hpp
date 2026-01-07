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
#ifndef ORES_ASSETS_DOMAIN_COUNTRY_IMAGE_HPP
#define ORES_ASSETS_DOMAIN_COUNTRY_IMAGE_HPP

#include <string>

namespace ores::assets::domain {

/**
 * @brief Represents an association between a country and its flag image.
 *
 * Each country has one primary image (typically a flag).
 */
struct country_image final {
    /**
     * @brief The country ISO 3166-1 alpha-2 code (e.g., 'US', 'GB', 'RO').
     */
    std::string alpha2_code;

    /**
     * @brief The image identifier (UUID).
     */
    std::string image_id;

    /**
     * @brief Username of the person who assigned this image to the country.
     */
    std::string assigned_by;

    /**
     * @brief Timestamp when this image was assigned to the country.
     */
    std::string assigned_at;
};

}

#endif

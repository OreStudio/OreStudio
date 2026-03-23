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
#ifndef ORES_ASSETS_REPOSITORY_IMAGE_TAG_ENTITY_HPP
#define ORES_ASSETS_REPOSITORY_IMAGE_TAG_ENTITY_HPP

#include <string>
#include <optional>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::assets::repository {

/**
 * @brief Represents an image-tag association in the database.
 *
 * Junction table entity for many-to-many relationship between images and tags.
 */
struct image_tag_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_assets_image_tags_tbl";

    std::string tenant_id;
    sqlgen::PrimaryKey<std::string> image_id;
    sqlgen::PrimaryKey<std::string> tag_id;
    std::string assigned_by;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> assigned_at = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const image_tag_entity& v);

}

#endif

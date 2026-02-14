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
#ifndef ORES_ASSETS_GENERATORS_IMAGE_TAG_GENERATOR_HPP
#define ORES_ASSETS_GENERATORS_IMAGE_TAG_GENERATOR_HPP

#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.assets/domain/image_tag.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::assets::generators {

/**
 * @brief Generates a synthetic image-tag association.
 */
domain::image_tag generate_synthetic_image_tag(
    utility::generation::generation_context& ctx);

/**
 * @brief Generates a synthetic image-tag with specific IDs.
 */
domain::image_tag generate_synthetic_image_tag(
    utility::generation::generation_context& ctx,
    const boost::uuids::uuid& image_id,
    const boost::uuids::uuid& tag_id);

/**
 * @brief Generates N synthetic image-tag associations. May contain duplicates.
 */
std::vector<domain::image_tag> generate_synthetic_image_tags(std::size_t n,
    utility::generation::generation_context& ctx);

}

#endif

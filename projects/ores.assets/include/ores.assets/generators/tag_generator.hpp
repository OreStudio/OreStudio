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
#ifndef ORES_ASSETS_GENERATORS_TAG_GENERATOR_HPP
#define ORES_ASSETS_GENERATORS_TAG_GENERATOR_HPP

#include <vector>
#include "ores.assets/domain/tag.hpp"

namespace ores::assets::generators {

/**
 * @brief Generates a synthetic tag.
 */
domain::tag generate_synthetic_tag();

/**
 * @brief Generates N synthetic tags. May contain duplicates.
 */
std::vector<domain::tag> generate_synthetic_tags(std::size_t n);

/**
 * @brief Generates N synthetic tags with unique names.
 */
std::vector<domain::tag> generate_unique_synthetic_tags(std::size_t n);

}

#endif

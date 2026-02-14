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
#ifndef ORES_REFDATA_GENERATORS_COUNTRY_GENERATOR_HPP
#define ORES_REFDATA_GENERATORS_COUNTRY_GENERATOR_HPP

#include <vector>
#include "ores.refdata/domain/country.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::refdata::generators {

/**
 * @brief Generates a set of fictional countries.
 *
 * These are intentionally fake countries with made-up codes that do not
 * correspond to any real ISO 3166-1 codes. Useful for testing and demo
 * purposes where real country data should not be used.
 *
 * @param n Number of countries to generate. If n is 0 or greater than the
 *          available set (50), returns all available fictional countries.
 */
std::vector<domain::country> generate_fictional_countries(std::size_t n,
    utility::generation::generation_context& ctx);

}

#endif

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
#ifndef ORES_REFDATA_GENERATORS_BUSINESS_CENTRE_GENERATOR_HPP
#define ORES_REFDATA_GENERATORS_BUSINESS_CENTRE_GENERATOR_HPP

#include <vector>
#include "ores.refdata/domain/business_centre.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::refdata::generators {

/**
 * @brief Generates a set of fictional business centres.
 *
 * These are intentionally fictional business centres with made-up FpML-style
 * codes that do not correspond to any real financial centres. Useful for
 * testing and demo purposes where real business centre data should not be used.
 *
 * @param n Number of business centres to generate. If n is 0 or greater than
 *          the available set (20), returns all available fictional centres.
 */
std::vector<domain::business_centre>
generate_fictional_business_centres(std::size_t n,
    utility::generation::generation_context& ctx);

}

#endif

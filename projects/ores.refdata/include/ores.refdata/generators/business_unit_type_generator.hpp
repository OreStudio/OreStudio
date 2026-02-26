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
#ifndef ORES_REFDATA_GENERATORS_BUSINESS_UNIT_TYPE_GENERATOR_HPP
#define ORES_REFDATA_GENERATORS_BUSINESS_UNIT_TYPE_GENERATOR_HPP

#include <vector>
#include "ores.refdata/domain/business_unit_type.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::refdata::generators {

/**
 * @brief Generates a synthetic business_unit_type.
 */
domain::business_unit_type generate_synthetic_business_unit_type(
    utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic business_unit_types.
 */
std::vector<domain::business_unit_type>
generate_synthetic_business_unit_types(std::size_t n,
    utility::generation::generation_context& ctx);

}

#endif

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
#ifndef ORES_DQ_GENERATORS_CODING_SCHEME_AUTHORITY_TYPE_GENERATOR_HPP
#define ORES_DQ_GENERATORS_CODING_SCHEME_AUTHORITY_TYPE_GENERATOR_HPP

#include <vector>
#include "ores.dq/domain/coding_scheme_authority_type.hpp"

namespace ores::dq::generators {

/**
 * @brief Generates a synthetic coding_scheme_authority_type.
 */
domain::coding_scheme_authority_type generate_synthetic_coding_scheme_authority_type();

/**
 * @brief Generates N synthetic coding_scheme_authority_types.
 */
std::vector<domain::coding_scheme_authority_type>
generate_synthetic_coding_scheme_authority_types(std::size_t n);

}

#endif

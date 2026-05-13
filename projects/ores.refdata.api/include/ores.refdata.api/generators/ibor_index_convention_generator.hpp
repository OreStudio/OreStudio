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
#ifndef ORES_REFDATA_GENERATORS_IBOR_INDEX_CONVENTION_GENERATOR_HPP
#define ORES_REFDATA_GENERATORS_IBOR_INDEX_CONVENTION_GENERATOR_HPP

#include <vector>
#include "ores.refdata.api/domain/ibor_index_convention.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::refdata::generators {

/**
 * @brief Generates a synthetic ibor_index_convention.
 */
ORES_REFDATA_API_EXPORT domain::ibor_index_convention generate_synthetic_ibor_index_convention(
    utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic ibor_index_conventions.
 */
ORES_REFDATA_API_EXPORT std::vector<domain::ibor_index_convention>
generate_synthetic_ibor_index_conventions(std::size_t n,
    utility::generation::generation_context& ctx);

}

#endif

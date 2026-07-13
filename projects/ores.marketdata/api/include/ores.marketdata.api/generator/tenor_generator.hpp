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
#ifndef ORES_MARKETDATA_API_GENERATOR_TENOR_GENERATOR_HPP
#define ORES_MARKETDATA_API_GENERATOR_TENOR_GENERATOR_HPP

#include "ores.marketdata.api/domain/tenor.hpp"
#include "ores.marketdata.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::marketdata::generator {

/**
 * @brief Generates a synthetic tenor.
 */
ORES_MARKETDATA_API_EXPORT domain::tenor
generate_synthetic_tenor(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic tenors.
 */
ORES_MARKETDATA_API_EXPORT std::vector<domain::tenor>
generate_synthetic_tenors(std::size_t n, utility::generation::generation_context& ctx);

}

#endif

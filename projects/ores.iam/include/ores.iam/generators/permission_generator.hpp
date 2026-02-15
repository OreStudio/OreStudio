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
#ifndef ORES_IAM_GENERATORS_PERMISSION_GENERATOR_HPP
#define ORES_IAM_GENERATORS_PERMISSION_GENERATOR_HPP

#include <vector>
#include "ores.iam/domain/permission.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::iam::generators {

/**
 * @brief Generates a synthetic permission.
 */
domain::permission generate_synthetic_permission(
    utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic permissions.
 */
std::vector<domain::permission>
generate_synthetic_permissions(std::size_t n,
    utility::generation::generation_context& ctx);

}

#endif

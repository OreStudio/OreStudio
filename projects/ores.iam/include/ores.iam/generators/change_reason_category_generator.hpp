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
#ifndef ORES_IAM_GENERATORS_CHANGE_REASON_CATEGORY_GENERATOR_HPP
#define ORES_IAM_GENERATORS_CHANGE_REASON_CATEGORY_GENERATOR_HPP

#include <vector>
#include "ores.iam/domain/change_reason_category.hpp"

namespace ores::iam::generators {

/**
 * @brief Generates a synthetic change_reason_category.
 */
domain::change_reason_category generate_synthetic_change_reason_category();

/**
 * @brief Generates N synthetic change_reason_categories.
 *
 * @note c++ 23 generators are not supported on all compilers.
 */
std::vector<domain::change_reason_category>
generate_synthetic_change_reason_categories(std::size_t n);

}

#endif

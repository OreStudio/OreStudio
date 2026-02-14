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
#ifndef ORES_TESTING_MAKE_GENERATION_CONTEXT_HPP
#define ORES_TESTING_MAKE_GENERATION_CONTEXT_HPP

#include <cstdint>
#include "ores.utility/generation/generation_context.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"

namespace ores::testing {

/**
 * @brief Creates a generation context populated from the test database helper.
 *
 * Populates modified_by from db_user() and tenant_id from the test tenant.
 */
utility::generation::generation_context
make_generation_context(database_helper& h);

/**
 * @brief Creates a generation context with a specific seed.
 */
utility::generation::generation_context
make_generation_context(database_helper& h, std::uint64_t seed);

/**
 * @brief Creates a generation context populated from the scoped database helper.
 *
 * Populates modified_by from db_user() and tenant_id from the test tenant.
 */
utility::generation::generation_context
make_generation_context(scoped_database_helper& h);

/**
 * @brief Creates a generation context with a specific seed.
 */
utility::generation::generation_context
make_generation_context(scoped_database_helper& h, std::uint64_t seed);

}

#endif

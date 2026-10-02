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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_generator.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ANALYTICS_API_GENERATORS_TODAYS_MARKET_COLLECTION_GENERATOR_HPP
#define ORES_ANALYTICS_API_GENERATORS_TODAYS_MARKET_COLLECTION_GENERATOR_HPP

#include "ores.analytics.api/domain/todays_market_collection.hpp"
#include "ores.analytics.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::analytics::generators {

/**
 * @brief Generates a synthetic todays_market_collection.
 */
ORES_ANALYTICS_API_EXPORT domain::todays_market_collection
generate_synthetic_todays_market_collection(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic todays_market_collections.
 */
ORES_ANALYTICS_API_EXPORT std::vector<domain::todays_market_collection>
generate_synthetic_todays_market_collections(std::size_t n,
                                             utility::generation::generation_context& ctx);

}

#endif

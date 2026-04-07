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
#ifndef ORES_ANALYTICS_GENERATORS_PRICING_MODEL_PRODUCT_GENERATOR_HPP
#define ORES_ANALYTICS_GENERATORS_PRICING_MODEL_PRODUCT_GENERATOR_HPP

#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::analytics::generators {

/**
 * @brief Generates a set of fictional pricing model products.
 *
 * Each generated product is associated with the given config_id and uses
 * engine type codes from the fictional pricing engine type generator.
 *
 * @param n Number of products to generate. If n is 0 or greater than the
 *          available set, returns all available fictional products.
 * @param config_id UUID of the pricing model config these products belong to.
 */
std::vector<domain::pricing_model_product> generate_fictional_pricing_model_products(
    std::size_t n,
    const boost::uuids::uuid& config_id,
    utility::generation::generation_context& ctx);

}

#endif

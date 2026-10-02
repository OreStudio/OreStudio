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
#ifndef ORES_ORE_CORE_DOMAIN_PRICING_ENGINE_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_PRICING_ENGINE_MAPPER_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include <string>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief One ORE PricingEngines document, mapped to the analytics entities.
 *
 * The document decomposes into the three tables the analytics component
 * already holds for it: the pricing model configuration that names the set,
 * one product row per Product element, and one parameter row per Parameter
 * element wherever it appeared. Global parameters carry no product id, which
 * is what the nullable column is for.
 */
struct mapped_pricing_engines {
    analytics::domain::pricing_model_config config;
    std::vector<analytics::domain::pricing_model_product> products;
    std::vector<analytics::domain::pricing_model_product_parameter> parameters;
};

/**
 * @brief Maps between an ORE PricingEngines XML document and the analytics
 * pricing model entities.
 *
 * Import turns each Product into a product row and each Parameter into a
 * parameter row scoped to the product it was written under, or to the
 * configuration when it came from GlobalParameters. Export regroups the rows
 * by scope and puts them back in the order they arrived, so the document comes
 * back product for product and parameter for parameter.
 *
 * Order is carried in the =position= column rather than recovered on export,
 * because a document may name the same pricing engine type twice and the same
 * parameter name twice within one scope. Neither field identifies its row, so
 * neither can be sorted back into the order the document used.
 */
class ORES_ORE_CORE_EXPORT pricing_engine_mapper {
public:
    /**
     * @brief Maps an ORE PricingEngines document to the analytics entities.
     *
     * Every Parameter becomes a row whose scope is the element that held it:
     * =model= under ModelParameters, =engine= under EngineParameters, and
     * =global= under GlobalParameters with no product id.
     */
    static mapped_pricing_engines map(const pricingengines& v);

    /**
     * @brief Reconstructs an ORE PricingEngines document from mapped entities.
     *
     * Products are emitted in =position= order, and each product's model and
     * engine parameters in the =position= order of their own scope; rows that
     * share a position are ordered by id, which is stable but not the order
     * they were created in. GlobalParameters is written only when
     * a global-scope row exists, which is equivalent because no shipped document
     * writes an empty one.
     *
     * @throws std::runtime_error for a parameter row the document has no place
     * for: an unknown scope, a global row that names a product, or a model or
     * engine row whose product is not among the products.
     */
    static pricingengines reverse(const mapped_pricing_engines& v);
};

}

#endif

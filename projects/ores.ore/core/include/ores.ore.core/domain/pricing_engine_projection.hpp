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
#ifndef ORES_ORE_CORE_DOMAIN_PRICING_ENGINE_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_PRICING_ENGINE_PROJECTION_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/generated/pricing_engines.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief Projects an ORE PricingEngines record into the analytics pricing
 * model entities.
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
class ORES_ORE_CORE_EXPORT pricing_engine_projection {
public:
    /**
     * @brief Maps an ORE PricingEngines document to the analytics entities.
     *
     * Every Parameter becomes a row whose scope is the element that held it:
     * =model= under ModelParameters, =engine= under EngineParameters, and
     * =global= under GlobalParameters with no product id.
     */
    using document = xml::generated::pricing_engines::pricingengines;

    static ores::analytics::messaging::pricing_engines_document map(const document& v);

    /**
     * @brief Reconstructs an ORE PricingEngines document from mapped entities.
     *
     * Products are emitted in =position= order, and each product's model and
     * engine parameters in the =position= order of their own scope; rows that
     * share a position are ordered by id, which is stable but not the order
     * they were created in. The GlobalParameters block is marked present only
     * when a global-scope row exists; the writer states the block either way,
     * because the document states every section.
     *
     * @throws std::runtime_error for a parameter row the document has no place
     * for: an unknown scope, a global row that names a product, or a model or
     * engine row whose product is not among the products.
     */
    static document reverse(const ores::analytics::messaging::pricing_engines_document& v);

    /**
     * @brief Reads a PricingEngines document into the record the table describes.
     *
     * @param text The document.
     * @param origin The file the text came from, named in the failure.
     * @throws std::invalid_argument naming the origin and the line where the
     * document does not bind, which is the shape the run store reports its
     * other configuration documents with.
     */
    static document read(std::string_view text, std::string_view origin = {});

    /**
     * @brief Writes a PricingEngines record as a document.
     *
     * @throws std::runtime_error when the record cannot produce one.
     */
    static std::string write(const document& v);
};

}

#endif

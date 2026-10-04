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
#ifndef ORES_ANALYTICS_API_DOMAIN_PRICING_ENGINES_DOCUMENT_HPP
#define ORES_ANALYTICS_API_DOMAIN_PRICING_ENGINES_DOCUMENT_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include <string>
#include <vector>

namespace ores::analytics::domain {

/**
 * @brief One ORE pricing engines document as the rows analytics stores.
 *
 * The header row and its child rows. Analytics stores and reads the document
 * whole; another component reaches it through analytics' operations.
 */
struct pricing_engines_document {
    pricing_model_config config;
    std::vector<pricing_model_product> products;
    std::vector<pricing_model_product_parameter> parameters;

    friend bool operator==(const pricing_engines_document&,
                           const pricing_engines_document&) = default;
};

}

#endif

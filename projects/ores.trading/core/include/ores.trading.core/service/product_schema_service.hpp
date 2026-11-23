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
#ifndef ORES_TRADING_CORE_SERVICE_PRODUCT_SCHEMA_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_PRODUCT_SCHEMA_SERVICE_HPP

#include "ores.ore.api/domain/ore_parameter.hpp"
#include "ores.trading.api/domain/product_type.hpp"
#include "ores.trading.api/domain/script.hpp"
#include "ores.trading.api/domain/script_input.hpp"
#include "ores.trading.api/domain/script_input_code.hpp"
#include "ores.trading.api/domain/script_twin_input.hpp"
#include "ores.trading.api/messaging/product_schema_operation_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::trading::service {

/**
 * @brief The rows of trading's own tables a product's description is made from.
 *
 * The scripts are those the caller's tenant sees: its own and the world
 * library's. Where both name a script, the tenant's own is the one its
 * bookings resolve, so it is the one described.
 */
struct product_catalogue final {
    std::vector<domain::product_type> product_types;
    std::vector<domain::script> scripts;
    std::vector<domain::script_twin_input> twins;
    std::vector<domain::script_input> inputs;
    std::vector<domain::script_input_code> codes;
};

/**
 * @brief The fields of the instruments, by the schema type they belong to, as
 * the cache of ores.ore's parameters holds them.
 */
using instrument_fields =
    std::unordered_map<std::string, std::vector<ores::ore::domain::ore_parameter>>;

/**
 * @brief Describes one product: the fields of its element and of every type
 * they reach, the inputs and codes of the scripts its forms run, and the forms.
 *
 * A code that names no product type but names a library script describes the
 * script as a script-only product. A product with neither an element nor a
 * scripted form is refused, naming what it lacks.
 */
ORES_TRADING_CORE_EXPORT messaging::describe_product_response describe_product(
    const std::string& code, const product_catalogue& catalogue, const instrument_fields& fields);

/**
 * @brief Every product type in display order, then every library script that
 * no product type twins and no product type is named after.
 */
ORES_TRADING_CORE_EXPORT messaging::list_products_response
list_products(const product_catalogue& catalogue);

}

#endif

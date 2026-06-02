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
#ifndef ORES_ANALYTICS_REPOSITORY_PRICING_MODEL_PRODUCT_ENTITY_HPP
#define ORES_ANALYTICS_REPOSITORY_PRICING_MODEL_PRODUCT_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::analytics::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a pricing model product in the database.
 */
struct pricing_model_product_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_analytics_pricing_model_products_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string pricing_model_config_id;
    std::string pricing_engine_type_code;
    std::string model;
    std::string engine;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<db_timestamp> valid_from = "9999-12-31 23:59:59";
    std::optional<db_timestamp> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const pricing_model_product_entity& v);

}

#endif

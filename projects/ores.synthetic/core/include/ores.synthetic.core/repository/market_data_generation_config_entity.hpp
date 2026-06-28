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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_MARKET_DATA_GENERATION_CONFIG_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_MARKET_DATA_GENERATION_CONFIG_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <ostream>
#include <string>

namespace ores::synthetic::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a market data generation configuration in the database.
 */
struct market_data_generation_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_synthetic_market_data_generation_configs_tbl";

    sqlgen::PrimaryKey<std::string> id; // UUID stored as string, converted in mapper
    std::string tenant_id;
    std::string party_id; // UUID stored as string, converted in mapper
    int version = 0;
    std::string name;
    std::string description;
    bool enabled = false;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const market_data_generation_config_entity& v);

}

#endif

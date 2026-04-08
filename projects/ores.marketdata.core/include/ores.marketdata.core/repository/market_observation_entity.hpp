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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_OBSERVATION_ENTITY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_OBSERVATION_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::marketdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Database entity for a single market data observation.
 *
 * Maps to ores_marketdata_observations_tbl (TimescaleDB hypertable).
 * observation_date is stored as "YYYY-MM-DD" string mapped to a PostgreSQL
 * date column. valid_from/valid_to are managed by the insert/delete triggers.
 */
struct market_observation_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_observations_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    std::string series_id;
    std::string observation_date;
    std::optional<std::string> point_id;
    std::string value;
    std::optional<std::string> source;
    std::optional<db_timestamp> valid_from =
        "9999-12-31 23:59:59";
    std::optional<db_timestamp> valid_to =
        "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const market_observation_entity& v);

}

#endif

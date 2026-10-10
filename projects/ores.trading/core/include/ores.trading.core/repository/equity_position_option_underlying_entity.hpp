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
 * Template: cpp_domain_type_entity.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_REPOSITORY_EQUITY_POSITION_OPTION_UNDERLYING_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_EQUITY_POSITION_OPTION_UNDERLYING_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a equity position option underlying in the database.
 */
struct equity_position_option_underlying_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_equity_position_option_underlyings_tbl";

    sqlgen::PrimaryKey<std::string> trade_id;
    sqlgen::PrimaryKey<std::string> sequence_number;
    std::string tenant_id;
    int version = 0;
    std::string trade_activity_id;
    std::string underlying_name;
    std::string strike;
    std::optional<double> weight;
    std::string long_short;
    std::optional<std::string> option_type;
    std::optional<std::string> exercise_type;
    std::optional<std::string> settlement_type;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const equity_position_option_underlying_entity& v);

}

#endif

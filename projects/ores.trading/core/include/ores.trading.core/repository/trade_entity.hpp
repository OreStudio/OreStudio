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
#ifndef ORES_TRADING_CORE_REPOSITORY_TRADE_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_TRADE_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a trade in the database.
 */
struct trade_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_trades_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    std::string workspace_id;
    int version = 0;
    std::string party_id;
    std::optional<std::string> external_id;
    std::string book_id;
    std::string portfolio_id;
    std::optional<std::string> successor_trade_id;
    std::string trade_type;
    std::optional<std::string> counterparty_id;
    std::optional<std::string> product_type;
    std::optional<std::string> instrument_id;
    std::optional<std::string> asset_class;
    std::string netting_set_id;
    std::string activity_type_code;
    std::string status_id;
    std::optional<std::string> trade_date;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> execution_timestamp;
    std::optional<std::string> effective_date;
    std::optional<std::string> termination_date;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const trade_entity& v);

}

#endif

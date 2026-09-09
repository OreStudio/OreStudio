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
#ifndef ORES_TRADING_CORE_REPOSITORY_BOND_FUTURE_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_BOND_FUTURE_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a bond future in the database.
 */
struct bond_future_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_bond_futures_tbl";

    sqlgen::PrimaryKey<std::string> instrument_id;
    std::string tenant_id;
    int version = 0;
    std::string contract_name;
    double contract_notional = 0.0;
    std::string long_short;
    std::string currency;
    std::string contract_month;
    std::optional<std::string> deliverable_grade;
    double fair_price = 0.0;
    std::string settlement;
    bool settlement_dirty = false;
    std::optional<std::string> root_date;
    std::optional<std::string> expiry_basis;
    std::optional<std::string> settlement_basis;
    int expiry_lag = 0;
    int settlement_lag = 0;
    std::string last_trading_date;
    std::string last_delivery_date;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const bond_future_entity& v);

}

#endif

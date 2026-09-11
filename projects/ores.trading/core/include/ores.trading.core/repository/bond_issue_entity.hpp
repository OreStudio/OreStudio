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
#ifndef ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a bond issue in the database.
 */
struct bond_issue_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_bond_issues_tbl";

    sqlgen::PrimaryKey<std::string> issue_id;
    std::string tenant_id;
    std::string workspace_id;
    int version = 0;
    std::string security_id;
    std::string issuer;
    std::string currency;
    double face_value = 0.0;
    double coupon_rate = 0.0;
    std::string coupon_frequency_code;
    std::string day_count_code;
    std::string issue_date;
    std::string maturity_date;
    int settlement_days = 0;
    std::optional<std::string> description;
    std::optional<std::string> calendar;
    std::optional<std::string> credit_curve_id;
    std::optional<std::string> reference_curve_id;
    std::optional<std::string> income_curve_id;
    std::optional<std::string> bond_notional;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const bond_issue_entity& v);

}

#endif

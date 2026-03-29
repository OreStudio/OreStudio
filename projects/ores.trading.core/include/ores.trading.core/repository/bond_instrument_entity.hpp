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
#ifndef ORES_TRADING_REPOSITORY_BOND_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_REPOSITORY_BOND_INSTRUMENT_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::trading::repository {

/**
 * @brief Represents a bond instrument in the database.
 */
struct bond_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_bond_instruments_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string trade_type_code;
    std::string issuer;
    std::string currency;
    double face_value = 0.0;
    double coupon_rate = 0.0;
    std::string coupon_frequency_code;
    std::string day_count_code;
    std::string issue_date;
    std::string maturity_date;
    std::optional<int> settlement_days;
    std::optional<std::string> call_date;
    std::optional<double> conversion_ratio;
    std::optional<std::string> description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
    // Phase 7 extensions
    std::optional<std::string> future_expiry_date;
    std::optional<std::string> option_type;
    std::optional<std::string> option_expiry_date;
    std::optional<double> option_strike;
    std::optional<std::string> trs_return_type;
    std::optional<std::string> trs_funding_leg_code;
    std::optional<std::string> ascot_option_type;
};

std::ostream& operator<<(std::ostream& s, const bond_instrument_entity& v);

}

#endif

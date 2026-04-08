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
#ifndef ORES_TRADING_REPOSITORY_COMMODITY_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_REPOSITORY_COMMODITY_INSTRUMENT_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a commodity instrument in the database.
 */
struct commodity_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_commodity_instruments_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::optional<std::string> trade_id;
    std::string trade_type_code;
    std::string commodity_code;
    std::string currency;
    double quantity = 0.0;
    std::string unit;
    std::optional<std::string> start_date;
    std::optional<std::string> maturity_date;
    std::optional<double> fixed_price;
    std::optional<std::string> option_type;
    std::optional<double> strike_price;
    std::optional<std::string> exercise_type;
    std::optional<std::string> average_type;
    std::optional<std::string> averaging_start_date;
    std::optional<std::string> averaging_end_date;
    std::optional<std::string> spread_commodity_code;
    std::optional<double> spread_amount;
    std::optional<std::string> strip_frequency_code;
    std::optional<double> variance_strike;
    std::optional<double> accumulation_amount;
    std::optional<double> knock_out_barrier;
    std::optional<std::string> barrier_type;
    std::optional<double> lower_barrier;
    std::optional<double> upper_barrier;
    std::optional<std::string> basket_json;
    std::optional<std::string> day_count_code;
    std::optional<std::string> payment_frequency_code;
    std::optional<std::string> swaption_expiry_date;
    std::optional<std::string> description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<db_timestamp> valid_from = "9999-12-31 23:59:59";
    std::optional<db_timestamp> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const commodity_instrument_entity& v);

}

#endif

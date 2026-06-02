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
#ifndef ORES_TRADING_REPOSITORY_EQUITY_BARRIER_OPTION_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_REPOSITORY_EQUITY_BARRIER_OPTION_INSTRUMENT_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::trading::repository {

struct equity_barrier_option_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_equity_barrier_option_instruments_tbl";

    sqlgen::PrimaryKey<std::string> instrument_id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::optional<std::string> trade_id;
    std::string trade_type_code;
    std::string underlying_name;
    std::string currency;
    double notional = 0.0;
    std::string option_type;
    double strike = 0.0;
    std::string expiry_date;
    std::string exercise_type;
    std::string long_short;
    double lower_barrier = 0.0;
    std::string lower_barrier_type;
    std::optional<double> upper_barrier;
    std::optional<std::string> upper_barrier_type;
    std::optional<double> rebate;
    std::optional<std::string> description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const equity_barrier_option_instrument_entity& v);

}

#endif

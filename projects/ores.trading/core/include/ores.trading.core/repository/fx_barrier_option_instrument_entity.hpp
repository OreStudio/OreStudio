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
#ifndef ORES_TRADING_REPOSITORY_FX_BARRIER_OPTION_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_REPOSITORY_FX_BARRIER_OPTION_INSTRUMENT_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::trading::repository {

/**
 * @brief Represents an FX barrier option instrument in the database.
 *
 * Covers ORE product types: FxBarrierOption, FxDoubleBarrierOption,
 * FxEuropeanBarrierOption, FxKIKOBarrierOption, FxGenericBarrierOption.
 */
struct fx_barrier_option_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_fx_barrier_option_instruments_tbl";

    sqlgen::PrimaryKey<std::string> instrument_id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::optional<std::string> trade_id;
    std::string trade_type_code;
    std::string bought_currency;
    double bought_amount = 0.0;
    std::string sold_currency;
    double sold_amount = 0.0;
    std::optional<std::string> option_type;
    std::string expiry_date;
    std::optional<std::string> settlement;
    std::string barrier_type;
    double lower_barrier = 0.0;
    std::optional<double> upper_barrier;
    std::optional<std::string> underlying_code;
    std::optional<std::string> description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const fx_barrier_option_instrument_entity& v);

}

#endif

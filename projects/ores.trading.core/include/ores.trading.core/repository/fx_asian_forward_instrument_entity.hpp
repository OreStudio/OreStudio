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
#ifndef ORES_TRADING_REPOSITORY_FX_ASIAN_FORWARD_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_REPOSITORY_FX_ASIAN_FORWARD_INSTRUMENT_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::trading::repository {

/**
 * @brief Represents an FX asian forward instrument in the database.
 *
 * Covers ORE product types: FxAverageForward, FxTaRF.
 */
struct fx_asian_forward_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_fx_asian_forward_instruments_tbl";

    sqlgen::PrimaryKey<std::string> instrument_id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::optional<std::string> trade_id;
    std::string trade_type_code;
    std::string fx_index;
    // FxAverageForward-specific
    std::optional<std::string> reference_currency;
    std::optional<double> reference_notional;
    std::optional<std::string> settlement_currency;
    std::optional<double> settlement_notional;
    std::optional<std::string> payment_date;
    std::optional<std::string> long_short;
    // FxTaRF-specific
    std::optional<std::string> currency;
    std::optional<double> fixing_amount;
    std::optional<double> target_amount;
    std::optional<double> strike;
    std::optional<std::string> description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const fx_asian_forward_instrument_entity& v);

}

#endif

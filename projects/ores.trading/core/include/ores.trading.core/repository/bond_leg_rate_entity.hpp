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
#ifndef ORES_TRADING_CORE_REPOSITORY_BOND_LEG_RATE_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_BOND_LEG_RATE_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a bond leg rate in the database.
 */
struct bond_leg_rate_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_bond_leg_rates_tbl";

    sqlgen::PrimaryKey<std::string> instrument_id;
    sqlgen::PrimaryKey<std::string> leg_role;
    sqlgen::PrimaryKey<std::string> leg_number;
    std::string tenant_id;
    int version = 0;
    std::string rate_kind;
    std::optional<std::string> index;
    std::optional<bool> is_in_arrears;
    std::optional<std::int64_t> fixing_days;
    std::optional<std::string> fixing_calendar;
    std::optional<std::string> last_recent_period;
    std::optional<std::string> last_recent_period_calendar;
    std::optional<std::string> lookback;
    std::optional<std::int64_t> rate_cutoff;
    std::optional<bool> is_averaged;
    std::optional<bool> has_sub_periods;
    std::optional<bool> include_spread;
    std::optional<bool> is_not_resetting_xccy;
    std::optional<bool> naked_option;
    std::optional<bool> local_cap_floor;
    std::optional<bool> stub_use_original_curve;
    std::optional<bool> observation_shift;
    std::optional<std::string> front_stub_short_index;
    std::optional<std::string> front_stub_long_index;
    std::optional<std::string> front_stub_rounding_type;
    std::optional<std::int64_t> front_stub_rounding_precision;
    std::optional<std::string> back_stub_short_index;
    std::optional<std::string> back_stub_long_index;
    std::optional<std::string> back_stub_rounding_type;
    std::optional<std::int64_t> back_stub_rounding_precision;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const bond_leg_rate_entity& v);

}

#endif

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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CROSS_CURRENCY_FIX_FLOAT_CONVENTION_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CROSS_CURRENCY_FIX_FLOAT_CONVENTION_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a cross-currency fix-float convention in the database.
 */
struct cross_currency_fix_float_convention_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_refdata_cross_currency_fix_float_conventions_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    int settlement_days = 0;
    std::string settlement_calendar;
    std::string settlement_convention;
    std::string fixed_currency;
    std::string fixed_frequency;
    std::string fixed_convention;
    std::string fixed_day_count_fraction;
    std::string index;
    std::optional<bool> eom;
    std::optional<bool> is_resettable;
    std::optional<bool> float_index_is_resettable;
    std::optional<bool> include_spread;
    std::optional<std::string> lookback;
    std::optional<int> fixing_days;
    std::optional<int> rate_cutoff;
    std::optional<bool> is_averaged;
    std::optional<bool> observation_shift;
    std::optional<std::string> oresmd_uri;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const cross_currency_fix_float_convention_entity& v);

}

#endif

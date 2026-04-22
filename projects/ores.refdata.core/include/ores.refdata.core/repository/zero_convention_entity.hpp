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
#ifndef ORES_REFDATA_REPOSITORY_ZERO_CONVENTION_ENTITY_HPP
#define ORES_REFDATA_REPOSITORY_ZERO_CONVENTION_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::refdata::repository {

/**
 * @brief Represents a zero convention in the database.
 */
struct zero_convention_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_zero_conventions_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    bool tenor_based;
    std::string day_count_fraction;
    std::optional<std::string> compounding;
    std::optional<std::string> compounding_frequency;
    std::optional<std::string> tenor_calendar;
    std::optional<int> spot_lag;
    std::optional<std::string> spot_calendar;
    std::optional<std::string> roll_convention;
    std::optional<bool> end_of_month;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const zero_convention_entity& v);

}

#endif

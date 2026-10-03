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
#ifndef ORES_REFDATA_CORE_REPOSITORY_BASE_CORRELATION_CONFIG_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_BASE_CORRELATION_CONFIG_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a base correlation config in the database.
 */
struct base_correlation_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_base_correlation_configs_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string curve_definition_id;
    std::string terms;
    std::string detachment_points;
    double settlement_days;
    std::string calendar;
    std::string business_day_convention;
    std::string day_counter;
    std::optional<std::string> extrapolate;
    std::optional<std::string> quote_name;
    std::optional<std::string> start_date;
    std::optional<std::string> rule;
    std::optional<std::string> adjust_for_losses;
    std::optional<std::string> index_term;
    std::optional<std::string> index_spread;
    std::optional<std::string> currency;
    std::optional<std::string> calibrate_constituents_to_index_spread;
    std::optional<std::string> use_assumed_recovery;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const base_correlation_config_entity& v);

}

#endif

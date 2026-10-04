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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CAP_FLOOR_VOLATILITY_CONFIG_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CAP_FLOOR_VOLATILITY_CONFIG_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a cap floor volatility config in the database.
 */
struct cap_floor_volatility_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_cap_floor_volatility_configs_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string curve_definition_id;
    std::optional<std::string> volatility_type;
    std::optional<std::string> output_volatility_type;
    std::optional<double> model_shift;
    std::optional<double> output_shift;
    std::optional<std::string> extrapolation;
    std::optional<std::string> interpolation_method;
    std::optional<std::string> include_atm;
    std::optional<std::string> day_counter;
    std::optional<std::string> calendar;
    std::optional<std::string> business_day_convention;
    std::optional<std::string> tenors;
    std::optional<std::string> strikes;
    std::optional<std::string> optional_quotes;
    std::optional<std::string> ibor_index;
    std::optional<std::string> index;
    std::optional<std::string> rate_computation_period;
    std::optional<int> on_cap_settlement_days;
    std::optional<std::string> discount_curve;
    std::optional<std::string> atm_tenors;
    std::optional<int> settlement_days;
    std::optional<std::string> interpolate_on;
    std::optional<std::string> time_interpolation;
    std::optional<std::string> strike_interpolation;
    std::optional<std::string> input_type;
    std::optional<std::string> quote_includes_index_name;
    std::optional<std::string> flat_first_period;
    std::optional<std::string> use_effecive_volatility;
    std::optional<std::string> use_effective_volatility;
    bool has_proxy_config = false;
    std::optional<std::string> proxy_source_curve_id;
    std::optional<std::string> proxy_source_index;
    std::optional<std::string> proxy_source_rate_computation_period;
    std::optional<std::string> proxy_target_index;
    std::optional<std::string> proxy_target_rate_computation_period;
    std::optional<int> proxy_target_on_cap_settlement_days;
    std::optional<double> proxy_scaling_factor;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const cap_floor_volatility_config_entity& v);

}

#endif

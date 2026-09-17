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
#ifndef ORES_REPORTING_CORE_REPOSITORY_RISK_REPORT_CONFIG_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_RISK_REPORT_CONFIG_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::reporting::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a risk report config in the database.
 */
struct risk_report_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_risk_report_configs_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string report_definition_id;

    std::string base_currency;
    std::string observation_model = "disable";
    int n_threads = 1;
    std::string market_data_type = "eod";
    std::optional<std::string> market_data_date;
    int npv_enabled = 1;
    int cashflow_enabled = 1;
    int curves_enabled = 0;
    int sensitivity_enabled = 0;
    int simulation_enabled = 0;
    int xva_enabled = 0;
    int stress_enabled = 0;
    int parametric_var_enabled = 0;
    int initial_margin_enabled = 0;
    int pfe_enabled = 0;
    std::optional<double> xva_quantile;
    int xva_cva_enabled = 0;
    int xva_dva_enabled = 0;
    int xva_fva_enabled = 0;
    int xva_colva_enabled = 0;
    int xva_dim_enabled = 0;
    std::optional<double> xva_dim_quantile;
    std::optional<int> xva_dim_horizon_calendar_days;
    std::optional<int> xva_dim_regression_order;
    std::optional<std::string> var_method;
    std::optional<std::string> simm_version;
    std::optional<std::string> simm_calculation_currency;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const risk_report_config_entity& v);

}

#endif

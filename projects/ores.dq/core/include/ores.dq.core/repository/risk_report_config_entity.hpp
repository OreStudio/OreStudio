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
#ifndef ORES_DQ_CORE_REPOSITORY_RISK_REPORT_CONFIG_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_RISK_REPORT_CONFIG_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a risk report config in the database.
 */
struct risk_report_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_risk_report_configs_artefact_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    std::string report_name;
    std::string base_currency;
    std::string observation_model;
    int n_threads = 0;
    std::string market_data_type;
    int npv_enabled = 0;
    int cashflow_enabled = 0;
    int curves_enabled = 0;
    int sensitivity_enabled = 0;
    int simulation_enabled = 0;
    int xva_enabled = 0;
    int stress_enabled = 0;
    int parametric_var_enabled = 0;
    int initial_margin_enabled = 0;
    int pfe_enabled = 0;
    int xva_cva_enabled = 0;
    int xva_dva_enabled = 0;
    int xva_fva_enabled = 0;
    int display_order = 0;
};

std::ostream& operator<<(std::ostream& s, const risk_report_config_entity& v);

}

#endif

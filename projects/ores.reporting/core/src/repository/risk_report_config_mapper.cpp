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
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.reporting.core/repository/risk_report_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.reporting.api/domain/risk_report_config_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::risk_report_config risk_report_config_mapper::map(const risk_report_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::risk_report_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.report_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.report_definition_id);

    r.base_currency = v.base_currency;
    r.observation_model = v.observation_model;
    r.n_threads = v.n_threads;
    r.market_data_type = v.market_data_type;
    r.market_data_date = v.market_data_date.value_or("");
    r.npv_enabled = v.npv_enabled;
    r.cashflow_enabled = v.cashflow_enabled;
    r.curves_enabled = v.curves_enabled;
    r.sensitivity_enabled = v.sensitivity_enabled;
    r.simulation_enabled = v.simulation_enabled;
    r.xva_enabled = v.xva_enabled;
    r.stress_enabled = v.stress_enabled;
    r.parametric_var_enabled = v.parametric_var_enabled;
    r.initial_margin_enabled = v.initial_margin_enabled;
    r.pfe_enabled = v.pfe_enabled;
    r.xva_quantile = v.xva_quantile.value_or(0);
    r.xva_cva_enabled = v.xva_cva_enabled;
    r.xva_dva_enabled = v.xva_dva_enabled;
    r.xva_fva_enabled = v.xva_fva_enabled;
    r.xva_colva_enabled = v.xva_colva_enabled;
    r.xva_dim_enabled = v.xva_dim_enabled;
    r.xva_dim_quantile = v.xva_dim_quantile.value_or(0);
    r.xva_dim_horizon_calendar_days = v.xva_dim_horizon_calendar_days.value_or(0);
    r.xva_dim_regression_order = v.xva_dim_regression_order.value_or(0);
    r.var_method = v.var_method.value_or("");
    r.simm_version = v.simm_version.value_or("");
    r.simm_calculation_currency = v.simm_calculation_currency.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

risk_report_config_entity risk_report_config_mapper::map(const domain::risk_report_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    risk_report_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.report_definition_id = boost::uuids::to_string(v.report_definition_id);

    r.base_currency = v.base_currency;
    r.observation_model = v.observation_model;
    r.n_threads = v.n_threads;
    r.market_data_type = v.market_data_type;
    r.market_data_date =
        v.market_data_date.empty() ? std::nullopt : std::optional(v.market_data_date);
    r.npv_enabled = v.npv_enabled;
    r.cashflow_enabled = v.cashflow_enabled;
    r.curves_enabled = v.curves_enabled;
    r.sensitivity_enabled = v.sensitivity_enabled;
    r.simulation_enabled = v.simulation_enabled;
    r.xva_enabled = v.xva_enabled;
    r.stress_enabled = v.stress_enabled;
    r.parametric_var_enabled = v.parametric_var_enabled;
    r.initial_margin_enabled = v.initial_margin_enabled;
    r.pfe_enabled = v.pfe_enabled;
    r.xva_quantile = v.xva_quantile == 0 ? std::nullopt : std::optional(v.xva_quantile);
    r.xva_cva_enabled = v.xva_cva_enabled;
    r.xva_dva_enabled = v.xva_dva_enabled;
    r.xva_fva_enabled = v.xva_fva_enabled;
    r.xva_colva_enabled = v.xva_colva_enabled;
    r.xva_dim_enabled = v.xva_dim_enabled;
    r.xva_dim_quantile = v.xva_dim_quantile == 0 ? std::nullopt : std::optional(v.xva_dim_quantile);
    r.xva_dim_horizon_calendar_days = v.xva_dim_horizon_calendar_days == 0 ?
                                          std::nullopt :
                                          std::optional(v.xva_dim_horizon_calendar_days);
    r.xva_dim_regression_order =
        v.xva_dim_regression_order == 0 ? std::nullopt : std::optional(v.xva_dim_regression_order);
    r.var_method = v.var_method.empty() ? std::nullopt : std::optional(v.var_method);
    r.simm_version = v.simm_version.empty() ? std::nullopt : std::optional(v.simm_version);
    r.simm_calculation_currency = v.simm_calculation_currency.empty() ?
                                      std::nullopt :
                                      std::optional(v.simm_calculation_currency);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::risk_report_config>
risk_report_config_mapper::map(const std::vector<risk_report_config_entity>& v) {
    return map_vector<risk_report_config_entity, domain::risk_report_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<risk_report_config_entity>
risk_report_config_mapper::map(const std::vector<domain::risk_report_config>& v) {
    return map_vector<domain::risk_report_config, risk_report_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}

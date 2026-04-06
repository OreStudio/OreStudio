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
#include "ores.reporting.core/repository/risk_report_config_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::reporting::repository {

using namespace ores::database::repository;

domain::risk_report_config
risk_report_config_mapper::map(const risk_report_config_entity& v) {
    domain::risk_report_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.report_definition_id =
        boost::lexical_cast<boost::uuids::uuid>(v.report_definition_id);

    r.base_currency = v.base_currency;
    r.observation_model = v.observation_model;
    r.n_threads = v.n_threads;
    r.market_data_type = v.market_data_type;
    r.market_data_date = v.market_data_date;

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

    r.xva_quantile = v.xva_quantile;
    r.xva_cva_enabled = v.xva_cva_enabled;
    r.xva_dva_enabled = v.xva_dva_enabled;
    r.xva_fva_enabled = v.xva_fva_enabled;
    r.xva_colva_enabled = v.xva_colva_enabled;
    r.xva_dim_enabled = v.xva_dim_enabled;
    r.xva_dim_quantile = v.xva_dim_quantile;
    r.xva_dim_horizon_calendar_days = v.xva_dim_horizon_calendar_days;
    r.xva_dim_regression_order = v.xva_dim_regression_order;

    r.var_method = v.var_method;
    r.simm_version = v.simm_version;
    r.simm_calculation_currency = v.simm_calculation_currency;

    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    if (!v.valid_from)
        throw std::logic_error(
            "Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    return r;
}

std::vector<domain::risk_report_config>
risk_report_config_mapper::map(
    const std::vector<risk_report_config_entity>& v) {
    std::vector<domain::risk_report_config> r;
    r.reserve(v.size());
    for (const auto& e : v)
        r.push_back(map(e));
    return r;
}

}

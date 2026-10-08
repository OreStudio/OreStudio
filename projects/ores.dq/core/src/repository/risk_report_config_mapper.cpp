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
#include "ores.dq.core/repository/risk_report_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/risk_report_config.hpp"
#include "ores.dq.api/domain/risk_report_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/risk_report_config_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::risk_report_config risk_report_config_mapper::map(const risk_report_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::risk_report_config r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.report_name = v.report_name;
    r.base_currency = v.base_currency;
    r.observation_model = v.observation_model;
    r.n_threads = v.n_threads;
    r.market_data_type = v.market_data_type;
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
    r.xva_cva_enabled = v.xva_cva_enabled;
    r.xva_dva_enabled = v.xva_dva_enabled;
    r.xva_fva_enabled = v.xva_fva_enabled;
    r.display_order = v.display_order;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

risk_report_config_entity risk_report_config_mapper::map(const domain::risk_report_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    risk_report_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.report_name = v.report_name;
    r.base_currency = v.base_currency;
    r.observation_model = v.observation_model;
    r.n_threads = v.n_threads;
    r.market_data_type = v.market_data_type;
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
    r.xva_cva_enabled = v.xva_cva_enabled;
    r.xva_dva_enabled = v.xva_dva_enabled;
    r.xva_fva_enabled = v.xva_fva_enabled;
    r.display_order = v.display_order;

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

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
 * Template: cpp_domain_type_generator.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.reporting.api/generators/risk_report_config_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::reporting::generators {

using ores::utility::generation::generation_keys;

domain::risk_report_config
generate_synthetic_risk_report_config(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::risk_report_config r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    r.report_definition_id = ctx.generate_uuid();
    r.base_currency = std::string(faker::finance::currencyCode());
    r.observation_model = std::string("disable");
    r.n_threads = faker::number::integer(1, 8);
    r.market_data_type = std::string("eod");
    r.market_data_date = std::string("");
    r.npv_enabled = faker::number::integer(0, 1);
    r.cashflow_enabled = faker::number::integer(0, 1);
    r.curves_enabled = faker::number::integer(0, 1);
    r.sensitivity_enabled = faker::number::integer(0, 1);
    r.simulation_enabled = faker::number::integer(0, 1);
    r.xva_enabled = faker::number::integer(0, 1);
    r.stress_enabled = faker::number::integer(0, 1);
    r.parametric_var_enabled = faker::number::integer(0, 1);
    r.initial_margin_enabled = faker::number::integer(0, 1);
    r.pfe_enabled = faker::number::integer(0, 1);
    r.xva_quantile = faker::number::decimal(0.9, 0.99);
    r.xva_cva_enabled = faker::number::integer(0, 1);
    r.xva_dva_enabled = faker::number::integer(0, 1);
    r.xva_fva_enabled = faker::number::integer(0, 1);
    r.xva_colva_enabled = faker::number::integer(0, 1);
    r.xva_dim_enabled = faker::number::integer(0, 1);
    r.xva_dim_quantile = faker::number::decimal(0.9, 0.99);
    r.xva_dim_horizon_calendar_days = faker::number::integer(1, 30);
    r.xva_dim_regression_order = faker::number::integer(1, 3);
    r.var_method = std::string("delta");
    r.simm_version = std::string("2.6");
    r.simm_calculation_currency = std::string(faker::finance::currencyCode());
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::risk_report_config>
generate_synthetic_risk_report_configs(std::size_t n,
                                       utility::generation::generation_context& ctx) {
    std::vector<domain::risk_report_config> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_risk_report_config(ctx));
    return r;
}

}

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
#ifndef ORES_REPORTING_API_DOMAIN_RISK_REPORT_CONFIG_HPP
#define ORES_REPORTING_API_DOMAIN_RISK_REPORT_CONFIG_HPP

#include <chrono>
#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::reporting::domain {

/**
 * @brief ORE risk report configuration — 1:1 companion to report_definition.
 *
 * Carries all ORE-level parameters: base currency, observation model,
 * analytics flags, XVA/VaR/SIMM settings, and threading.
 *
 * Portfolio and book scope are stored in separate temporal junction tables
 * (ores_reporting_risk_report_config_portfolios_tbl and
 *  ores_reporting_risk_report_config_books_tbl).
 */
struct risk_report_config final {
    int version = 0;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid id;
    boost::uuids::uuid report_definition_id;

    // ── Core ORE setup ───────────────────────────────────────────────
    std::string base_currency;
    std::string observation_model = "disable";  ///< disable|none|move|defer
    int n_threads = 1;

    // ── Market data convention ───────────────────────────────────────
    std::string market_data_type = "eod";       ///< live|eod|date
    std::optional<std::string> market_data_date; ///< ISO date; set when type=date

    // ── Analytics flags (0/1) ────────────────────────────────────────
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

    // ── XVA settings (when xva_enabled=1) ────────────────────────────
    double xva_quantile = 0.95;
    int xva_cva_enabled = 0;
    int xva_dva_enabled = 0;
    int xva_fva_enabled = 0;
    int xva_colva_enabled = 0;
    int xva_dim_enabled = 0;
    double xva_dim_quantile = 0.99;
    int xva_dim_horizon_calendar_days = 14;
    int xva_dim_regression_order = 0;

    // ── Parametric VaR settings ──────────────────────────────────────
    std::vector<double> var_quantiles;
    std::string var_method = "delta";  ///< delta|delta_gamma_normal|monte_carlo

    // ── SIMM / Initial margin settings ───────────────────────────────
    std::string simm_version;
    std::string simm_calculation_currency;

    // ── Audit ────────────────────────────────────────────────────────
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif

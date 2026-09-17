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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_API_DOMAIN_RISK_REPORT_CONFIG_HPP
#define ORES_REPORTING_API_DOMAIN_RISK_REPORT_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief ORE-level parameters for a risk report definition.
 *
 * The ORE-level parameters for a risk report definition: base currency,
 * observation model, analytics flags, XVA/VaR/SIMM settings and threading.
 *
 * Each row is owned by exactly one report_definition (1:1, enforced by the
 * unique index on report_definition_id). Portfolio and book scope live in
 * separate temporal junction tables; an empty set in either junction means
 * "all visible to the tenant".
 *
 * Analytics flags use integer 0/1, not boolean, to match the project
 * convention. npv and cashflow default to enabled; all others default off.
 */
struct risk_report_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief The report definition this configuration belongs to. Unique per tenant on active
     * records, so a definition has at most one live configuration.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief Reporting base currency (ISO 4217 code) that all results are converted to.
     */
    std::string base_currency;

    /**
     * @brief How the engine handles absent market data: disable, none, move or defer.
     */
    std::string observation_model = "disable";

    /**
     * @brief Number of worker threads the risk engine may use.
     */
    int n_threads = 1;

    /**
     * @brief Market data convention: live, eod or date.
     */
    std::string market_data_type = "eod";

    /**
     * @brief As-of date (ISO 8601) used when market_data_type is date. An empty string is the
     * not-set sentinel: the mapper writes NULL for it, which is what the CHECK requires while
     * market_data_type stays eod.
     */
    std::string market_data_date;

    /**
     * @brief Enables the NPV analytic.
     */
    int npv_enabled = 1;

    /**
     * @brief Enables the cashflow analytic.
     */
    int cashflow_enabled = 1;

    /**
     * @brief Enables curve analytic output.
     */
    int curves_enabled = 0;

    /**
     * @brief Enables sensitivity analytic output.
     */
    int sensitivity_enabled = 0;

    /**
     * @brief Enables Monte Carlo simulation output.
     */
    int simulation_enabled = 0;

    /**
     * @brief Enables XVA analytics. Gates the xva_* settings below.
     */
    int xva_enabled = 0;

    /**
     * @brief Enables stress test analytics.
     */
    int stress_enabled = 0;

    /**
     * @brief Enables parametric VaR. Gates the var_* settings below.
     */
    int parametric_var_enabled = 0;

    /**
     * @brief Enables initial margin (SIMM) calculation. Gates the simm_* settings.
     */
    int initial_margin_enabled = 0;

    /**
     * @brief Enables potential future exposure output.
     */
    int pfe_enabled = 0;

    /**
     * @brief XVA confidence quantile. NULL means "not set".
     */
    double xva_quantile = 0.95;

    /**
     * @brief Enables the CVA adjustment within XVA.
     */
    int xva_cva_enabled = 0;

    /**
     * @brief Enables the DVA adjustment within XVA.
     */
    int xva_dva_enabled = 0;

    /**
     * @brief Enables the FVA adjustment within XVA.
     */
    int xva_fva_enabled = 0;

    /**
     * @brief Enables the ColVA adjustment within XVA.
     */
    int xva_colva_enabled = 0;

    /**
     * @brief Enables the DIM (dynamic initial margin) calculation within XVA.
     */
    int xva_dim_enabled = 0;

    /**
     * @brief DIM confidence quantile. NULL means "not set".
     */
    double xva_dim_quantile = 0.99;

    /**
     * @brief DIM horizon in calendar days. NULL means "not set".
     */
    int xva_dim_horizon_calendar_days = 14;

    /**
     * @brief Regression order for the DIM backtest, 1 to 3. NULL means "not set".
     */
    int xva_dim_regression_order = 0;

    /**
     * @brief Parametric VaR method: delta, delta_gamma_normal or monte_carlo. NULL means "not set".
     */
    std::string var_method = "delta";

    /**
     * @brief SIMM methodology version. NULL means "not set".
     */
    std::string simm_version;

    /**
     * @brief Currency the SIMM amount is expressed in. NULL means "not set".
     */
    std::string simm_calculation_currency;

    /**
     * @brief Username of the person who last modified this risk report config.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for risk_report_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const risk_report_config&) {
    return "ores.reporting.risk_report_config";
}

}

#endif

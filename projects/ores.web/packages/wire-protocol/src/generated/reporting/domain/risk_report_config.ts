/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: domain_types.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * The risk report config wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface RiskReportConfig {
    version: number;
    tenant_id: string;
    id: string;
    report_definition_id: string;
    base_currency: string;
    observation_model: string;
    n_threads: number;
    market_data_type: string;
    market_data_date: string;
    npv_enabled: number;
    cashflow_enabled: number;
    curves_enabled: number;
    sensitivity_enabled: number;
    simulation_enabled: number;
    xva_enabled: number;
    stress_enabled: number;
    parametric_var_enabled: number;
    initial_margin_enabled: number;
    pfe_enabled: number;
    xva_quantile: number;
    xva_cva_enabled: number;
    xva_dva_enabled: number;
    xva_fva_enabled: number;
    xva_colva_enabled: number;
    xva_dim_enabled: number;
    xva_dim_quantile: number;
    xva_dim_horizon_calendar_days: number;
    xva_dim_regression_order: number;
    var_method: string;
    simm_version: string;
    simm_calculation_currency: string;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}

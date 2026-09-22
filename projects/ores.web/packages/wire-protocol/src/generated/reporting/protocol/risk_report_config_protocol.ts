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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { RiskReportConfig } from '../domain/risk_report_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface RiskReportConfigKey {
    id: string;
}

export interface RiskReportConfigWrite {
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
    var_quantiles: number[];
    var_method: string;
    simm_version: string;
    simm_calculation_currency: string;
}

export interface RiskReportConfigChange {
    write: RiskReportConfigWrite;
    precondition: Precondition;
}

export interface RiskReportConfigRemoval {
    key: RiskReportConfigKey;
    precondition: Precondition;
}

export interface RiskReportConfigLookup {
    key: RiskReportConfigKey;
    risk_report_config: RiskReportConfig | null;
}

export interface RiskReportConfigsFilter {
    report_definition_id: string | null;
}

export interface RiskReportConfigEvent {
    event_id: string;
    key: RiskReportConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RiskReportConfigVersionKey {
    risk_report_config: RiskReportConfigKey;
    version: number;
}

export interface RiskReportConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRiskReportConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RiskReportConfigsFilter | null;
}

export interface ListRiskReportConfigsResponse {
    result: Result;
    configs: RiskReportConfig[];
    total: number;
}

export interface GetRiskReportConfigRequest {
    key: RiskReportConfigKey;
}

export interface GetRiskReportConfigResponse {
    result: Result;
    risk_report_config: RiskReportConfig | null;
}

export interface GetManyRiskReportConfigsRequest {
    keys: RiskReportConfigKey[];
}

export interface GetManyRiskReportConfigsResponse {
    result: Result;
    entries: RiskReportConfigLookup[];
}

export interface PutRiskReportConfigRequest {
    change: RiskReportConfigChange;
    intent: ChangeIntent;
}

export interface PutRiskReportConfigResponse {
    result: Result;
    risk_report_config: RiskReportConfig;
}

export interface PutManyRiskReportConfigsRequest {
    changes: RiskReportConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyRiskReportConfigsResponse {
    result: Result;
    configs: RiskReportConfig[];
}

export interface DeleteRiskReportConfigRequest {
    removal: RiskReportConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteRiskReportConfigResponse {
    result: Result;
}

export interface DeleteManyRiskReportConfigsRequest {
    removals: RiskReportConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRiskReportConfigsResponse {
    result: Result;
}

export interface ListByReportDefinitionIdRiskReportConfigsRequest {
    report_definition_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RiskReportConfigsFilter | null;
}

export interface ListByReportDefinitionIdRiskReportConfigsResponse {
    result: Result;
    configs: RiskReportConfig[];
    total: number;
}

export interface ListRiskReportConfigVersionsRequest {
    key: RiskReportConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RiskReportConfigVersionsFilter | null;
}

export interface ListRiskReportConfigVersionsResponse {
    result: Result;
    versions: RiskReportConfig[];
    total: number;
}

export interface GetRiskReportConfigVersionRequest {
    key: RiskReportConfigVersionKey;
}

export interface GetRiskReportConfigVersionResponse {
    result: Result;
    version: RiskReportConfig;
}

export const subjects = {
    list_risk_report_configs_request: "reporting.v1.risk_report_configs.list",
    get_risk_report_config_request: "reporting.v1.risk_report_configs.get",
    get_many_risk_report_configs_request: "reporting.v1.risk_report_configs.get_many",
    put_risk_report_config_request: "reporting.v1.risk_report_configs.put",
    put_many_risk_report_configs_request: "reporting.v1.risk_report_configs.put_many",
    delete_risk_report_config_request: "reporting.v1.risk_report_configs.delete",
    delete_many_risk_report_configs_request: "reporting.v1.risk_report_configs.delete_many",
    list_by_report_definition_id_risk_report_configs_request: "reporting.v1.risk_report_configs.list_by_report_definition_id",
    list_risk_report_config_versions_request: "reporting.v1.risk_report_configs_versions.list",
    get_risk_report_config_version_request: "reporting.v1.risk_report_configs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_risk_report_configs_request: true,
    get_risk_report_config_request: true,
    get_many_risk_report_configs_request: true,
    put_risk_report_config_request: true,
    put_many_risk_report_configs_request: true,
    delete_risk_report_config_request: true,
    delete_many_risk_report_configs_request: true,
    list_by_report_definition_id_risk_report_configs_request: true,
    list_risk_report_config_versions_request: true,
    get_risk_report_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "reporting.v1.risk_report_configs_events.created",
    updated: "reporting.v1.risk_report_configs_events.updated",
    deleted: "reporting.v1.risk_report_configs_events.deleted",
} as const;

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

export interface GetRiskReportConfigsRequest {
    offset: number;
    limit: number;
}

export interface GetRiskReportConfigsResponse {
    configs: RiskReportConfig[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveRiskReportConfigRequest {
    data: RiskReportConfig;
}

export interface SaveRiskReportConfigResponse {
    success: boolean;
    message: string;
}

export interface DeleteRiskReportConfigRequest {
    ids: string[];
}

export interface DeleteRiskReportConfigResponse {
    success: boolean;
    message: string;
}

export interface GetRiskReportConfigHistoryRequest {
    id: string;
}

export interface GetRiskReportConfigHistoryResponse {
    history: RiskReportConfig[];
    success: boolean;
    message: string;
}

export interface GetRiskReportConfigsByReportDefinitionIdRequest {
    report_definition_id: string;
    offset: number;
    limit: number;
}

export interface GetRiskReportConfigsByReportDefinitionIdResponse {
    configs: RiskReportConfig[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_risk_report_configs_request: "reporting.v1.risk_report_configs.list",
    save_risk_report_config_request: "reporting.v1.risk_report_configs.save",
    delete_risk_report_config_request: "reporting.v1.risk_report_configs.delete",
    get_risk_report_config_history_request: "reporting.v1.risk_report_configs.history",
    get_risk_report_configs_by_report_definition_id_request: "reporting.v1.risk_report_configs.list_by_report_definition_id",
} as const;

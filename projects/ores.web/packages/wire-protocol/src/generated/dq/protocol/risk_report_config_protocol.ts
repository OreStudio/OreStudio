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
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RiskReportConfigKey {
    id: string;
}

export interface RiskReportConfigLookup {
    key: RiskReportConfigKey;
    risk_report_config: RiskReportConfig | null;
}

export interface RiskReportConfigsFilter {
    id_one_of: string[] | null;
}

export interface RiskReportConfigEvent {
    event_id: string;
    key: RiskReportConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
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

export const subjects = {
    list_risk_report_configs_request: 'dq.v1.risk_report_configs.list',
    get_risk_report_config_request: 'dq.v1.risk_report_configs.get',
    get_many_risk_report_configs_request: 'dq.v1.risk_report_configs.get_many',
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
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.risk_report_configs_events.created',
    updated: 'dq.v1.risk_report_configs_events.updated',
    deleted: 'dq.v1.risk_report_configs_events.deleted',
} as const;

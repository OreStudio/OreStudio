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
import type { CurveSecurityConfig } from '../domain/curve_security_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSecurityConfigKey {
    id: string;
}

export interface CurveSecurityConfigWrite {
    id: string;
    curve_definition_id: string;
    spread_quote: string | null;
    recovery_rate_quote: string | null;
    cpr_quote: string | null;
    price_quote: string | null;
    conversion_factor: string | null;
}

export interface CurveSecurityConfigChange {
    write: CurveSecurityConfigWrite;
    precondition: Precondition;
}

export interface CurveSecurityConfigRemoval {
    key: CurveSecurityConfigKey;
    precondition: Precondition;
}

export interface CurveSecurityConfigLookup {
    key: CurveSecurityConfigKey;
    curve_security_config: CurveSecurityConfig | null;
}

export interface CurveSecurityConfigEvent {
    event_id: string;
    key: CurveSecurityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSecurityConfigVersionKey {
    curve_security_config: CurveSecurityConfigKey;
    version: number;
}

export interface CurveSecurityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSecurityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveSecurityConfigsResponse {
    result: Result;
    security_configs: CurveSecurityConfig[];
    total: number;
}

export interface GetCurveSecurityConfigRequest {
    key: CurveSecurityConfigKey;
}

export interface GetCurveSecurityConfigResponse {
    result: Result;
    curve_security_config: CurveSecurityConfig | null;
}

export interface GetManyCurveSecurityConfigsRequest {
    keys: CurveSecurityConfigKey[];
}

export interface GetManyCurveSecurityConfigsResponse {
    result: Result;
    entries: CurveSecurityConfigLookup[];
}

export interface PutCurveSecurityConfigRequest {
    change: CurveSecurityConfigChange;
    intent: ChangeIntent;
}

export interface PutCurveSecurityConfigResponse {
    result: Result;
    curve_security_config: CurveSecurityConfig | null;
}

export interface PutManyCurveSecurityConfigsRequest {
    changes: CurveSecurityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSecurityConfigsResponse {
    result: Result;
    security_configs: CurveSecurityConfig[];
}

export interface DeleteCurveSecurityConfigRequest {
    removal: CurveSecurityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSecurityConfigResponse {
    result: Result;
}

export interface DeleteManyCurveSecurityConfigsRequest {
    removals: CurveSecurityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSecurityConfigsResponse {
    result: Result;
}

export interface ListCurveSecurityConfigVersionsRequest {
    key: CurveSecurityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSecurityConfigVersionsFilter | null;
}

export interface ListCurveSecurityConfigVersionsResponse {
    result: Result;
    versions: CurveSecurityConfig[];
    total: number;
}

export interface GetCurveSecurityConfigVersionRequest {
    key: CurveSecurityConfigVersionKey;
}

export interface GetCurveSecurityConfigVersionResponse {
    result: Result;
    version: CurveSecurityConfig | null;
}

export const subjects = {
    list_curve_security_configs_request: 'refdata.v1.curve_security_configs.list',
    get_curve_security_config_request: 'refdata.v1.curve_security_configs.get',
    get_many_curve_security_configs_request: 'refdata.v1.curve_security_configs.get_many',
    put_curve_security_config_request: 'refdata.v1.curve_security_configs.put',
    put_many_curve_security_configs_request: 'refdata.v1.curve_security_configs.put_many',
    delete_curve_security_config_request: 'refdata.v1.curve_security_configs.delete',
    delete_many_curve_security_configs_request: 'refdata.v1.curve_security_configs.delete_many',
    list_curve_security_config_versions_request: 'refdata.v1.curve_security_configs_versions.list',
    get_curve_security_config_version_request: 'refdata.v1.curve_security_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_security_configs_request: true,
    get_curve_security_config_request: true,
    get_many_curve_security_configs_request: true,
    put_curve_security_config_request: true,
    put_many_curve_security_configs_request: true,
    delete_curve_security_config_request: true,
    delete_many_curve_security_configs_request: true,
    list_curve_security_config_versions_request: true,
    get_curve_security_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_security_configs_events.created',
    updated: 'refdata.v1.curve_security_configs_events.updated',
    deleted: 'refdata.v1.curve_security_configs_events.deleted',
} as const;

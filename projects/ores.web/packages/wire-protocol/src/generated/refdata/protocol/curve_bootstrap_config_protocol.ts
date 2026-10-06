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
import type { CurveBootstrapConfig } from '../domain/curve_bootstrap_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveBootstrapConfigKey {
    id: string;
}

export interface CurveBootstrapConfigWrite {
    id: string;
    curve_definition_id: string;
    default_curve_configuration_id: string;
    accuracy: number | null;
    global_accuracy: number | null;
    dont_throw: boolean | null;
    max_attempts: number | null;
    max_factor: number | null;
    min_factor: number | null;
    dont_throw_steps: number | null;
    global: boolean | null;
    smoothness_lambda: number | null;
}

export interface CurveBootstrapConfigChange {
    write: CurveBootstrapConfigWrite;
    precondition: Precondition;
}

export interface CurveBootstrapConfigRemoval {
    key: CurveBootstrapConfigKey;
    precondition: Precondition;
}

export interface CurveBootstrapConfigLookup {
    key: CurveBootstrapConfigKey;
    curve_bootstrap_config: CurveBootstrapConfig | null;
}

export interface CurveBootstrapConfigsFilter {
    id_one_of: string[] | null;
}

export interface CurveBootstrapConfigEvent {
    event_id: string;
    key: CurveBootstrapConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveBootstrapConfigVersionKey {
    curve_bootstrap_config: CurveBootstrapConfigKey;
    version: number;
}

export interface CurveBootstrapConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveBootstrapConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveBootstrapConfigsFilter | null;
    as_of: string | null;
}

export interface ListCurveBootstrapConfigsResponse {
    result: Result;
    bootstrap_configs: CurveBootstrapConfig[];
    total: number;
}

export interface GetCurveBootstrapConfigRequest {
    key: CurveBootstrapConfigKey;
}

export interface GetCurveBootstrapConfigResponse {
    result: Result;
    curve_bootstrap_config: CurveBootstrapConfig | null;
}

export interface GetManyCurveBootstrapConfigsRequest {
    keys: CurveBootstrapConfigKey[];
}

export interface GetManyCurveBootstrapConfigsResponse {
    result: Result;
    entries: CurveBootstrapConfigLookup[];
}

export interface PutCurveBootstrapConfigRequest {
    change: CurveBootstrapConfigChange;
    intent: ChangeIntent;
}

export interface PutCurveBootstrapConfigResponse {
    result: Result;
    curve_bootstrap_config: CurveBootstrapConfig | null;
}

export interface PutManyCurveBootstrapConfigsRequest {
    changes: CurveBootstrapConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveBootstrapConfigsResponse {
    result: Result;
    bootstrap_configs: CurveBootstrapConfig[];
}

export interface DeleteCurveBootstrapConfigRequest {
    removal: CurveBootstrapConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveBootstrapConfigResponse {
    result: Result;
}

export interface DeleteManyCurveBootstrapConfigsRequest {
    removals: CurveBootstrapConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveBootstrapConfigsResponse {
    result: Result;
}

export interface ListCurveBootstrapConfigVersionsRequest {
    key: CurveBootstrapConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveBootstrapConfigVersionsFilter | null;
}

export interface ListCurveBootstrapConfigVersionsResponse {
    result: Result;
    versions: CurveBootstrapConfig[];
    total: number;
}

export interface GetCurveBootstrapConfigVersionRequest {
    key: CurveBootstrapConfigVersionKey;
}

export interface GetCurveBootstrapConfigVersionResponse {
    result: Result;
    version: CurveBootstrapConfig | null;
}

export const subjects = {
    list_curve_bootstrap_configs_request: 'refdata.v1.curve_bootstrap_configs.list',
    get_curve_bootstrap_config_request: 'refdata.v1.curve_bootstrap_configs.get',
    get_many_curve_bootstrap_configs_request: 'refdata.v1.curve_bootstrap_configs.get_many',
    put_curve_bootstrap_config_request: 'refdata.v1.curve_bootstrap_configs.put',
    put_many_curve_bootstrap_configs_request: 'refdata.v1.curve_bootstrap_configs.put_many',
    delete_curve_bootstrap_config_request: 'refdata.v1.curve_bootstrap_configs.delete',
    delete_many_curve_bootstrap_configs_request: 'refdata.v1.curve_bootstrap_configs.delete_many',
    list_curve_bootstrap_config_versions_request:
        'refdata.v1.curve_bootstrap_configs_versions.list',
    get_curve_bootstrap_config_version_request: 'refdata.v1.curve_bootstrap_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_bootstrap_configs_request: true,
    get_curve_bootstrap_config_request: true,
    get_many_curve_bootstrap_configs_request: true,
    put_curve_bootstrap_config_request: true,
    put_many_curve_bootstrap_configs_request: true,
    delete_curve_bootstrap_config_request: true,
    delete_many_curve_bootstrap_configs_request: true,
    list_curve_bootstrap_config_versions_request: true,
    get_curve_bootstrap_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_bootstrap_configs_events.created',
    updated: 'refdata.v1.curve_bootstrap_configs_events.updated',
    deleted: 'refdata.v1.curve_bootstrap_configs_events.deleted',
} as const;

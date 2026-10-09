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
import type { IrCurveGenerationConfig } from '../domain/ir_curve_generation_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IrCurveGenerationConfigKey {
    id: string;
}

export interface IrCurveGenerationConfigWrite {
    id: string;
    party_id: string;
    config_id: string;
    currency_code: string;
    index_family: string;
    tenor: string;
    role: string;
    process_type: string;
    ticks_per_hour: number;
    enabled: boolean;
    auto_start: boolean;
    price_source: string;
    vintage_source: string;
    vintage_date: string;
    vintage_series_uri: string;
    description: string;
    fixed_leg_payment_frequency_code: string;
    source_name: string;
    folder_id: string | null;
}

export interface IrCurveGenerationConfigChange {
    write: IrCurveGenerationConfigWrite;
    precondition: Precondition;
}

export interface IrCurveGenerationConfigRemoval {
    key: IrCurveGenerationConfigKey;
    precondition: Precondition;
}

export interface IrCurveGenerationConfigLookup {
    key: IrCurveGenerationConfigKey;
    ir_curve_generation_config: IrCurveGenerationConfig | null;
}

export interface IrCurveGenerationConfigsFilter {
    id_one_of: string[] | null;
}

export interface IrCurveGenerationConfigEvent {
    event_id: string;
    key: IrCurveGenerationConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IrCurveGenerationConfigVersionKey {
    ir_curve_generation_config: IrCurveGenerationConfigKey;
    version: number;
}

export interface IrCurveGenerationConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIrCurveGenerationConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveGenerationConfigsFilter | null;
    as_of: string | null;
}

export interface ListIrCurveGenerationConfigsResponse {
    result: Result;
    ir_curve_generation_configs: IrCurveGenerationConfig[];
    total: number;
}

export interface GetIrCurveGenerationConfigRequest {
    key: IrCurveGenerationConfigKey;
}

export interface GetIrCurveGenerationConfigResponse {
    result: Result;
    ir_curve_generation_config: IrCurveGenerationConfig | null;
}

export interface GetManyIrCurveGenerationConfigsRequest {
    keys: IrCurveGenerationConfigKey[];
}

export interface GetManyIrCurveGenerationConfigsResponse {
    result: Result;
    entries: IrCurveGenerationConfigLookup[];
}

export interface PutIrCurveGenerationConfigRequest {
    change: IrCurveGenerationConfigChange;
    intent: ChangeIntent;
}

export interface PutIrCurveGenerationConfigResponse {
    result: Result;
    ir_curve_generation_config: IrCurveGenerationConfig | null;
}

export interface PutManyIrCurveGenerationConfigsRequest {
    changes: IrCurveGenerationConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyIrCurveGenerationConfigsResponse {
    result: Result;
    ir_curve_generation_configs: IrCurveGenerationConfig[];
}

export interface DeleteIrCurveGenerationConfigRequest {
    removal: IrCurveGenerationConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteIrCurveGenerationConfigResponse {
    result: Result;
}

export interface DeleteManyIrCurveGenerationConfigsRequest {
    removals: IrCurveGenerationConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIrCurveGenerationConfigsResponse {
    result: Result;
}

export interface ListIrCurveGenerationConfigVersionsRequest {
    key: IrCurveGenerationConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveGenerationConfigVersionsFilter | null;
}

export interface ListIrCurveGenerationConfigVersionsResponse {
    result: Result;
    versions: IrCurveGenerationConfig[];
    total: number;
}

export interface GetIrCurveGenerationConfigVersionRequest {
    key: IrCurveGenerationConfigVersionKey;
}

export interface GetIrCurveGenerationConfigVersionResponse {
    result: Result;
    version: IrCurveGenerationConfig | null;
}

export const subjects = {
    list_ir_curve_generation_configs_request: 'synthetic.v1.ir_curve_generation_configs.list',
    get_ir_curve_generation_config_request: 'synthetic.v1.ir_curve_generation_configs.get',
    get_many_ir_curve_generation_configs_request:
        'synthetic.v1.ir_curve_generation_configs.get_many',
    put_ir_curve_generation_config_request: 'synthetic.v1.ir_curve_generation_configs.put',
    put_many_ir_curve_generation_configs_request:
        'synthetic.v1.ir_curve_generation_configs.put_many',
    delete_ir_curve_generation_config_request: 'synthetic.v1.ir_curve_generation_configs.delete',
    delete_many_ir_curve_generation_configs_request:
        'synthetic.v1.ir_curve_generation_configs.delete_many',
    list_ir_curve_generation_config_versions_request:
        'synthetic.v1.ir_curve_generation_configs_versions.list',
    get_ir_curve_generation_config_version_request:
        'synthetic.v1.ir_curve_generation_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ir_curve_generation_configs_request: true,
    get_ir_curve_generation_config_request: true,
    get_many_ir_curve_generation_configs_request: true,
    put_ir_curve_generation_config_request: true,
    put_many_ir_curve_generation_configs_request: true,
    delete_ir_curve_generation_config_request: true,
    delete_many_ir_curve_generation_configs_request: true,
    list_ir_curve_generation_config_versions_request: true,
    get_ir_curve_generation_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.ir_curve_generation_configs_events.created',
    updated: 'synthetic.v1.ir_curve_generation_configs_events.updated',
    deleted: 'synthetic.v1.ir_curve_generation_configs_events.deleted',
} as const;

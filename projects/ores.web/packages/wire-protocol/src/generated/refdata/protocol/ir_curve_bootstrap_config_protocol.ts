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
import type { IrCurveBootstrapConfig } from '../domain/ir_curve_bootstrap_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IrCurveBootstrapConfigKey {
    id: string;
}

export interface IrCurveBootstrapConfigWrite {
    id: string;
    output_series_id: string;
    source_series_id: string;
    curve_family_role: string;
    discount_curve_config_id: string;
    interpolation_method: string;
    day_count_convention: string;
    split_tenor_code: string;
}

export interface IrCurveBootstrapConfigChange {
    write: IrCurveBootstrapConfigWrite;
    precondition: Precondition;
}

export interface IrCurveBootstrapConfigRemoval {
    key: IrCurveBootstrapConfigKey;
    precondition: Precondition;
}

export interface IrCurveBootstrapConfigLookup {
    key: IrCurveBootstrapConfigKey;
    ir_curve_bootstrap_config: IrCurveBootstrapConfig | null;
}

export interface IrCurveBootstrapConfigEvent {
    event_id: string;
    key: IrCurveBootstrapConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IrCurveBootstrapConfigVersionKey {
    ir_curve_bootstrap_config: IrCurveBootstrapConfigKey;
    version: number;
}

export interface IrCurveBootstrapConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIrCurveBootstrapConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListIrCurveBootstrapConfigsResponse {
    result: Result;
    bootstrap_configs: IrCurveBootstrapConfig[];
    total: number;
}

export interface GetIrCurveBootstrapConfigRequest {
    key: IrCurveBootstrapConfigKey;
}

export interface GetIrCurveBootstrapConfigResponse {
    result: Result;
    ir_curve_bootstrap_config: IrCurveBootstrapConfig | null;
}

export interface GetManyIrCurveBootstrapConfigsRequest {
    keys: IrCurveBootstrapConfigKey[];
}

export interface GetManyIrCurveBootstrapConfigsResponse {
    result: Result;
    entries: IrCurveBootstrapConfigLookup[];
}

export interface PutIrCurveBootstrapConfigRequest {
    change: IrCurveBootstrapConfigChange;
    intent: ChangeIntent;
}

export interface PutIrCurveBootstrapConfigResponse {
    result: Result;
    ir_curve_bootstrap_config: IrCurveBootstrapConfig;
}

export interface PutManyIrCurveBootstrapConfigsRequest {
    changes: IrCurveBootstrapConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyIrCurveBootstrapConfigsResponse {
    result: Result;
    bootstrap_configs: IrCurveBootstrapConfig[];
}

export interface DeleteIrCurveBootstrapConfigRequest {
    removal: IrCurveBootstrapConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteIrCurveBootstrapConfigResponse {
    result: Result;
}

export interface DeleteManyIrCurveBootstrapConfigsRequest {
    removals: IrCurveBootstrapConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIrCurveBootstrapConfigsResponse {
    result: Result;
}

export interface ListIrCurveBootstrapConfigVersionsRequest {
    key: IrCurveBootstrapConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveBootstrapConfigVersionsFilter | null;
}

export interface ListIrCurveBootstrapConfigVersionsResponse {
    result: Result;
    versions: IrCurveBootstrapConfig[];
    total: number;
}

export interface GetIrCurveBootstrapConfigVersionRequest {
    key: IrCurveBootstrapConfigVersionKey;
}

export interface GetIrCurveBootstrapConfigVersionResponse {
    result: Result;
    version: IrCurveBootstrapConfig;
}

export const subjects = {
    list_ir_curve_bootstrap_configs_request: "refdata.v1.ir_curve_bootstrap_configs.list",
    get_ir_curve_bootstrap_config_request: "refdata.v1.ir_curve_bootstrap_configs.get",
    get_many_ir_curve_bootstrap_configs_request: "refdata.v1.ir_curve_bootstrap_configs.get_many",
    put_ir_curve_bootstrap_config_request: "refdata.v1.ir_curve_bootstrap_configs.put",
    put_many_ir_curve_bootstrap_configs_request: "refdata.v1.ir_curve_bootstrap_configs.put_many",
    delete_ir_curve_bootstrap_config_request: "refdata.v1.ir_curve_bootstrap_configs.delete",
    delete_many_ir_curve_bootstrap_configs_request: "refdata.v1.ir_curve_bootstrap_configs.delete_many",
    list_ir_curve_bootstrap_config_versions_request: "refdata.v1.ir_curve_bootstrap_configs_versions.list",
    get_ir_curve_bootstrap_config_version_request: "refdata.v1.ir_curve_bootstrap_configs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ir_curve_bootstrap_configs_request: true,
    get_ir_curve_bootstrap_config_request: true,
    get_many_ir_curve_bootstrap_configs_request: true,
    put_ir_curve_bootstrap_config_request: true,
    put_many_ir_curve_bootstrap_configs_request: true,
    delete_ir_curve_bootstrap_config_request: true,
    delete_many_ir_curve_bootstrap_configs_request: true,
    list_ir_curve_bootstrap_config_versions_request: true,
    get_ir_curve_bootstrap_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.ir_curve_bootstrap_configs_events.created",
    updated: "refdata.v1.ir_curve_bootstrap_configs_events.updated",
    deleted: "refdata.v1.ir_curve_bootstrap_configs_events.deleted",
} as const;

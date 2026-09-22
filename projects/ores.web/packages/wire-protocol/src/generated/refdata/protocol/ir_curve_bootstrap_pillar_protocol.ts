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
import type { IrCurveBootstrapPillar } from '../domain/ir_curve_bootstrap_pillar.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IrCurveBootstrapPillarKey {
    id: string;
}

export interface IrCurveBootstrapPillarWrite {
    id: string;
    bootstrap_config_id: string;
    sequence_index: number;
    start_tenor_code: string;
    end_tenor_code: string;
    curve_role_code: string;
}

export interface IrCurveBootstrapPillarChange {
    write: IrCurveBootstrapPillarWrite;
    precondition: Precondition;
}

export interface IrCurveBootstrapPillarRemoval {
    key: IrCurveBootstrapPillarKey;
    precondition: Precondition;
}

export interface IrCurveBootstrapPillarLookup {
    key: IrCurveBootstrapPillarKey;
    ir_curve_bootstrap_pillar: IrCurveBootstrapPillar | null;
}

export interface IrCurveBootstrapPillarEvent {
    event_id: string;
    key: IrCurveBootstrapPillarKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IrCurveBootstrapPillarVersionKey {
    ir_curve_bootstrap_pillar: IrCurveBootstrapPillarKey;
    version: number;
}

export interface IrCurveBootstrapPillarVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIrCurveBootstrapPillarsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListIrCurveBootstrapPillarsResponse {
    result: Result;
    pillars: IrCurveBootstrapPillar[];
    total: number;
}

export interface GetIrCurveBootstrapPillarRequest {
    key: IrCurveBootstrapPillarKey;
}

export interface GetIrCurveBootstrapPillarResponse {
    result: Result;
    ir_curve_bootstrap_pillar: IrCurveBootstrapPillar | null;
}

export interface GetManyIrCurveBootstrapPillarsRequest {
    keys: IrCurveBootstrapPillarKey[];
}

export interface GetManyIrCurveBootstrapPillarsResponse {
    result: Result;
    entries: IrCurveBootstrapPillarLookup[];
}

export interface PutIrCurveBootstrapPillarRequest {
    change: IrCurveBootstrapPillarChange;
    intent: ChangeIntent;
}

export interface PutIrCurveBootstrapPillarResponse {
    result: Result;
    ir_curve_bootstrap_pillar: IrCurveBootstrapPillar;
}

export interface PutManyIrCurveBootstrapPillarsRequest {
    changes: IrCurveBootstrapPillarChange[];
    intent: ChangeIntent;
}

export interface PutManyIrCurveBootstrapPillarsResponse {
    result: Result;
    pillars: IrCurveBootstrapPillar[];
}

export interface DeleteIrCurveBootstrapPillarRequest {
    removal: IrCurveBootstrapPillarRemoval;
    intent: ChangeIntent;
}

export interface DeleteIrCurveBootstrapPillarResponse {
    result: Result;
}

export interface DeleteManyIrCurveBootstrapPillarsRequest {
    removals: IrCurveBootstrapPillarRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIrCurveBootstrapPillarsResponse {
    result: Result;
}

export interface ListIrCurveBootstrapPillarVersionsRequest {
    key: IrCurveBootstrapPillarKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveBootstrapPillarVersionsFilter | null;
}

export interface ListIrCurveBootstrapPillarVersionsResponse {
    result: Result;
    versions: IrCurveBootstrapPillar[];
    total: number;
}

export interface GetIrCurveBootstrapPillarVersionRequest {
    key: IrCurveBootstrapPillarVersionKey;
}

export interface GetIrCurveBootstrapPillarVersionResponse {
    result: Result;
    version: IrCurveBootstrapPillar;
}

export const subjects = {
    list_ir_curve_bootstrap_pillars_request: "refdata.v1.ir_curve_bootstrap_pillars.list",
    get_ir_curve_bootstrap_pillar_request: "refdata.v1.ir_curve_bootstrap_pillars.get",
    get_many_ir_curve_bootstrap_pillars_request: "refdata.v1.ir_curve_bootstrap_pillars.get_many",
    put_ir_curve_bootstrap_pillar_request: "refdata.v1.ir_curve_bootstrap_pillars.put",
    put_many_ir_curve_bootstrap_pillars_request: "refdata.v1.ir_curve_bootstrap_pillars.put_many",
    delete_ir_curve_bootstrap_pillar_request: "refdata.v1.ir_curve_bootstrap_pillars.delete",
    delete_many_ir_curve_bootstrap_pillars_request: "refdata.v1.ir_curve_bootstrap_pillars.delete_many",
    list_ir_curve_bootstrap_pillar_versions_request: "refdata.v1.ir_curve_bootstrap_pillars_versions.list",
    get_ir_curve_bootstrap_pillar_version_request: "refdata.v1.ir_curve_bootstrap_pillars_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ir_curve_bootstrap_pillars_request: true,
    get_ir_curve_bootstrap_pillar_request: true,
    get_many_ir_curve_bootstrap_pillars_request: true,
    put_ir_curve_bootstrap_pillar_request: true,
    put_many_ir_curve_bootstrap_pillars_request: true,
    delete_ir_curve_bootstrap_pillar_request: true,
    delete_many_ir_curve_bootstrap_pillars_request: true,
    list_ir_curve_bootstrap_pillar_versions_request: true,
    get_ir_curve_bootstrap_pillar_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.ir_curve_bootstrap_pillars_events.created",
    updated: "refdata.v1.ir_curve_bootstrap_pillars_events.updated",
    deleted: "refdata.v1.ir_curve_bootstrap_pillars_events.deleted",
} as const;

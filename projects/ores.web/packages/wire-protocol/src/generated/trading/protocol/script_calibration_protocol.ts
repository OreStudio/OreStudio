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
import type { ScriptCalibration } from '../domain/script_calibration.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptCalibrationKey {
    script_name: string;
    purpose: string;
    sequence_number: number;
}

export interface ScriptCalibrationWrite {
    script_name: string;
    purpose: string;
    sequence_number: number;
    index_variable: string;
}

export interface ScriptCalibrationChange {
    write: ScriptCalibrationWrite;
    precondition: Precondition;
}

export interface ScriptCalibrationRemoval {
    key: ScriptCalibrationKey;
    precondition: Precondition;
}

export interface ScriptCalibrationLookup {
    key: ScriptCalibrationKey;
    script_calibration: ScriptCalibration | null;
}

export interface ScriptCalibrationEvent {
    event_id: string;
    key: ScriptCalibrationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptCalibrationVersionKey {
    script_calibration: ScriptCalibrationKey;
    version: number;
}

export interface ScriptCalibrationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptCalibrationsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListScriptCalibrationsResponse {
    result: Result;
    calibrations: ScriptCalibration[];
    total: number;
}

export interface GetScriptCalibrationRequest {
    key: ScriptCalibrationKey;
}

export interface GetScriptCalibrationResponse {
    result: Result;
    script_calibration: ScriptCalibration | null;
}

export interface GetManyScriptCalibrationsRequest {
    keys: ScriptCalibrationKey[];
}

export interface GetManyScriptCalibrationsResponse {
    result: Result;
    entries: ScriptCalibrationLookup[];
}

export interface PutScriptCalibrationRequest {
    change: ScriptCalibrationChange;
    intent: ChangeIntent;
}

export interface PutScriptCalibrationResponse {
    result: Result;
    script_calibration: ScriptCalibration | null;
}

export interface PutManyScriptCalibrationsRequest {
    changes: ScriptCalibrationChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptCalibrationsResponse {
    result: Result;
    calibrations: ScriptCalibration[];
}

export interface DeleteScriptCalibrationRequest {
    removal: ScriptCalibrationRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptCalibrationResponse {
    result: Result;
}

export interface DeleteManyScriptCalibrationsRequest {
    removals: ScriptCalibrationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptCalibrationsResponse {
    result: Result;
}

export interface ListScriptCalibrationVersionsRequest {
    key: ScriptCalibrationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptCalibrationVersionsFilter | null;
}

export interface ListScriptCalibrationVersionsResponse {
    result: Result;
    versions: ScriptCalibration[];
    total: number;
}

export interface GetScriptCalibrationVersionRequest {
    key: ScriptCalibrationVersionKey;
}

export interface GetScriptCalibrationVersionResponse {
    result: Result;
    version: ScriptCalibration | null;
}

export const subjects = {
    list_script_calibrations_request: 'trading.v1.script_calibrations.list',
    get_script_calibration_request: 'trading.v1.script_calibrations.get',
    get_many_script_calibrations_request: 'trading.v1.script_calibrations.get_many',
    put_script_calibration_request: 'trading.v1.script_calibrations.put',
    put_many_script_calibrations_request: 'trading.v1.script_calibrations.put_many',
    delete_script_calibration_request: 'trading.v1.script_calibrations.delete',
    delete_many_script_calibrations_request: 'trading.v1.script_calibrations.delete_many',
    list_script_calibration_versions_request: 'trading.v1.script_calibrations_versions.list',
    get_script_calibration_version_request: 'trading.v1.script_calibrations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_script_calibrations_request: true,
    get_script_calibration_request: true,
    get_many_script_calibrations_request: true,
    put_script_calibration_request: true,
    put_many_script_calibrations_request: true,
    delete_script_calibration_request: true,
    delete_many_script_calibrations_request: true,
    list_script_calibration_versions_request: true,
    get_script_calibration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.script_calibrations.created',
    updated: 'trading.v1.events.script_calibrations.updated',
    deleted: 'trading.v1.events.script_calibrations.deleted',
} as const;

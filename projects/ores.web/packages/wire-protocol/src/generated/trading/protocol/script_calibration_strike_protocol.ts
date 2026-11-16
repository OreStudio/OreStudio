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
import type { ScriptCalibrationStrike } from '../domain/script_calibration_strike.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptCalibrationStrikeKey {
    script_name: string;
    purpose: string;
    calibration_number: number;
    sequence_number: number;
}

export interface ScriptCalibrationStrikeWrite {
    script_name: string;
    purpose: string;
    calibration_number: number;
    sequence_number: number;
    strike_variable: string;
}

export interface ScriptCalibrationStrikeChange {
    write: ScriptCalibrationStrikeWrite;
    precondition: Precondition;
}

export interface ScriptCalibrationStrikeRemoval {
    key: ScriptCalibrationStrikeKey;
    precondition: Precondition;
}

export interface ScriptCalibrationStrikeLookup {
    key: ScriptCalibrationStrikeKey;
    script_calibration_strike: ScriptCalibrationStrike | null;
}

export interface ScriptCalibrationStrikeEvent {
    event_id: string;
    key: ScriptCalibrationStrikeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptCalibrationStrikeVersionKey {
    script_calibration_strike: ScriptCalibrationStrikeKey;
    version: number;
}

export interface ScriptCalibrationStrikeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptCalibrationStrikesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListScriptCalibrationStrikesResponse {
    result: Result;
    strikes: ScriptCalibrationStrike[];
    total: number;
}

export interface GetScriptCalibrationStrikeRequest {
    key: ScriptCalibrationStrikeKey;
}

export interface GetScriptCalibrationStrikeResponse {
    result: Result;
    script_calibration_strike: ScriptCalibrationStrike | null;
}

export interface GetManyScriptCalibrationStrikesRequest {
    keys: ScriptCalibrationStrikeKey[];
}

export interface GetManyScriptCalibrationStrikesResponse {
    result: Result;
    entries: ScriptCalibrationStrikeLookup[];
}

export interface PutScriptCalibrationStrikeRequest {
    change: ScriptCalibrationStrikeChange;
    intent: ChangeIntent;
}

export interface PutScriptCalibrationStrikeResponse {
    result: Result;
    script_calibration_strike: ScriptCalibrationStrike | null;
}

export interface PutManyScriptCalibrationStrikesRequest {
    changes: ScriptCalibrationStrikeChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptCalibrationStrikesResponse {
    result: Result;
    strikes: ScriptCalibrationStrike[];
}

export interface DeleteScriptCalibrationStrikeRequest {
    removal: ScriptCalibrationStrikeRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptCalibrationStrikeResponse {
    result: Result;
}

export interface DeleteManyScriptCalibrationStrikesRequest {
    removals: ScriptCalibrationStrikeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptCalibrationStrikesResponse {
    result: Result;
}

export interface ListScriptCalibrationStrikeVersionsRequest {
    key: ScriptCalibrationStrikeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptCalibrationStrikeVersionsFilter | null;
}

export interface ListScriptCalibrationStrikeVersionsResponse {
    result: Result;
    versions: ScriptCalibrationStrike[];
    total: number;
}

export interface GetScriptCalibrationStrikeVersionRequest {
    key: ScriptCalibrationStrikeVersionKey;
}

export interface GetScriptCalibrationStrikeVersionResponse {
    result: Result;
    version: ScriptCalibrationStrike | null;
}

export const subjects = {
    list_script_calibration_strikes_request: 'trading.v1.script_calibration_strikes.list',
    get_script_calibration_strike_request: 'trading.v1.script_calibration_strikes.get',
    get_many_script_calibration_strikes_request: 'trading.v1.script_calibration_strikes.get_many',
    put_script_calibration_strike_request: 'trading.v1.script_calibration_strikes.put',
    put_many_script_calibration_strikes_request: 'trading.v1.script_calibration_strikes.put_many',
    delete_script_calibration_strike_request: 'trading.v1.script_calibration_strikes.delete',
    delete_many_script_calibration_strikes_request:
        'trading.v1.script_calibration_strikes.delete_many',
    list_script_calibration_strike_versions_request:
        'trading.v1.script_calibration_strikes_versions.list',
    get_script_calibration_strike_version_request:
        'trading.v1.script_calibration_strikes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_script_calibration_strikes_request: true,
    get_script_calibration_strike_request: true,
    get_many_script_calibration_strikes_request: true,
    put_script_calibration_strike_request: true,
    put_many_script_calibration_strikes_request: true,
    delete_script_calibration_strike_request: true,
    delete_many_script_calibration_strikes_request: true,
    list_script_calibration_strike_versions_request: true,
    get_script_calibration_strike_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.script_calibration_strikes.created',
    updated: 'trading.v1.events.script_calibration_strikes.updated',
    deleted: 'trading.v1.events.script_calibration_strikes.deleted',
} as const;

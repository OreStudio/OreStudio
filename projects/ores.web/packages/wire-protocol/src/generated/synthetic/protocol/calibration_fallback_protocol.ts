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
import type { CalibrationFallback } from '../domain/calibration_fallback.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CalibrationFallbackKey {
    code: string;
}

export interface CalibrationFallbackWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CalibrationFallbackChange {
    write: CalibrationFallbackWrite;
    precondition: Precondition;
}

export interface CalibrationFallbackRemoval {
    key: CalibrationFallbackKey;
    precondition: Precondition;
}

export interface CalibrationFallbackLookup {
    key: CalibrationFallbackKey;
    calibration_fallback: CalibrationFallback | null;
}

export interface CalibrationFallbacksFilter {
    code_one_of: string[] | null;
}

export interface CalibrationFallbackEvent {
    event_id: string;
    key: CalibrationFallbackKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalibrationFallbackVersionKey {
    calibration_fallback: CalibrationFallbackKey;
    version: number;
}

export interface CalibrationFallbackVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalibrationFallbacksRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CalibrationFallbacksFilter | null;
    as_of: string | null;
}

export interface ListCalibrationFallbacksResponse {
    result: Result;
    fallbacks: CalibrationFallback[];
    total: number;
}

export interface GetCalibrationFallbackRequest {
    key: CalibrationFallbackKey;
}

export interface GetCalibrationFallbackResponse {
    result: Result;
    calibration_fallback: CalibrationFallback | null;
}

export interface GetManyCalibrationFallbacksRequest {
    keys: CalibrationFallbackKey[];
}

export interface GetManyCalibrationFallbacksResponse {
    result: Result;
    entries: CalibrationFallbackLookup[];
}

export interface PutCalibrationFallbackRequest {
    change: CalibrationFallbackChange;
    intent: ChangeIntent;
}

export interface PutCalibrationFallbackResponse {
    result: Result;
    calibration_fallback: CalibrationFallback | null;
}

export interface PutManyCalibrationFallbacksRequest {
    changes: CalibrationFallbackChange[];
    intent: ChangeIntent;
}

export interface PutManyCalibrationFallbacksResponse {
    result: Result;
    fallbacks: CalibrationFallback[];
}

export interface DeleteCalibrationFallbackRequest {
    removal: CalibrationFallbackRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalibrationFallbackResponse {
    result: Result;
}

export interface DeleteManyCalibrationFallbacksRequest {
    removals: CalibrationFallbackRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalibrationFallbacksResponse {
    result: Result;
}

export interface ListCalibrationFallbackVersionsRequest {
    key: CalibrationFallbackKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalibrationFallbackVersionsFilter | null;
}

export interface ListCalibrationFallbackVersionsResponse {
    result: Result;
    versions: CalibrationFallback[];
    total: number;
}

export interface GetCalibrationFallbackVersionRequest {
    key: CalibrationFallbackVersionKey;
}

export interface GetCalibrationFallbackVersionResponse {
    result: Result;
    version: CalibrationFallback | null;
}

export const subjects = {
    list_calibration_fallbacks_request: 'synthetic.v1.calibration_fallbacks.list',
    get_calibration_fallback_request: 'synthetic.v1.calibration_fallbacks.get',
    get_many_calibration_fallbacks_request: 'synthetic.v1.calibration_fallbacks.get_many',
    put_calibration_fallback_request: 'synthetic.v1.calibration_fallbacks.put',
    put_many_calibration_fallbacks_request: 'synthetic.v1.calibration_fallbacks.put_many',
    delete_calibration_fallback_request: 'synthetic.v1.calibration_fallbacks.delete',
    delete_many_calibration_fallbacks_request: 'synthetic.v1.calibration_fallbacks.delete_many',
    list_calibration_fallback_versions_request: 'synthetic.v1.calibration_fallbacks_versions.list',
    get_calibration_fallback_version_request: 'synthetic.v1.calibration_fallbacks_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calibration_fallbacks_request: true,
    get_calibration_fallback_request: true,
    get_many_calibration_fallbacks_request: true,
    put_calibration_fallback_request: true,
    put_many_calibration_fallbacks_request: true,
    delete_calibration_fallback_request: true,
    delete_many_calibration_fallbacks_request: true,
    list_calibration_fallback_versions_request: true,
    get_calibration_fallback_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.calibration_fallbacks_events.created',
    updated: 'synthetic.v1.calibration_fallbacks_events.updated',
    deleted: 'synthetic.v1.calibration_fallbacks_events.deleted',
} as const;

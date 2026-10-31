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
import type { CalibrationPolicy } from '../domain/calibration_policy.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CalibrationPolicyKey {
    id: string;
}

export interface CalibrationPolicyWrite {
    id: string;
    asset_class_code: string;
    window_days: number;
    allow_inverse: boolean;
    fallback_code: string;
}

export interface CalibrationPolicyChange {
    write: CalibrationPolicyWrite;
    precondition: Precondition;
}

export interface CalibrationPolicyRemoval {
    key: CalibrationPolicyKey;
    precondition: Precondition;
}

export interface CalibrationPolicyLookup {
    key: CalibrationPolicyKey;
    calibration_policy: CalibrationPolicy | null;
}

export interface CalibrationPoliciesFilter {
    fallback_code: string | null;
    id_one_of: string[] | null;
    fallback_code_one_of: string[] | null;
    search: string | null;
}

export interface CalibrationPolicyEvent {
    event_id: string;
    key: CalibrationPolicyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalibrationPolicyVersionKey {
    calibration_policy: CalibrationPolicyKey;
    version: number;
}

export interface CalibrationPolicyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalibrationPoliciesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CalibrationPoliciesFilter | null;
    as_of: string | null;
}

export interface ListCalibrationPoliciesResponse {
    result: Result;
    policies: CalibrationPolicy[];
    total: number;
}

export interface GetCalibrationPolicyRequest {
    key: CalibrationPolicyKey;
}

export interface GetCalibrationPolicyResponse {
    result: Result;
    calibration_policy: CalibrationPolicy | null;
}

export interface GetManyCalibrationPoliciesRequest {
    keys: CalibrationPolicyKey[];
}

export interface GetManyCalibrationPoliciesResponse {
    result: Result;
    entries: CalibrationPolicyLookup[];
}

export interface PutCalibrationPolicyRequest {
    change: CalibrationPolicyChange;
    intent: ChangeIntent;
}

export interface PutCalibrationPolicyResponse {
    result: Result;
    calibration_policy: CalibrationPolicy | null;
}

export interface PutManyCalibrationPoliciesRequest {
    changes: CalibrationPolicyChange[];
    intent: ChangeIntent;
}

export interface PutManyCalibrationPoliciesResponse {
    result: Result;
    policies: CalibrationPolicy[];
}

export interface DeleteCalibrationPolicyRequest {
    removal: CalibrationPolicyRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalibrationPolicyResponse {
    result: Result;
}

export interface DeleteManyCalibrationPoliciesRequest {
    removals: CalibrationPolicyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalibrationPoliciesResponse {
    result: Result;
}

export interface ListCalibrationPolicyVersionsRequest {
    key: CalibrationPolicyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalibrationPolicyVersionsFilter | null;
}

export interface ListCalibrationPolicyVersionsResponse {
    result: Result;
    versions: CalibrationPolicy[];
    total: number;
}

export interface GetCalibrationPolicyVersionRequest {
    key: CalibrationPolicyVersionKey;
}

export interface GetCalibrationPolicyVersionResponse {
    result: Result;
    version: CalibrationPolicy | null;
}

export const subjects = {
    list_calibration_policies_request: 'synthetic.v1.calibration_policies.list',
    get_calibration_policy_request: 'synthetic.v1.calibration_policies.get',
    get_many_calibration_policies_request: 'synthetic.v1.calibration_policies.get_many',
    put_calibration_policy_request: 'synthetic.v1.calibration_policies.put',
    put_many_calibration_policies_request: 'synthetic.v1.calibration_policies.put_many',
    delete_calibration_policy_request: 'synthetic.v1.calibration_policies.delete',
    delete_many_calibration_policies_request: 'synthetic.v1.calibration_policies.delete_many',
    list_calibration_policy_versions_request: 'synthetic.v1.calibration_policies_versions.list',
    get_calibration_policy_version_request: 'synthetic.v1.calibration_policies_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calibration_policies_request: true,
    get_calibration_policy_request: true,
    get_many_calibration_policies_request: true,
    put_calibration_policy_request: true,
    put_many_calibration_policies_request: true,
    delete_calibration_policy_request: true,
    delete_many_calibration_policies_request: true,
    list_calibration_policy_versions_request: true,
    get_calibration_policy_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.calibration_policies_events.created',
    updated: 'synthetic.v1.calibration_policies_events.updated',
    deleted: 'synthetic.v1.calibration_policies_events.deleted',
} as const;

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
import type { SensitivityMethod } from '../domain/sensitivity_method.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SensitivityMethodKey {
    code: string;
}

export interface SensitivityMethodWrite {
    code: string;
    name: string;
    description: string;
    needs_asset_class: boolean;
    display_order: number;
}

export interface SensitivityMethodChange {
    write: SensitivityMethodWrite;
    precondition: Precondition;
}

export interface SensitivityMethodRemoval {
    key: SensitivityMethodKey;
    precondition: Precondition;
}

export interface SensitivityMethodLookup {
    key: SensitivityMethodKey;
    sensitivity_method: SensitivityMethod | null;
}

export interface SensitivityMethodsFilter {
    code_one_of: string[] | null;
}

export interface SensitivityMethodEvent {
    event_id: string;
    key: SensitivityMethodKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SensitivityMethodVersionKey {
    sensitivity_method: SensitivityMethodKey;
    version: number;
}

export interface SensitivityMethodVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSensitivityMethodsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SensitivityMethodsFilter | null;
    as_of: string | null;
}

export interface ListSensitivityMethodsResponse {
    result: Result;
    methods: SensitivityMethod[];
    total: number;
}

export interface GetSensitivityMethodRequest {
    key: SensitivityMethodKey;
}

export interface GetSensitivityMethodResponse {
    result: Result;
    sensitivity_method: SensitivityMethod | null;
}

export interface GetManySensitivityMethodsRequest {
    keys: SensitivityMethodKey[];
}

export interface GetManySensitivityMethodsResponse {
    result: Result;
    entries: SensitivityMethodLookup[];
}

export interface PutSensitivityMethodRequest {
    change: SensitivityMethodChange;
    intent: ChangeIntent;
}

export interface PutSensitivityMethodResponse {
    result: Result;
    sensitivity_method: SensitivityMethod | null;
}

export interface PutManySensitivityMethodsRequest {
    changes: SensitivityMethodChange[];
    intent: ChangeIntent;
}

export interface PutManySensitivityMethodsResponse {
    result: Result;
    methods: SensitivityMethod[];
}

export interface DeleteSensitivityMethodRequest {
    removal: SensitivityMethodRemoval;
    intent: ChangeIntent;
}

export interface DeleteSensitivityMethodResponse {
    result: Result;
}

export interface DeleteManySensitivityMethodsRequest {
    removals: SensitivityMethodRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySensitivityMethodsResponse {
    result: Result;
}

export interface ListSensitivityMethodVersionsRequest {
    key: SensitivityMethodKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SensitivityMethodVersionsFilter | null;
}

export interface ListSensitivityMethodVersionsResponse {
    result: Result;
    versions: SensitivityMethod[];
    total: number;
}

export interface GetSensitivityMethodVersionRequest {
    key: SensitivityMethodVersionKey;
}

export interface GetSensitivityMethodVersionResponse {
    result: Result;
    version: SensitivityMethod | null;
}

export const subjects = {
    list_sensitivity_methods_request: 'analytics.v1.sensitivity_methods.list',
    get_sensitivity_method_request: 'analytics.v1.sensitivity_methods.get',
    get_many_sensitivity_methods_request: 'analytics.v1.sensitivity_methods.get_many',
    put_sensitivity_method_request: 'analytics.v1.sensitivity_methods.put',
    put_many_sensitivity_methods_request: 'analytics.v1.sensitivity_methods.put_many',
    delete_sensitivity_method_request: 'analytics.v1.sensitivity_methods.delete',
    delete_many_sensitivity_methods_request: 'analytics.v1.sensitivity_methods.delete_many',
    list_sensitivity_method_versions_request: 'analytics.v1.sensitivity_methods_versions.list',
    get_sensitivity_method_version_request: 'analytics.v1.sensitivity_methods_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_sensitivity_methods_request: true,
    get_sensitivity_method_request: true,
    get_many_sensitivity_methods_request: true,
    put_sensitivity_method_request: true,
    put_many_sensitivity_methods_request: true,
    delete_sensitivity_method_request: true,
    delete_many_sensitivity_methods_request: true,
    list_sensitivity_method_versions_request: true,
    get_sensitivity_method_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.sensitivity_methods_events.created',
    updated: 'analytics.v1.sensitivity_methods_events.updated',
    deleted: 'analytics.v1.sensitivity_methods_events.deleted',
} as const;

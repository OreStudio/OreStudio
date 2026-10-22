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
import type { EngineSettingsFixings } from '../domain/engine_settings_fixings.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface EngineSettingsFixingsKey {
    id: string;
}

export interface EngineSettingsFixingsWrite {
    id: string;
    engine_settings_id: string;
    imply_todays_fixings: boolean | null;
    include_todays_cash_flows: boolean | null;
    include_reference_date_events: boolean | null;
    enrich_index_fixings: boolean | null;
    ignore_fixing_lag: number | null;
    ignore_fixing_lead: number | null;
}

export interface EngineSettingsFixingsChange {
    write: EngineSettingsFixingsWrite;
    precondition: Precondition;
}

export interface EngineSettingsFixingsRemoval {
    key: EngineSettingsFixingsKey;
    precondition: Precondition;
}

export interface EngineSettingsFixingsLookup {
    key: EngineSettingsFixingsKey;
    engine_settings_fixings: EngineSettingsFixings | null;
}

export interface EngineSettingsFixingsFilter {
    engine_settings_id: string | null;
    id_one_of: string[] | null;
    engine_settings_id_one_of: string[] | null;
}

export interface EngineSettingsFixingsEvent {
    event_id: string;
    key: EngineSettingsFixingsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EngineSettingsFixingsVersionKey {
    engine_settings_fixings: EngineSettingsFixingsKey;
    version: number;
}

export interface EngineSettingsFixingsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEngineSettingsFixingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsFixingsFilter | null;
    as_of: string | null;
}

export interface ListEngineSettingsFixingsResponse {
    result: Result;
    engine_settings_fixings: EngineSettingsFixings[];
    total: number;
}

export interface GetEngineSettingsFixingsRequest {
    key: EngineSettingsFixingsKey;
}

export interface GetEngineSettingsFixingsResponse {
    result: Result;
    engine_settings_fixings: EngineSettingsFixings | null;
}

export interface GetManyEngineSettingsFixingsRequest {
    keys: EngineSettingsFixingsKey[];
}

export interface GetManyEngineSettingsFixingsResponse {
    result: Result;
    entries: EngineSettingsFixingsLookup[];
}

export interface PutEngineSettingsFixingsRequest {
    change: EngineSettingsFixingsChange;
    intent: ChangeIntent;
}

export interface PutEngineSettingsFixingsResponse {
    result: Result;
    engine_settings_fixings: EngineSettingsFixings | null;
}

export interface PutManyEngineSettingsFixingsRequest {
    changes: EngineSettingsFixingsChange[];
    intent: ChangeIntent;
}

export interface PutManyEngineSettingsFixingsResponse {
    result: Result;
    engine_settings_fixings: EngineSettingsFixings[];
}

export interface DeleteEngineSettingsFixingsRequest {
    removal: EngineSettingsFixingsRemoval;
    intent: ChangeIntent;
}

export interface DeleteEngineSettingsFixingsResponse {
    result: Result;
}

export interface DeleteManyEngineSettingsFixingsRequest {
    removals: EngineSettingsFixingsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEngineSettingsFixingsResponse {
    result: Result;
}

export interface ListByEngineSettingsIdEngineSettingsFixingsRequest {
    engine_settings_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsFixingsFilter | null;
}

export interface ListByEngineSettingsIdEngineSettingsFixingsResponse {
    result: Result;
    engine_settings_fixings: EngineSettingsFixings[];
    total: number;
}

export interface ListEngineSettingsFixingsVersionsRequest {
    key: EngineSettingsFixingsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsFixingsVersionsFilter | null;
}

export interface ListEngineSettingsFixingsVersionsResponse {
    result: Result;
    versions: EngineSettingsFixings[];
    total: number;
}

export interface GetEngineSettingsFixingsVersionRequest {
    key: EngineSettingsFixingsVersionKey;
}

export interface GetEngineSettingsFixingsVersionResponse {
    result: Result;
    version: EngineSettingsFixings | null;
}

export const subjects = {
    list_engine_settings_fixings_request: 'reporting.v1.engine_settings_fixings.list',
    get_engine_settings_fixings_request: 'reporting.v1.engine_settings_fixings.get',
    get_many_engine_settings_fixings_request: 'reporting.v1.engine_settings_fixings.get_many',
    put_engine_settings_fixings_request: 'reporting.v1.engine_settings_fixings.put',
    put_many_engine_settings_fixings_request: 'reporting.v1.engine_settings_fixings.put_many',
    delete_engine_settings_fixings_request: 'reporting.v1.engine_settings_fixings.delete',
    delete_many_engine_settings_fixings_request: 'reporting.v1.engine_settings_fixings.delete_many',
    list_by_engine_settings_id_engine_settings_fixings_request:
        'reporting.v1.engine_settings_fixings.list_by_engine_settings_id',
    list_engine_settings_fixings_versions_request:
        'reporting.v1.engine_settings_fixings_versions.list',
    get_engine_settings_fixings_version_request:
        'reporting.v1.engine_settings_fixings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_engine_settings_fixings_request: true,
    get_engine_settings_fixings_request: true,
    get_many_engine_settings_fixings_request: true,
    put_engine_settings_fixings_request: true,
    put_many_engine_settings_fixings_request: true,
    delete_engine_settings_fixings_request: true,
    delete_many_engine_settings_fixings_request: true,
    list_by_engine_settings_id_engine_settings_fixings_request: true,
    list_engine_settings_fixings_versions_request: true,
    get_engine_settings_fixings_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.engine_settings_fixings_events.created',
    updated: 'reporting.v1.engine_settings_fixings_events.updated',
    deleted: 'reporting.v1.engine_settings_fixings_events.deleted',
} as const;

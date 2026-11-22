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
import type { StoppedSimulatedSource } from '../domain/stopped_simulated_source.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StoppedSimulatedSourceKey {
    source_name: string;
}

export interface StoppedSimulatedSourceWrite {
    source_name: string;
    provider_code: string;
}

export interface StoppedSimulatedSourceChange {
    write: StoppedSimulatedSourceWrite;
    precondition: Precondition;
}

export interface StoppedSimulatedSourceRemoval {
    key: StoppedSimulatedSourceKey;
    precondition: Precondition;
}

export interface StoppedSimulatedSourceLookup {
    key: StoppedSimulatedSourceKey;
    stopped_simulated_source: StoppedSimulatedSource | null;
}

export interface StoppedSimulatedSourcesFilter {
    source_name_one_of: string[] | null;
}

export interface StoppedSimulatedSourceEvent {
    event_id: string;
    key: StoppedSimulatedSourceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StoppedSimulatedSourceVersionKey {
    stopped_simulated_source: StoppedSimulatedSourceKey;
    version: number;
}

export interface StoppedSimulatedSourceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStoppedSimulatedSourcesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StoppedSimulatedSourcesFilter | null;
    as_of: string | null;
}

export interface ListStoppedSimulatedSourcesResponse {
    result: Result;
    stopped_sources: StoppedSimulatedSource[];
    total: number;
}

export interface GetStoppedSimulatedSourceRequest {
    key: StoppedSimulatedSourceKey;
}

export interface GetStoppedSimulatedSourceResponse {
    result: Result;
    stopped_simulated_source: StoppedSimulatedSource | null;
}

export interface GetManyStoppedSimulatedSourcesRequest {
    keys: StoppedSimulatedSourceKey[];
}

export interface GetManyStoppedSimulatedSourcesResponse {
    result: Result;
    entries: StoppedSimulatedSourceLookup[];
}

export interface PutStoppedSimulatedSourceRequest {
    change: StoppedSimulatedSourceChange;
    intent: ChangeIntent;
}

export interface PutStoppedSimulatedSourceResponse {
    result: Result;
    stopped_simulated_source: StoppedSimulatedSource | null;
}

export interface PutManyStoppedSimulatedSourcesRequest {
    changes: StoppedSimulatedSourceChange[];
    intent: ChangeIntent;
}

export interface PutManyStoppedSimulatedSourcesResponse {
    result: Result;
    stopped_sources: StoppedSimulatedSource[];
}

export interface DeleteStoppedSimulatedSourceRequest {
    removal: StoppedSimulatedSourceRemoval;
    intent: ChangeIntent;
}

export interface DeleteStoppedSimulatedSourceResponse {
    result: Result;
}

export interface DeleteManyStoppedSimulatedSourcesRequest {
    removals: StoppedSimulatedSourceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStoppedSimulatedSourcesResponse {
    result: Result;
}

export interface ListStoppedSimulatedSourceVersionsRequest {
    key: StoppedSimulatedSourceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StoppedSimulatedSourceVersionsFilter | null;
}

export interface ListStoppedSimulatedSourceVersionsResponse {
    result: Result;
    versions: StoppedSimulatedSource[];
    total: number;
}

export interface GetStoppedSimulatedSourceVersionRequest {
    key: StoppedSimulatedSourceVersionKey;
}

export interface GetStoppedSimulatedSourceVersionResponse {
    result: Result;
    version: StoppedSimulatedSource | null;
}

export const subjects = {
    list_stopped_simulated_sources_request: 'synthetic.v1.stopped_simulated_sources.list',
    get_stopped_simulated_source_request: 'synthetic.v1.stopped_simulated_sources.get',
    get_many_stopped_simulated_sources_request: 'synthetic.v1.stopped_simulated_sources.get_many',
    put_stopped_simulated_source_request: 'synthetic.v1.stopped_simulated_sources.put',
    put_many_stopped_simulated_sources_request: 'synthetic.v1.stopped_simulated_sources.put_many',
    delete_stopped_simulated_source_request: 'synthetic.v1.stopped_simulated_sources.delete',
    delete_many_stopped_simulated_sources_request:
        'synthetic.v1.stopped_simulated_sources.delete_many',
    list_stopped_simulated_source_versions_request:
        'synthetic.v1.stopped_simulated_sources_versions.list',
    get_stopped_simulated_source_version_request:
        'synthetic.v1.stopped_simulated_sources_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_stopped_simulated_sources_request: true,
    get_stopped_simulated_source_request: true,
    get_many_stopped_simulated_sources_request: true,
    put_stopped_simulated_source_request: true,
    put_many_stopped_simulated_sources_request: true,
    delete_stopped_simulated_source_request: true,
    delete_many_stopped_simulated_sources_request: true,
    list_stopped_simulated_source_versions_request: true,
    get_stopped_simulated_source_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.stopped_simulated_sources.created',
    updated: 'synthetic.v1.events.stopped_simulated_sources.updated',
    deleted: 'synthetic.v1.events.stopped_simulated_sources.deleted',
} as const;

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
import type { ObservationLineage } from '../domain/observation_lineage.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ObservationLineageKey {
    id: string;
}

export interface ObservationLineageWrite {
    id: string;
    party_id: string;
    series_id: string;
    observation_datetime: string;
    point_id: string;
    derivation_config_id: string;
    derivation_config_version: number;
    source_as_of: string;
    source_series_ids: string;
}

export interface ObservationLineageChange {
    write: ObservationLineageWrite;
    precondition: Precondition;
}

export interface ObservationLineageRemoval {
    key: ObservationLineageKey;
    precondition: Precondition;
}

export interface ObservationLineageLookup {
    key: ObservationLineageKey;
    observation_lineage: ObservationLineage | null;
}

export interface ObservationLineageEvent {
    event_id: string;
    key: ObservationLineageKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ObservationLineageVersionKey {
    observation_lineage: ObservationLineageKey;
    version: number;
}

export interface ObservationLineageVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListObservationLineagesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListObservationLineagesResponse {
    result: Result;
    observation_lineages: ObservationLineage[];
    total: number;
}

export interface GetObservationLineageRequest {
    key: ObservationLineageKey;
}

export interface GetObservationLineageResponse {
    result: Result;
    observation_lineage: ObservationLineage | null;
}

export interface GetManyObservationLineagesRequest {
    keys: ObservationLineageKey[];
}

export interface GetManyObservationLineagesResponse {
    result: Result;
    entries: ObservationLineageLookup[];
}

export interface PutObservationLineageRequest {
    change: ObservationLineageChange;
    intent: ChangeIntent;
}

export interface PutObservationLineageResponse {
    result: Result;
    observation_lineage: ObservationLineage | null;
}

export interface PutManyObservationLineagesRequest {
    changes: ObservationLineageChange[];
    intent: ChangeIntent;
}

export interface PutManyObservationLineagesResponse {
    result: Result;
    observation_lineages: ObservationLineage[];
}

export interface DeleteObservationLineageRequest {
    removal: ObservationLineageRemoval;
    intent: ChangeIntent;
}

export interface DeleteObservationLineageResponse {
    result: Result;
}

export interface DeleteManyObservationLineagesRequest {
    removals: ObservationLineageRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyObservationLineagesResponse {
    result: Result;
}

export interface ListObservationLineageVersionsRequest {
    key: ObservationLineageKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ObservationLineageVersionsFilter | null;
}

export interface ListObservationLineageVersionsResponse {
    result: Result;
    versions: ObservationLineage[];
    total: number;
}

export interface GetObservationLineageVersionRequest {
    key: ObservationLineageVersionKey;
}

export interface GetObservationLineageVersionResponse {
    result: Result;
    version: ObservationLineage | null;
}

export const subjects = {
    list_observation_lineages_request: 'marketdata.v1.observation_lineages.list',
    get_observation_lineage_request: 'marketdata.v1.observation_lineages.get',
    get_many_observation_lineages_request: 'marketdata.v1.observation_lineages.get_many',
    put_observation_lineage_request: 'marketdata.v1.observation_lineages.put',
    put_many_observation_lineages_request: 'marketdata.v1.observation_lineages.put_many',
    delete_observation_lineage_request: 'marketdata.v1.observation_lineages.delete',
    delete_many_observation_lineages_request: 'marketdata.v1.observation_lineages.delete_many',
    list_observation_lineage_versions_request: 'marketdata.v1.observation_lineages_versions.list',
    get_observation_lineage_version_request: 'marketdata.v1.observation_lineages_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_observation_lineages_request: true,
    get_observation_lineage_request: true,
    get_many_observation_lineages_request: true,
    put_observation_lineage_request: true,
    put_many_observation_lineages_request: true,
    delete_observation_lineage_request: true,
    delete_many_observation_lineages_request: true,
    list_observation_lineage_versions_request: true,
    get_observation_lineage_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.observation_lineages_events.created',
    updated: 'marketdata.v1.observation_lineages_events.updated',
    deleted: 'marketdata.v1.observation_lineages_events.deleted',
} as const;

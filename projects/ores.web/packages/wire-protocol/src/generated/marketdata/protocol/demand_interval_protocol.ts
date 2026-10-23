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
import type { DemandInterval } from '../domain/demand_interval.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DemandIntervalKey {
    oresmd_uri: string;
}

export interface DemandIntervalWrite {
    id: string;
    party_id: string;
    provider_code: string;
    oresmd_uri: string;
    interval_from: string;
    source_id: string;
    interval_to: string | null;
}

export interface DemandIntervalChange {
    write: DemandIntervalWrite;
    precondition: Precondition;
}

export interface DemandIntervalRemoval {
    key: DemandIntervalKey;
    precondition: Precondition;
}

export interface DemandIntervalLookup {
    key: DemandIntervalKey;
    demand_interval: DemandInterval | null;
}

export interface DemandIntervalsFilter {
    party_id: string | null;
    provider_code: string | null;
    source_id: string | null;
    id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    provider_code_one_of: string[] | null;
    source_id_one_of: string[] | null;
}

export interface DemandIntervalEvent {
    event_id: string;
    key: DemandIntervalKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DemandIntervalVersionKey {
    demand_interval: DemandIntervalKey;
    version: number;
}

export interface DemandIntervalVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDemandIntervalsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DemandIntervalsFilter | null;
    as_of: string | null;
}

export interface ListDemandIntervalsResponse {
    result: Result;
    demand_intervals: DemandInterval[];
    total: number;
}

export interface GetDemandIntervalRequest {
    key: DemandIntervalKey;
}

export interface GetDemandIntervalResponse {
    result: Result;
    demand_interval: DemandInterval | null;
}

export interface GetManyDemandIntervalsRequest {
    keys: DemandIntervalKey[];
}

export interface GetManyDemandIntervalsResponse {
    result: Result;
    entries: DemandIntervalLookup[];
}

export interface PutDemandIntervalRequest {
    change: DemandIntervalChange;
    intent: ChangeIntent;
}

export interface PutDemandIntervalResponse {
    result: Result;
    demand_interval: DemandInterval | null;
}

export interface PutManyDemandIntervalsRequest {
    changes: DemandIntervalChange[];
    intent: ChangeIntent;
}

export interface PutManyDemandIntervalsResponse {
    result: Result;
    demand_intervals: DemandInterval[];
}

export interface DeleteDemandIntervalRequest {
    removal: DemandIntervalRemoval;
    intent: ChangeIntent;
}

export interface DeleteDemandIntervalResponse {
    result: Result;
}

export interface DeleteManyDemandIntervalsRequest {
    removals: DemandIntervalRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDemandIntervalsResponse {
    result: Result;
}

export interface ListDemandIntervalVersionsRequest {
    key: DemandIntervalKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DemandIntervalVersionsFilter | null;
}

export interface ListDemandIntervalVersionsResponse {
    result: Result;
    versions: DemandInterval[];
    total: number;
}

export interface GetDemandIntervalVersionRequest {
    key: DemandIntervalVersionKey;
}

export interface GetDemandIntervalVersionResponse {
    result: Result;
    version: DemandInterval | null;
}

export const subjects = {
    list_demand_intervals_request: 'marketdata.v1.demand_intervals.list',
    get_demand_interval_request: 'marketdata.v1.demand_intervals.get',
    get_many_demand_intervals_request: 'marketdata.v1.demand_intervals.get_many',
    put_demand_interval_request: 'marketdata.v1.demand_intervals.put',
    put_many_demand_intervals_request: 'marketdata.v1.demand_intervals.put_many',
    delete_demand_interval_request: 'marketdata.v1.demand_intervals.delete',
    delete_many_demand_intervals_request: 'marketdata.v1.demand_intervals.delete_many',
    list_demand_interval_versions_request: 'marketdata.v1.demand_intervals_versions.list',
    get_demand_interval_version_request: 'marketdata.v1.demand_intervals_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_demand_intervals_request: true,
    get_demand_interval_request: true,
    get_many_demand_intervals_request: true,
    put_demand_interval_request: true,
    put_many_demand_intervals_request: true,
    delete_demand_interval_request: true,
    delete_many_demand_intervals_request: true,
    list_demand_interval_versions_request: true,
    get_demand_interval_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.demand_intervals_events.created',
    updated: 'marketdata.v1.demand_intervals_events.updated',
    deleted: 'marketdata.v1.demand_intervals_events.deleted',
} as const;

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
import type { CounterpartyChange } from '../domain/counterparty_change.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CounterpartyChangeKey {
    id: string;
}

export interface CounterpartyChangeLookup {
    key: CounterpartyChangeKey;
    counterparty_change: CounterpartyChange | null;
}

export interface CounterpartyChangesFilter {
    request_id: string | null;
    id_one_of: string[] | null;
    request_id_one_of: string[] | null;
}

export interface CounterpartyChangeEvent {
    event_id: string;
    key: CounterpartyChangeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CounterpartyChangeVersionKey {
    counterparty_change: CounterpartyChangeKey;
    version: number;
}

export interface CounterpartyChangeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCounterpartyChangesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyChangesFilter | null;
    as_of: string | null;
}

export interface ListCounterpartyChangesResponse {
    result: Result;
    counterparty_changes: CounterpartyChange[];
    total: number;
}

export interface GetCounterpartyChangeRequest {
    key: CounterpartyChangeKey;
}

export interface GetCounterpartyChangeResponse {
    result: Result;
    counterparty_change: CounterpartyChange | null;
}

export interface GetManyCounterpartyChangesRequest {
    keys: CounterpartyChangeKey[];
}

export interface GetManyCounterpartyChangesResponse {
    result: Result;
    entries: CounterpartyChangeLookup[];
}

export interface ListByRequestIdCounterpartyChangesRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyChangesFilter | null;
}

export interface ListByRequestIdCounterpartyChangesResponse {
    result: Result;
    counterparty_changes: CounterpartyChange[];
    total: number;
}

export interface ListCounterpartyChangeVersionsRequest {
    key: CounterpartyChangeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyChangeVersionsFilter | null;
}

export interface ListCounterpartyChangeVersionsResponse {
    result: Result;
    versions: CounterpartyChange[];
    total: number;
}

export interface GetCounterpartyChangeVersionRequest {
    key: CounterpartyChangeVersionKey;
}

export interface GetCounterpartyChangeVersionResponse {
    result: Result;
    version: CounterpartyChange | null;
}

export const subjects = {
    list_counterparty_changes_request: 'refdata.v1.counterparty_changes.list',
    get_counterparty_change_request: 'refdata.v1.counterparty_changes.get',
    get_many_counterparty_changes_request: 'refdata.v1.counterparty_changes.get_many',
    list_by_request_id_counterparty_changes_request:
        'refdata.v1.counterparty_changes.list_by_request_id',
    list_counterparty_change_versions_request: 'refdata.v1.counterparty_changes_versions.list',
    get_counterparty_change_version_request: 'refdata.v1.counterparty_changes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_counterparty_changes_request: true,
    get_counterparty_change_request: true,
    get_many_counterparty_changes_request: true,
    list_by_request_id_counterparty_changes_request: true,
    list_counterparty_change_versions_request: true,
    get_counterparty_change_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.counterparty_changes_events.created',
    updated: 'refdata.v1.counterparty_changes_events.updated',
    deleted: 'refdata.v1.counterparty_changes_events.deleted',
} as const;

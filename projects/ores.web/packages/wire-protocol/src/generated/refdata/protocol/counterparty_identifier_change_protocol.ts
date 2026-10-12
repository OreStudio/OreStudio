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
import type { CounterpartyIdentifierChange } from '../domain/counterparty_identifier_change.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CounterpartyIdentifierChangeKey {
    id: string;
}

export interface CounterpartyIdentifierChangeLookup {
    key: CounterpartyIdentifierChangeKey;
    counterparty_identifier_change: CounterpartyIdentifierChange | null;
}

export interface CounterpartyIdentifierChangesFilter {
    request_id: string | null;
    id_one_of: string[] | null;
    request_id_one_of: string[] | null;
}

export interface CounterpartyIdentifierChangeEvent {
    event_id: string;
    key: CounterpartyIdentifierChangeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CounterpartyIdentifierChangeVersionKey {
    counterparty_identifier_change: CounterpartyIdentifierChangeKey;
    version: number;
}

export interface CounterpartyIdentifierChangeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCounterpartyIdentifierChangesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyIdentifierChangesFilter | null;
    as_of: string | null;
}

export interface ListCounterpartyIdentifierChangesResponse {
    result: Result;
    counterparty_identifier_changes: CounterpartyIdentifierChange[];
    total: number;
}

export interface GetCounterpartyIdentifierChangeRequest {
    key: CounterpartyIdentifierChangeKey;
}

export interface GetCounterpartyIdentifierChangeResponse {
    result: Result;
    counterparty_identifier_change: CounterpartyIdentifierChange | null;
}

export interface GetManyCounterpartyIdentifierChangesRequest {
    keys: CounterpartyIdentifierChangeKey[];
}

export interface GetManyCounterpartyIdentifierChangesResponse {
    result: Result;
    entries: CounterpartyIdentifierChangeLookup[];
}

export interface ListByRequestIdCounterpartyIdentifierChangesRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyIdentifierChangesFilter | null;
}

export interface ListByRequestIdCounterpartyIdentifierChangesResponse {
    result: Result;
    counterparty_identifier_changes: CounterpartyIdentifierChange[];
    total: number;
}

export interface ListCounterpartyIdentifierChangeVersionsRequest {
    key: CounterpartyIdentifierChangeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyIdentifierChangeVersionsFilter | null;
}

export interface ListCounterpartyIdentifierChangeVersionsResponse {
    result: Result;
    versions: CounterpartyIdentifierChange[];
    total: number;
}

export interface GetCounterpartyIdentifierChangeVersionRequest {
    key: CounterpartyIdentifierChangeVersionKey;
}

export interface GetCounterpartyIdentifierChangeVersionResponse {
    result: Result;
    version: CounterpartyIdentifierChange | null;
}

export const subjects = {
    list_counterparty_identifier_changes_request: 'refdata.v1.counterparty_identifier_changes.list',
    get_counterparty_identifier_change_request: 'refdata.v1.counterparty_identifier_changes.get',
    get_many_counterparty_identifier_changes_request:
        'refdata.v1.counterparty_identifier_changes.get_many',
    list_by_request_id_counterparty_identifier_changes_request:
        'refdata.v1.counterparty_identifier_changes.list_by_request_id',
    list_counterparty_identifier_change_versions_request:
        'refdata.v1.counterparty_identifier_changes_versions.list',
    get_counterparty_identifier_change_version_request:
        'refdata.v1.counterparty_identifier_changes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_counterparty_identifier_changes_request: true,
    get_counterparty_identifier_change_request: true,
    get_many_counterparty_identifier_changes_request: true,
    list_by_request_id_counterparty_identifier_changes_request: true,
    list_counterparty_identifier_change_versions_request: true,
    get_counterparty_identifier_change_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.counterparty_identifier_changes_events.created',
    updated: 'refdata.v1.counterparty_identifier_changes_events.updated',
    deleted: 'refdata.v1.counterparty_identifier_changes_events.deleted',
} as const;

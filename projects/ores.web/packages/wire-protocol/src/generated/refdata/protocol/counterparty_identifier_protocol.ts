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
import type { CounterpartyIdentifier } from '../domain/counterparty_identifier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CounterpartyIdentifierKey {
    id_value: string;
}

export interface CounterpartyIdentifierWrite {
    id: string;
    counterparty_id: string;
    id_scheme: string;
    id_value: string;
    description: string;
}

export interface CounterpartyIdentifierChange {
    write: CounterpartyIdentifierWrite;
    precondition: Precondition;
}

export interface CounterpartyIdentifierRemoval {
    key: CounterpartyIdentifierKey;
    precondition: Precondition;
}

export interface CounterpartyIdentifierLookup {
    key: CounterpartyIdentifierKey;
    counterparty_identifier: CounterpartyIdentifier | null;
}

export interface CounterpartyIdentifiersFilter {
    counterparty_id: string | null;
}

export interface CounterpartyIdentifierEvent {
    event_id: string;
    key: CounterpartyIdentifierKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CounterpartyIdentifierVersionKey {
    counterparty_identifier: CounterpartyIdentifierKey;
    version: number;
}

export interface CounterpartyIdentifierVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCounterpartyIdentifiersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyIdentifiersFilter | null;
}

export interface ListCounterpartyIdentifiersResponse {
    result: Result;
    counterparty_identifiers: CounterpartyIdentifier[];
    total: number;
}

export interface GetCounterpartyIdentifierRequest {
    key: CounterpartyIdentifierKey;
}

export interface GetCounterpartyIdentifierResponse {
    result: Result;
    counterparty_identifier: CounterpartyIdentifier | null;
}

export interface GetManyCounterpartyIdentifiersRequest {
    keys: CounterpartyIdentifierKey[];
}

export interface GetManyCounterpartyIdentifiersResponse {
    result: Result;
    entries: CounterpartyIdentifierLookup[];
}

export interface PutCounterpartyIdentifierRequest {
    change: CounterpartyIdentifierChange;
    intent: ChangeIntent;
}

export interface PutCounterpartyIdentifierResponse {
    result: Result;
    counterparty_identifier: CounterpartyIdentifier;
}

export interface PutManyCounterpartyIdentifiersRequest {
    changes: CounterpartyIdentifierChange[];
    intent: ChangeIntent;
}

export interface PutManyCounterpartyIdentifiersResponse {
    result: Result;
    counterparty_identifiers: CounterpartyIdentifier[];
}

export interface DeleteCounterpartyIdentifierRequest {
    removal: CounterpartyIdentifierRemoval;
    intent: ChangeIntent;
}

export interface DeleteCounterpartyIdentifierResponse {
    result: Result;
}

export interface DeleteManyCounterpartyIdentifiersRequest {
    removals: CounterpartyIdentifierRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCounterpartyIdentifiersResponse {
    result: Result;
}

export interface ListByCounterpartyIdCounterpartyIdentifiersRequest {
    counterparty_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyIdentifiersFilter | null;
}

export interface ListByCounterpartyIdCounterpartyIdentifiersResponse {
    result: Result;
    counterparty_identifiers: CounterpartyIdentifier[];
    total: number;
}

export interface ListCounterpartyIdentifierVersionsRequest {
    key: CounterpartyIdentifierKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyIdentifierVersionsFilter | null;
}

export interface ListCounterpartyIdentifierVersionsResponse {
    result: Result;
    versions: CounterpartyIdentifier[];
    total: number;
}

export interface GetCounterpartyIdentifierVersionRequest {
    key: CounterpartyIdentifierVersionKey;
}

export interface GetCounterpartyIdentifierVersionResponse {
    result: Result;
    version: CounterpartyIdentifier;
}

export const subjects = {
    list_counterparty_identifiers_request: "refdata.v1.counterparty_identifiers.list",
    get_counterparty_identifier_request: "refdata.v1.counterparty_identifiers.get",
    get_many_counterparty_identifiers_request: "refdata.v1.counterparty_identifiers.get_many",
    put_counterparty_identifier_request: "refdata.v1.counterparty_identifiers.put",
    put_many_counterparty_identifiers_request: "refdata.v1.counterparty_identifiers.put_many",
    delete_counterparty_identifier_request: "refdata.v1.counterparty_identifiers.delete",
    delete_many_counterparty_identifiers_request: "refdata.v1.counterparty_identifiers.delete_many",
    list_by_counterparty_id_counterparty_identifiers_request: "refdata.v1.counterparty_identifiers.list_by_counterparty_id",
    list_counterparty_identifier_versions_request: "refdata.v1.counterparty_identifiers_versions.list",
    get_counterparty_identifier_version_request: "refdata.v1.counterparty_identifiers_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_counterparty_identifiers_request: true,
    get_counterparty_identifier_request: true,
    get_many_counterparty_identifiers_request: true,
    put_counterparty_identifier_request: true,
    put_many_counterparty_identifiers_request: true,
    delete_counterparty_identifier_request: true,
    delete_many_counterparty_identifiers_request: true,
    list_by_counterparty_id_counterparty_identifiers_request: true,
    list_counterparty_identifier_versions_request: true,
    get_counterparty_identifier_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.counterparty_identifiers_events.created",
    updated: "refdata.v1.counterparty_identifiers_events.updated",
    deleted: "refdata.v1.counterparty_identifiers_events.deleted",
} as const;

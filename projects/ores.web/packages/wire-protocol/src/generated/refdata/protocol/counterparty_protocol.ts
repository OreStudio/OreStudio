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
import type { Counterparty } from '../domain/counterparty.js';
import type { CounterpartyContactInformation } from '../domain/counterparty_contact_information.js';
import type { CounterpartyIdentifier } from '../domain/counterparty_identifier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CounterpartyKey {
    short_code: string;
}

export interface CounterpartyWrite {
    id: string;
    short_code: string;
    full_name: string;
    transliterated_name: string | null;
    party_type: string;
    parent_counterparty_id: string | null;
    business_center_code: string;
    status: string;
    image_id: string | null;
}

export interface CounterpartyChange {
    write: CounterpartyWrite;
    precondition: Precondition;
}

export interface CounterpartyRemoval {
    key: CounterpartyKey;
    precondition: Precondition;
}

export interface CounterpartyLookup {
    key: CounterpartyKey;
    counterparty: Counterparty | null;
}

export interface CounterpartyEvent {
    event_id: string;
    key: CounterpartyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CounterpartyVersionKey {
    counterparty: CounterpartyKey;
    version: number;
}

export interface CounterpartyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCounterpartiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCounterpartiesResponse {
    result: Result;
    counterparties: Counterparty[];
    total: number;
}

export interface GetCounterpartyRequest {
    key: CounterpartyKey;
}

export interface GetCounterpartyResponse {
    result: Result;
    counterparty: Counterparty | null;
}

export interface GetManyCounterpartiesRequest {
    keys: CounterpartyKey[];
}

export interface GetManyCounterpartiesResponse {
    result: Result;
    entries: CounterpartyLookup[];
}

export interface PutCounterpartyRequest {
    change: CounterpartyChange;
    intent: ChangeIntent;
}

export interface PutCounterpartyResponse {
    result: Result;
    counterparty: Counterparty;
}

export interface PutManyCounterpartiesRequest {
    changes: CounterpartyChange[];
    intent: ChangeIntent;
}

export interface PutManyCounterpartiesResponse {
    result: Result;
    counterparties: Counterparty[];
}

export interface DeleteCounterpartyRequest {
    removal: CounterpartyRemoval;
    intent: ChangeIntent;
}

export interface DeleteCounterpartyResponse {
    result: Result;
}

export interface DeleteManyCounterpartiesRequest {
    removals: CounterpartyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCounterpartiesResponse {
    result: Result;
}

export interface ListCounterpartyVersionsRequest {
    key: CounterpartyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyVersionsFilter | null;
}

export interface ListCounterpartyVersionsResponse {
    result: Result;
    versions: Counterparty[];
    total: number;
}

export interface GetCounterpartyVersionRequest {
    key: CounterpartyVersionKey;
}

export interface GetCounterpartyVersionResponse {
    result: Result;
    version: Counterparty;
}

/**
 * @brief Reads a counterparty as it stood at a specific version, together
 * with its identifiers and contact information as they stood during that
 * same version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
export interface GetCounterpartyCompositeAsOfRequest {
    id: string;
    version: number;
}

export interface GetCounterpartyCompositeAsOfResponse {
    success: boolean;
    message: string;
    counterparty: Counterparty;
    identifiers: CounterpartyIdentifier[];
    contacts: CounterpartyContactInformation[];
}

export const subjects = {
    list_counterparties_request: "refdata.v1.counterparties.list",
    get_counterparty_request: "refdata.v1.counterparties.get",
    get_many_counterparties_request: "refdata.v1.counterparties.get_many",
    put_counterparty_request: "refdata.v1.counterparties.put",
    put_many_counterparties_request: "refdata.v1.counterparties.put_many",
    delete_counterparty_request: "refdata.v1.counterparties.delete",
    delete_many_counterparties_request: "refdata.v1.counterparties.delete_many",
    list_counterparty_versions_request: "refdata.v1.counterparties_versions.list",
    get_counterparty_version_request: "refdata.v1.counterparties_versions.get",
    get_counterparty_composite_as_of_request: "refdata.v1.counterparties.composite_as_of",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_counterparties_request: true,
    get_counterparty_request: true,
    get_many_counterparties_request: true,
    put_counterparty_request: true,
    put_many_counterparties_request: true,
    delete_counterparty_request: true,
    delete_many_counterparties_request: true,
    list_counterparty_versions_request: true,
    get_counterparty_version_request: true,
    get_counterparty_composite_as_of_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.counterparties_events.created",
    updated: "refdata.v1.counterparties_events.updated",
    deleted: "refdata.v1.counterparties_events.deleted",
} as const;

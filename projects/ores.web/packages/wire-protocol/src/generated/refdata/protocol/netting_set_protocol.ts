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
import type { NettingSet } from '../domain/netting_set.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface NettingSetKey {
    code: string;
}

export interface NettingSetWrite {
    id: string;
    code: string;
    party_id: string;
    netting_agreement_id: string | null;
    counterparty_id: string | null;
    call_type: string | null;
    initial_margin_type: string | null;
    risk_weight: number | null;
    description: string | null;
}

export interface NettingSetChange {
    write: NettingSetWrite;
    precondition: Precondition;
}

export interface NettingSetRemoval {
    key: NettingSetKey;
    precondition: Precondition;
}

export interface NettingSetLookup {
    key: NettingSetKey;
    netting_set: NettingSet | null;
}

export interface NettingSetsFilter {
    netting_agreement_id: string | null | null;
    id_one_of: string[] | null;
    netting_agreement_id_one_of: string[] | null;
}

export interface NettingSetEvent {
    event_id: string;
    key: NettingSetKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface NettingSetVersionKey {
    netting_set: NettingSetKey;
    version: number;
}

export interface NettingSetVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListNettingSetsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NettingSetsFilter | null;
    as_of: string | null;
}

export interface ListNettingSetsResponse {
    result: Result;
    netting_sets: NettingSet[];
    total: number;
}

export interface GetNettingSetRequest {
    key: NettingSetKey;
}

export interface GetNettingSetResponse {
    result: Result;
    netting_set: NettingSet | null;
}

export interface GetManyNettingSetsRequest {
    keys: NettingSetKey[];
}

export interface GetManyNettingSetsResponse {
    result: Result;
    entries: NettingSetLookup[];
}

export interface PutNettingSetRequest {
    change: NettingSetChange;
    intent: ChangeIntent;
}

export interface PutNettingSetResponse {
    result: Result;
    netting_set: NettingSet | null;
}

export interface PutManyNettingSetsRequest {
    changes: NettingSetChange[];
    intent: ChangeIntent;
}

export interface PutManyNettingSetsResponse {
    result: Result;
    netting_sets: NettingSet[];
}

export interface DeleteNettingSetRequest {
    removal: NettingSetRemoval;
    intent: ChangeIntent;
}

export interface DeleteNettingSetResponse {
    result: Result;
}

export interface DeleteManyNettingSetsRequest {
    removals: NettingSetRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNettingSetsResponse {
    result: Result;
}

export interface ListByNettingAgreementIdNettingSetsRequest {
    netting_agreement_id: string | null;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: NettingSetsFilter | null;
}

export interface ListByNettingAgreementIdNettingSetsResponse {
    result: Result;
    netting_sets: NettingSet[];
    total: number;
}

export interface ListNettingSetVersionsRequest {
    key: NettingSetKey;
    offset: number;
    limit: number;
    order: Order;
    filter: NettingSetVersionsFilter | null;
}

export interface ListNettingSetVersionsResponse {
    result: Result;
    versions: NettingSet[];
    total: number;
}

export interface GetNettingSetVersionRequest {
    key: NettingSetVersionKey;
}

export interface GetNettingSetVersionResponse {
    result: Result;
    version: NettingSet | null;
}

export const subjects = {
    list_netting_sets_request: 'refdata.v1.netting_sets.list',
    get_netting_set_request: 'refdata.v1.netting_sets.get',
    get_many_netting_sets_request: 'refdata.v1.netting_sets.get_many',
    put_netting_set_request: 'refdata.v1.netting_sets.put',
    put_many_netting_sets_request: 'refdata.v1.netting_sets.put_many',
    delete_netting_set_request: 'refdata.v1.netting_sets.delete',
    delete_many_netting_sets_request: 'refdata.v1.netting_sets.delete_many',
    list_by_netting_agreement_id_netting_sets_request:
        'refdata.v1.netting_sets.list_by_netting_agreement_id',
    list_netting_set_versions_request: 'refdata.v1.netting_sets_versions.list',
    get_netting_set_version_request: 'refdata.v1.netting_sets_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_netting_sets_request: true,
    get_netting_set_request: true,
    get_many_netting_sets_request: true,
    put_netting_set_request: true,
    put_many_netting_sets_request: true,
    delete_netting_set_request: true,
    delete_many_netting_sets_request: true,
    list_by_netting_agreement_id_netting_sets_request: true,
    list_netting_set_versions_request: true,
    get_netting_set_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.netting_sets_events.created',
    updated: 'refdata.v1.netting_sets_events.updated',
    deleted: 'refdata.v1.netting_sets_events.deleted',
} as const;

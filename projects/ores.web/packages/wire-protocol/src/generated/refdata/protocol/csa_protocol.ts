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
import type { Csa } from '../domain/csa.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CsaKey {
    id: string;
}

export interface CsaWrite {
    id: string;
    netting_set_id: string;
    is_active: boolean;
    bilateral: string | null;
    csa_currency: string | null;
    index_name: string | null;
    threshold_pay: number | null;
    threshold_receive: number | null;
    minimum_transfer_amount_pay: number | null;
    minimum_transfer_amount_receive: number | null;
    independent_amount_held: number | null;
    independent_amount_type: string | null;
    call_frequency: string | null;
    post_frequency: string | null;
    margin_period_of_risk: string | null;
    collateral_compounding_spread_receive: number | null;
    collateral_compounding_spread_pay: number | null;
    apply_initial_margin: boolean | null;
    initial_margin_type: string | null;
    calculate_im_amount: boolean | null;
    calculate_vm_amount: boolean | null;
    non_exempt_im_regulations: string | null;
}

export interface CsaChange {
    write: CsaWrite;
    precondition: Precondition;
}

export interface CsaRemoval {
    key: CsaKey;
    precondition: Precondition;
}

export interface CsaLookup {
    key: CsaKey;
    csa: Csa | null;
}

export interface CsasFilter {
    netting_set_id: string | null;
    id_one_of: string[] | null;
    netting_set_id_one_of: string[] | null;
}

export interface CsaEvent {
    event_id: string;
    key: CsaKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CsaVersionKey {
    csa: CsaKey;
    version: number;
}

export interface CsaVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCsasRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CsasFilter | null;
}

export interface ListCsasResponse {
    result: Result;
    csas: Csa[];
    total: number;
}

export interface GetCsaRequest {
    key: CsaKey;
}

export interface GetCsaResponse {
    result: Result;
    csa: Csa | null;
}

export interface GetManyCsasRequest {
    keys: CsaKey[];
}

export interface GetManyCsasResponse {
    result: Result;
    entries: CsaLookup[];
}

export interface PutCsaRequest {
    change: CsaChange;
    intent: ChangeIntent;
}

export interface PutCsaResponse {
    result: Result;
    csa: Csa | null;
}

export interface PutManyCsasRequest {
    changes: CsaChange[];
    intent: ChangeIntent;
}

export interface PutManyCsasResponse {
    result: Result;
    csas: Csa[];
}

export interface DeleteCsaRequest {
    removal: CsaRemoval;
    intent: ChangeIntent;
}

export interface DeleteCsaResponse {
    result: Result;
}

export interface DeleteManyCsasRequest {
    removals: CsaRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCsasResponse {
    result: Result;
}

export interface ListByNettingSetIdCsasRequest {
    netting_set_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CsasFilter | null;
}

export interface ListByNettingSetIdCsasResponse {
    result: Result;
    csas: Csa[];
    total: number;
}

export interface ListCsaVersionsRequest {
    key: CsaKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CsaVersionsFilter | null;
}

export interface ListCsaVersionsResponse {
    result: Result;
    versions: Csa[];
    total: number;
}

export interface GetCsaVersionRequest {
    key: CsaVersionKey;
}

export interface GetCsaVersionResponse {
    result: Result;
    version: Csa | null;
}

export const subjects = {
    list_csas_request: 'refdata.v1.csas.list',
    get_csa_request: 'refdata.v1.csas.get',
    get_many_csas_request: 'refdata.v1.csas.get_many',
    put_csa_request: 'refdata.v1.csas.put',
    put_many_csas_request: 'refdata.v1.csas.put_many',
    delete_csa_request: 'refdata.v1.csas.delete',
    delete_many_csas_request: 'refdata.v1.csas.delete_many',
    list_by_netting_set_id_csas_request: 'refdata.v1.csas.list_by_netting_set_id',
    list_csa_versions_request: 'refdata.v1.csas_versions.list',
    get_csa_version_request: 'refdata.v1.csas_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_csas_request: true,
    get_csa_request: true,
    get_many_csas_request: true,
    put_csa_request: true,
    put_many_csas_request: true,
    delete_csa_request: true,
    delete_many_csas_request: true,
    list_by_netting_set_id_csas_request: true,
    list_csa_versions_request: true,
    get_csa_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.csas_events.created',
    updated: 'refdata.v1.csas_events.updated',
    deleted: 'refdata.v1.csas_events.deleted',
} as const;

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
import type { PartyProductType } from '../domain/party_product_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface PartyProductTypeKey {
    party_id: string;
    product_type_code: string;
}

export interface PartyProductTypeWrite {
    party_id: string;
    product_type_code: string;
}

export interface PartyProductTypeChange {
    write: PartyProductTypeWrite;
    precondition: Precondition;
}

export interface PartyProductTypeRemoval {
    key: PartyProductTypeKey;
    precondition: Precondition;
}

export interface PartyProductTypeLookup {
    key: PartyProductTypeKey;
    party_product_type: PartyProductType | null;
}

export interface PartyProductTypesFilter {
    party_id: string | null;
    product_type_code: string | null;
    party_id_one_of: string[] | null;
    product_type_code_one_of: string[] | null;
}

export interface PartyProductTypeEvent {
    event_id: string;
    key: PartyProductTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListPartyProductTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PartyProductTypesFilter | null;
}

export interface ListPartyProductTypesResponse {
    result: Result;
    party_product_types: PartyProductType[];
    total: number;
}

export interface GetPartyProductTypeRequest {
    key: PartyProductTypeKey;
}

export interface GetPartyProductTypeResponse {
    result: Result;
    party_product_type: PartyProductType | null;
}

export interface GetManyPartyProductTypesRequest {
    keys: PartyProductTypeKey[];
}

export interface GetManyPartyProductTypesResponse {
    result: Result;
    entries: PartyProductTypeLookup[];
}

export interface PutPartyProductTypeRequest {
    change: PartyProductTypeChange;
    intent: ChangeIntent;
}

export interface PutPartyProductTypeResponse {
    result: Result;
    party_product_type: PartyProductType | null;
}

export interface PutManyPartyProductTypesRequest {
    changes: PartyProductTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyProductTypesResponse {
    result: Result;
    party_product_types: PartyProductType[];
}

export interface DeletePartyProductTypeRequest {
    removal: PartyProductTypeRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyProductTypeResponse {
    result: Result;
}

export interface DeleteManyPartyProductTypesRequest {
    removals: PartyProductTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyProductTypesResponse {
    result: Result;
}

export interface ListByPartyIdPartyProductTypesRequest {
    party_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyProductTypesFilter | null;
}

export interface ListByPartyIdPartyProductTypesResponse {
    result: Result;
    party_product_types: PartyProductType[];
    total: number;
}

export interface ListByProductTypeCodePartyProductTypesRequest {
    product_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyProductTypesFilter | null;
}

export interface ListByProductTypeCodePartyProductTypesResponse {
    result: Result;
    party_product_types: PartyProductType[];
    total: number;
}

export const subjects = {
    list_party_product_types_request: 'refdata.v1.party_product_types.list',
    get_party_product_type_request: 'refdata.v1.party_product_types.get',
    get_many_party_product_types_request: 'refdata.v1.party_product_types.get_many',
    put_party_product_type_request: 'refdata.v1.party_product_types.put',
    put_many_party_product_types_request: 'refdata.v1.party_product_types.put_many',
    delete_party_product_type_request: 'refdata.v1.party_product_types.delete',
    delete_many_party_product_types_request: 'refdata.v1.party_product_types.delete_many',
    list_by_party_id_party_product_types_request: 'refdata.v1.party_product_types.list_by_party_id',
    list_by_product_type_code_party_product_types_request:
        'refdata.v1.party_product_types.list_by_product_type_code',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_product_types_request: true,
    get_party_product_type_request: true,
    get_many_party_product_types_request: true,
    put_party_product_type_request: true,
    put_many_party_product_types_request: true,
    delete_party_product_type_request: true,
    delete_many_party_product_types_request: true,
    list_by_party_id_party_product_types_request: true,
    list_by_product_type_code_party_product_types_request: true,
} as const;

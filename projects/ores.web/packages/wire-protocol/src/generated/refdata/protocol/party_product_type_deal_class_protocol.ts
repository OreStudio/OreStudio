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
import type { PartyProductTypeDealClass } from '../domain/party_product_type_deal_class.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface PartyProductTypeDealClassKey {
    id: string;
}

export interface PartyProductTypeDealClassWrite {
    id: string;
    party_id: string;
    product_type_code: string;
    deal_class: string;
}

export interface PartyProductTypeDealClassChange {
    write: PartyProductTypeDealClassWrite;
    precondition: Precondition;
}

export interface PartyProductTypeDealClassRemoval {
    key: PartyProductTypeDealClassKey;
    precondition: Precondition;
}

export interface PartyProductTypeDealClassLookup {
    key: PartyProductTypeDealClassKey;
    party_product_type_deal_class: PartyProductTypeDealClass | null;
}

export interface PartyProductTypeDealClassesFilter {
    party_id: string | null;
    id_one_of: string[] | null;
    party_id_one_of: string[] | null;
}

export interface PartyProductTypeDealClassEvent {
    event_id: string;
    key: PartyProductTypeDealClassKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyProductTypeDealClassVersionKey {
    party_product_type_deal_class: PartyProductTypeDealClassKey;
    version: number;
}

export interface PartyProductTypeDealClassVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyProductTypeDealClassesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PartyProductTypeDealClassesFilter | null;
    as_of: string | null;
}

export interface ListPartyProductTypeDealClassesResponse {
    result: Result;
    party_product_type_deal_classes: PartyProductTypeDealClass[];
    total: number;
}

export interface GetPartyProductTypeDealClassRequest {
    key: PartyProductTypeDealClassKey;
}

export interface GetPartyProductTypeDealClassResponse {
    result: Result;
    party_product_type_deal_class: PartyProductTypeDealClass | null;
}

export interface GetManyPartyProductTypeDealClassesRequest {
    keys: PartyProductTypeDealClassKey[];
}

export interface GetManyPartyProductTypeDealClassesResponse {
    result: Result;
    entries: PartyProductTypeDealClassLookup[];
}

export interface PutPartyProductTypeDealClassRequest {
    change: PartyProductTypeDealClassChange;
    intent: ChangeIntent;
}

export interface PutPartyProductTypeDealClassResponse {
    result: Result;
    party_product_type_deal_class: PartyProductTypeDealClass | null;
}

export interface PutManyPartyProductTypeDealClassesRequest {
    changes: PartyProductTypeDealClassChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyProductTypeDealClassesResponse {
    result: Result;
    party_product_type_deal_classes: PartyProductTypeDealClass[];
}

export interface DeletePartyProductTypeDealClassRequest {
    removal: PartyProductTypeDealClassRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyProductTypeDealClassResponse {
    result: Result;
}

export interface DeleteManyPartyProductTypeDealClassesRequest {
    removals: PartyProductTypeDealClassRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyProductTypeDealClassesResponse {
    result: Result;
}

export interface ListByPartyIdPartyProductTypeDealClassesRequest {
    party_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyProductTypeDealClassesFilter | null;
}

export interface ListByPartyIdPartyProductTypeDealClassesResponse {
    result: Result;
    party_product_type_deal_classes: PartyProductTypeDealClass[];
    total: number;
}

export interface ListPartyProductTypeDealClassVersionsRequest {
    key: PartyProductTypeDealClassKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyProductTypeDealClassVersionsFilter | null;
}

export interface ListPartyProductTypeDealClassVersionsResponse {
    result: Result;
    versions: PartyProductTypeDealClass[];
    total: number;
}

export interface GetPartyProductTypeDealClassVersionRequest {
    key: PartyProductTypeDealClassVersionKey;
}

export interface GetPartyProductTypeDealClassVersionResponse {
    result: Result;
    version: PartyProductTypeDealClass | null;
}

export const subjects = {
    list_party_product_type_deal_classes_request: 'refdata.v1.party_product_type_deal_classes.list',
    get_party_product_type_deal_class_request: 'refdata.v1.party_product_type_deal_classes.get',
    get_many_party_product_type_deal_classes_request:
        'refdata.v1.party_product_type_deal_classes.get_many',
    put_party_product_type_deal_class_request: 'refdata.v1.party_product_type_deal_classes.put',
    put_many_party_product_type_deal_classes_request:
        'refdata.v1.party_product_type_deal_classes.put_many',
    delete_party_product_type_deal_class_request:
        'refdata.v1.party_product_type_deal_classes.delete',
    delete_many_party_product_type_deal_classes_request:
        'refdata.v1.party_product_type_deal_classes.delete_many',
    list_by_party_id_party_product_type_deal_classes_request:
        'refdata.v1.party_product_type_deal_classes.list_by_party_id',
    list_party_product_type_deal_class_versions_request:
        'refdata.v1.party_product_type_deal_classes_versions.list',
    get_party_product_type_deal_class_version_request:
        'refdata.v1.party_product_type_deal_classes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_product_type_deal_classes_request: true,
    get_party_product_type_deal_class_request: true,
    get_many_party_product_type_deal_classes_request: true,
    put_party_product_type_deal_class_request: true,
    put_many_party_product_type_deal_classes_request: true,
    delete_party_product_type_deal_class_request: true,
    delete_many_party_product_type_deal_classes_request: true,
    list_by_party_id_party_product_type_deal_classes_request: true,
    list_party_product_type_deal_class_versions_request: true,
    get_party_product_type_deal_class_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.party_product_type_deal_classes.created',
    updated: 'refdata.v1.events.party_product_type_deal_classes.updated',
    deleted: 'refdata.v1.events.party_product_type_deal_classes.deleted',
} as const;

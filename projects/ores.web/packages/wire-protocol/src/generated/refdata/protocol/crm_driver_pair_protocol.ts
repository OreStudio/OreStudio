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
import type { CrmDriverPair } from '../domain/crm_driver_pair.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CrmDriverPairKey {
    id: string;
}

export interface CrmDriverPairWrite {
    id: string;
    config_id: string;
    base_currency_code: string;
    quote_currency_code: string;
    enabled: boolean;
}

export interface CrmDriverPairChange {
    write: CrmDriverPairWrite;
    precondition: Precondition;
}

export interface CrmDriverPairRemoval {
    key: CrmDriverPairKey;
    precondition: Precondition;
}

export interface CrmDriverPairLookup {
    key: CrmDriverPairKey;
    crm_driver_pair: CrmDriverPair | null;
}

export interface CrmDriverPairEvent {
    event_id: string;
    key: CrmDriverPairKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrmDriverPairVersionKey {
    crm_driver_pair: CrmDriverPairKey;
    version: number;
}

export interface CrmDriverPairVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrmDriverPairsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCrmDriverPairsResponse {
    result: Result;
    crm_driver_pairs: CrmDriverPair[];
    total: number;
}

export interface GetCrmDriverPairRequest {
    key: CrmDriverPairKey;
}

export interface GetCrmDriverPairResponse {
    result: Result;
    crm_driver_pair: CrmDriverPair | null;
}

export interface GetManyCrmDriverPairsRequest {
    keys: CrmDriverPairKey[];
}

export interface GetManyCrmDriverPairsResponse {
    result: Result;
    entries: CrmDriverPairLookup[];
}

export interface PutCrmDriverPairRequest {
    change: CrmDriverPairChange;
    intent: ChangeIntent;
}

export interface PutCrmDriverPairResponse {
    result: Result;
    crm_driver_pair: CrmDriverPair;
}

export interface PutManyCrmDriverPairsRequest {
    changes: CrmDriverPairChange[];
    intent: ChangeIntent;
}

export interface PutManyCrmDriverPairsResponse {
    result: Result;
    crm_driver_pairs: CrmDriverPair[];
}

export interface DeleteCrmDriverPairRequest {
    removal: CrmDriverPairRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrmDriverPairResponse {
    result: Result;
}

export interface DeleteManyCrmDriverPairsRequest {
    removals: CrmDriverPairRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrmDriverPairsResponse {
    result: Result;
}

export interface ListCrmDriverPairVersionsRequest {
    key: CrmDriverPairKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrmDriverPairVersionsFilter | null;
}

export interface ListCrmDriverPairVersionsResponse {
    result: Result;
    versions: CrmDriverPair[];
    total: number;
}

export interface GetCrmDriverPairVersionRequest {
    key: CrmDriverPairVersionKey;
}

export interface GetCrmDriverPairVersionResponse {
    result: Result;
    version: CrmDriverPair;
}

export const subjects = {
    list_crm_driver_pairs_request: "refdata.v1.crm_driver_pairs.list",
    get_crm_driver_pair_request: "refdata.v1.crm_driver_pairs.get",
    get_many_crm_driver_pairs_request: "refdata.v1.crm_driver_pairs.get_many",
    put_crm_driver_pair_request: "refdata.v1.crm_driver_pairs.put",
    put_many_crm_driver_pairs_request: "refdata.v1.crm_driver_pairs.put_many",
    delete_crm_driver_pair_request: "refdata.v1.crm_driver_pairs.delete",
    delete_many_crm_driver_pairs_request: "refdata.v1.crm_driver_pairs.delete_many",
    list_crm_driver_pair_versions_request: "refdata.v1.crm_driver_pairs_versions.list",
    get_crm_driver_pair_version_request: "refdata.v1.crm_driver_pairs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_crm_driver_pairs_request: true,
    get_crm_driver_pair_request: true,
    get_many_crm_driver_pairs_request: true,
    put_crm_driver_pair_request: true,
    put_many_crm_driver_pairs_request: true,
    delete_crm_driver_pair_request: true,
    delete_many_crm_driver_pairs_request: true,
    list_crm_driver_pair_versions_request: true,
    get_crm_driver_pair_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.crm_driver_pairs_events.created",
    updated: "refdata.v1.crm_driver_pairs_events.updated",
    deleted: "refdata.v1.crm_driver_pairs_events.deleted",
} as const;

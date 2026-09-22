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
import type { CrmEnabledDerivedPair } from '../domain/crm_enabled_derived_pair.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CrmEnabledDerivedPairKey {
    id: string;
}

export interface CrmEnabledDerivedPairWrite {
    id: string;
    config_id: string;
    base_currency_code: string;
    quote_currency_code: string;
    enabled: boolean;
}

export interface CrmEnabledDerivedPairChange {
    write: CrmEnabledDerivedPairWrite;
    precondition: Precondition;
}

export interface CrmEnabledDerivedPairRemoval {
    key: CrmEnabledDerivedPairKey;
    precondition: Precondition;
}

export interface CrmEnabledDerivedPairLookup {
    key: CrmEnabledDerivedPairKey;
    crm_enabled_derived_pair: CrmEnabledDerivedPair | null;
}

export interface CrmEnabledDerivedPairEvent {
    event_id: string;
    key: CrmEnabledDerivedPairKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrmEnabledDerivedPairVersionKey {
    crm_enabled_derived_pair: CrmEnabledDerivedPairKey;
    version: number;
}

export interface CrmEnabledDerivedPairVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrmEnabledDerivedPairsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCrmEnabledDerivedPairsResponse {
    result: Result;
    crm_enabled_derived_pairs: CrmEnabledDerivedPair[];
    total: number;
}

export interface GetCrmEnabledDerivedPairRequest {
    key: CrmEnabledDerivedPairKey;
}

export interface GetCrmEnabledDerivedPairResponse {
    result: Result;
    crm_enabled_derived_pair: CrmEnabledDerivedPair | null;
}

export interface GetManyCrmEnabledDerivedPairsRequest {
    keys: CrmEnabledDerivedPairKey[];
}

export interface GetManyCrmEnabledDerivedPairsResponse {
    result: Result;
    entries: CrmEnabledDerivedPairLookup[];
}

export interface PutCrmEnabledDerivedPairRequest {
    change: CrmEnabledDerivedPairChange;
    intent: ChangeIntent;
}

export interface PutCrmEnabledDerivedPairResponse {
    result: Result;
    crm_enabled_derived_pair: CrmEnabledDerivedPair;
}

export interface PutManyCrmEnabledDerivedPairsRequest {
    changes: CrmEnabledDerivedPairChange[];
    intent: ChangeIntent;
}

export interface PutManyCrmEnabledDerivedPairsResponse {
    result: Result;
    crm_enabled_derived_pairs: CrmEnabledDerivedPair[];
}

export interface DeleteCrmEnabledDerivedPairRequest {
    removal: CrmEnabledDerivedPairRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrmEnabledDerivedPairResponse {
    result: Result;
}

export interface DeleteManyCrmEnabledDerivedPairsRequest {
    removals: CrmEnabledDerivedPairRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrmEnabledDerivedPairsResponse {
    result: Result;
}

export interface ListCrmEnabledDerivedPairVersionsRequest {
    key: CrmEnabledDerivedPairKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrmEnabledDerivedPairVersionsFilter | null;
}

export interface ListCrmEnabledDerivedPairVersionsResponse {
    result: Result;
    versions: CrmEnabledDerivedPair[];
    total: number;
}

export interface GetCrmEnabledDerivedPairVersionRequest {
    key: CrmEnabledDerivedPairVersionKey;
}

export interface GetCrmEnabledDerivedPairVersionResponse {
    result: Result;
    version: CrmEnabledDerivedPair;
}

export const subjects = {
    list_crm_enabled_derived_pairs_request: "refdata.v1.crm_enabled_derived_pairs.list",
    get_crm_enabled_derived_pair_request: "refdata.v1.crm_enabled_derived_pairs.get",
    get_many_crm_enabled_derived_pairs_request: "refdata.v1.crm_enabled_derived_pairs.get_many",
    put_crm_enabled_derived_pair_request: "refdata.v1.crm_enabled_derived_pairs.put",
    put_many_crm_enabled_derived_pairs_request: "refdata.v1.crm_enabled_derived_pairs.put_many",
    delete_crm_enabled_derived_pair_request: "refdata.v1.crm_enabled_derived_pairs.delete",
    delete_many_crm_enabled_derived_pairs_request: "refdata.v1.crm_enabled_derived_pairs.delete_many",
    list_crm_enabled_derived_pair_versions_request: "refdata.v1.crm_enabled_derived_pairs_versions.list",
    get_crm_enabled_derived_pair_version_request: "refdata.v1.crm_enabled_derived_pairs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_crm_enabled_derived_pairs_request: true,
    get_crm_enabled_derived_pair_request: true,
    get_many_crm_enabled_derived_pairs_request: true,
    put_crm_enabled_derived_pair_request: true,
    put_many_crm_enabled_derived_pairs_request: true,
    delete_crm_enabled_derived_pair_request: true,
    delete_many_crm_enabled_derived_pairs_request: true,
    list_crm_enabled_derived_pair_versions_request: true,
    get_crm_enabled_derived_pair_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.crm_enabled_derived_pairs_events.created",
    updated: "refdata.v1.crm_enabled_derived_pairs_events.updated",
    deleted: "refdata.v1.crm_enabled_derived_pairs_events.deleted",
} as const;

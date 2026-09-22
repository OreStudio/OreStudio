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
import type { MonetaryNature } from '../domain/monetary_nature.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MonetaryNatureKey {
    code: string;
}

export interface MonetaryNatureWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface MonetaryNatureChange {
    write: MonetaryNatureWrite;
    precondition: Precondition;
}

export interface MonetaryNatureRemoval {
    key: MonetaryNatureKey;
    precondition: Precondition;
}

export interface MonetaryNatureLookup {
    key: MonetaryNatureKey;
    monetary_nature: MonetaryNature | null;
}

export interface MonetaryNatureEvent {
    event_id: string;
    key: MonetaryNatureKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface MonetaryNatureVersionKey {
    monetary_nature: MonetaryNatureKey;
    version: number;
}

export interface MonetaryNatureVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListMonetaryNaturesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListMonetaryNaturesResponse {
    result: Result;
    types: MonetaryNature[];
    total: number;
}

export interface GetMonetaryNatureRequest {
    key: MonetaryNatureKey;
}

export interface GetMonetaryNatureResponse {
    result: Result;
    monetary_nature: MonetaryNature | null;
}

export interface GetManyMonetaryNaturesRequest {
    keys: MonetaryNatureKey[];
}

export interface GetManyMonetaryNaturesResponse {
    result: Result;
    entries: MonetaryNatureLookup[];
}

export interface PutMonetaryNatureRequest {
    change: MonetaryNatureChange;
    intent: ChangeIntent;
}

export interface PutMonetaryNatureResponse {
    result: Result;
    monetary_nature: MonetaryNature;
}

export interface PutManyMonetaryNaturesRequest {
    changes: MonetaryNatureChange[];
    intent: ChangeIntent;
}

export interface PutManyMonetaryNaturesResponse {
    result: Result;
    types: MonetaryNature[];
}

export interface DeleteMonetaryNatureRequest {
    removal: MonetaryNatureRemoval;
    intent: ChangeIntent;
}

export interface DeleteMonetaryNatureResponse {
    result: Result;
}

export interface DeleteManyMonetaryNaturesRequest {
    removals: MonetaryNatureRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMonetaryNaturesResponse {
    result: Result;
}

export interface ListMonetaryNatureVersionsRequest {
    key: MonetaryNatureKey;
    offset: number;
    limit: number;
    order: Order;
    filter: MonetaryNatureVersionsFilter | null;
}

export interface ListMonetaryNatureVersionsResponse {
    result: Result;
    versions: MonetaryNature[];
    total: number;
}

export interface GetMonetaryNatureVersionRequest {
    key: MonetaryNatureVersionKey;
}

export interface GetMonetaryNatureVersionResponse {
    result: Result;
    version: MonetaryNature;
}

export const subjects = {
    list_monetary_natures_request: "refdata.v1.monetary_natures.list",
    get_monetary_nature_request: "refdata.v1.monetary_natures.get",
    get_many_monetary_natures_request: "refdata.v1.monetary_natures.get_many",
    put_monetary_nature_request: "refdata.v1.monetary_natures.put",
    put_many_monetary_natures_request: "refdata.v1.monetary_natures.put_many",
    delete_monetary_nature_request: "refdata.v1.monetary_natures.delete",
    delete_many_monetary_natures_request: "refdata.v1.monetary_natures.delete_many",
    list_monetary_nature_versions_request: "refdata.v1.monetary_natures_versions.list",
    get_monetary_nature_version_request: "refdata.v1.monetary_natures_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_monetary_natures_request: true,
    get_monetary_nature_request: true,
    get_many_monetary_natures_request: true,
    put_monetary_nature_request: true,
    put_many_monetary_natures_request: true,
    delete_monetary_nature_request: true,
    delete_many_monetary_natures_request: true,
    list_monetary_nature_versions_request: true,
    get_monetary_nature_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.monetary_natures_events.created",
    updated: "refdata.v1.monetary_natures_events.updated",
    deleted: "refdata.v1.monetary_natures_events.deleted",
} as const;

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
import type { BondYieldConvention } from '../domain/bond_yield_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondYieldConventionKey {
    id: string;
}

export interface BondYieldConventionWrite {
    id: string;
    compounding: string;
    frequency: string | null;
    price_type: string | null;
    accuracy: number | null;
    max_evaluations: number | null;
    guess: number | null;
}

export interface BondYieldConventionChange {
    write: BondYieldConventionWrite;
    precondition: Precondition;
}

export interface BondYieldConventionRemoval {
    key: BondYieldConventionKey;
    precondition: Precondition;
}

export interface BondYieldConventionLookup {
    key: BondYieldConventionKey;
    bond_yield_convention: BondYieldConvention | null;
}

export interface BondYieldConventionsFilter {
    id_one_of: string[] | null;
}

export interface BondYieldConventionEvent {
    event_id: string;
    key: BondYieldConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondYieldConventionVersionKey {
    bond_yield_convention: BondYieldConventionKey;
    version: number;
}

export interface BondYieldConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondYieldConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BondYieldConventionsFilter | null;
    as_of: string | null;
}

export interface ListBondYieldConventionsResponse {
    result: Result;
    bond_yield_conventions: BondYieldConvention[];
    total: number;
}

export interface GetBondYieldConventionRequest {
    key: BondYieldConventionKey;
}

export interface GetBondYieldConventionResponse {
    result: Result;
    bond_yield_convention: BondYieldConvention | null;
}

export interface GetManyBondYieldConventionsRequest {
    keys: BondYieldConventionKey[];
}

export interface GetManyBondYieldConventionsResponse {
    result: Result;
    entries: BondYieldConventionLookup[];
}

export interface PutBondYieldConventionRequest {
    change: BondYieldConventionChange;
    intent: ChangeIntent;
}

export interface PutBondYieldConventionResponse {
    result: Result;
    bond_yield_convention: BondYieldConvention | null;
}

export interface PutManyBondYieldConventionsRequest {
    changes: BondYieldConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyBondYieldConventionsResponse {
    result: Result;
    bond_yield_conventions: BondYieldConvention[];
}

export interface DeleteBondYieldConventionRequest {
    removal: BondYieldConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondYieldConventionResponse {
    result: Result;
}

export interface DeleteManyBondYieldConventionsRequest {
    removals: BondYieldConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondYieldConventionsResponse {
    result: Result;
}

export interface ListBondYieldConventionVersionsRequest {
    key: BondYieldConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondYieldConventionVersionsFilter | null;
}

export interface ListBondYieldConventionVersionsResponse {
    result: Result;
    versions: BondYieldConvention[];
    total: number;
}

export interface GetBondYieldConventionVersionRequest {
    key: BondYieldConventionVersionKey;
}

export interface GetBondYieldConventionVersionResponse {
    result: Result;
    version: BondYieldConvention | null;
}

export const subjects = {
    list_bond_yield_conventions_request: 'refdata.v1.bond_yield_conventions.list',
    get_bond_yield_convention_request: 'refdata.v1.bond_yield_conventions.get',
    get_many_bond_yield_conventions_request: 'refdata.v1.bond_yield_conventions.get_many',
    put_bond_yield_convention_request: 'refdata.v1.bond_yield_conventions.put',
    put_many_bond_yield_conventions_request: 'refdata.v1.bond_yield_conventions.put_many',
    delete_bond_yield_convention_request: 'refdata.v1.bond_yield_conventions.delete',
    delete_many_bond_yield_conventions_request: 'refdata.v1.bond_yield_conventions.delete_many',
    list_bond_yield_convention_versions_request: 'refdata.v1.bond_yield_conventions_versions.list',
    get_bond_yield_convention_version_request: 'refdata.v1.bond_yield_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_yield_conventions_request: true,
    get_bond_yield_convention_request: true,
    get_many_bond_yield_conventions_request: true,
    put_bond_yield_convention_request: true,
    put_many_bond_yield_conventions_request: true,
    delete_bond_yield_convention_request: true,
    delete_many_bond_yield_conventions_request: true,
    list_bond_yield_convention_versions_request: true,
    get_bond_yield_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.bond_yield_conventions_events.created',
    updated: 'refdata.v1.bond_yield_conventions_events.updated',
    deleted: 'refdata.v1.bond_yield_conventions_events.deleted',
} as const;

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
import type { BondOption } from '../domain/bond_option.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondOptionKey {
    trade_id: string;
}

export interface BondOptionWrite {
    trade_id: string;
    option_type: string;
    option_strike: string;
    redemption: string | null;
    price_type: string | null;
    knocks_out: string | null;
}

export interface BondOptionChange {
    write: BondOptionWrite;
    precondition: Precondition;
}

export interface BondOptionRemoval {
    key: BondOptionKey;
    precondition: Precondition;
}

export interface BondOptionLookup {
    key: BondOptionKey;
    bond_option: BondOption | null;
}

export interface BondOptionsFilter {
    trade_id_one_of: string[] | null;
}

export interface BondOptionEvent {
    event_id: string;
    key: BondOptionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondOptionVersionKey {
    bond_option: BondOptionKey;
    version: number;
}

export interface BondOptionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondOptionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BondOptionsFilter | null;
}

export interface ListBondOptionsResponse {
    result: Result;
    options: BondOption[];
    total: number;
}

export interface GetBondOptionRequest {
    key: BondOptionKey;
}

export interface GetBondOptionResponse {
    result: Result;
    bond_option: BondOption | null;
}

export interface GetManyBondOptionsRequest {
    keys: BondOptionKey[];
}

export interface GetManyBondOptionsResponse {
    result: Result;
    entries: BondOptionLookup[];
}

export interface PutBondOptionRequest {
    change: BondOptionChange;
    intent: ChangeIntent;
}

export interface PutBondOptionResponse {
    result: Result;
    bond_option: BondOption | null;
}

export interface PutManyBondOptionsRequest {
    changes: BondOptionChange[];
    intent: ChangeIntent;
}

export interface PutManyBondOptionsResponse {
    result: Result;
    options: BondOption[];
}

export interface DeleteBondOptionRequest {
    removal: BondOptionRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondOptionResponse {
    result: Result;
}

export interface DeleteManyBondOptionsRequest {
    removals: BondOptionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondOptionsResponse {
    result: Result;
}

export interface ListBondOptionVersionsRequest {
    key: BondOptionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondOptionVersionsFilter | null;
}

export interface ListBondOptionVersionsResponse {
    result: Result;
    versions: BondOption[];
    total: number;
}

export interface GetBondOptionVersionRequest {
    key: BondOptionVersionKey;
}

export interface GetBondOptionVersionResponse {
    result: Result;
    version: BondOption | null;
}

export const subjects = {
    list_bond_options_request: 'trading.v1.bond_options.list',
    get_bond_option_request: 'trading.v1.bond_options.get',
    get_many_bond_options_request: 'trading.v1.bond_options.get_many',
    put_bond_option_request: 'trading.v1.bond_options.put',
    put_many_bond_options_request: 'trading.v1.bond_options.put_many',
    delete_bond_option_request: 'trading.v1.bond_options.delete',
    delete_many_bond_options_request: 'trading.v1.bond_options.delete_many',
    list_bond_option_versions_request: 'trading.v1.bond_options_versions.list',
    get_bond_option_version_request: 'trading.v1.bond_options_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_options_request: true,
    get_bond_option_request: true,
    get_many_bond_options_request: true,
    put_bond_option_request: true,
    put_many_bond_options_request: true,
    delete_bond_option_request: true,
    delete_many_bond_options_request: true,
    list_bond_option_versions_request: true,
    get_bond_option_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_options_events.created',
    updated: 'trading.v1.bond_options_events.updated',
    deleted: 'trading.v1.bond_options_events.deleted',
} as const;

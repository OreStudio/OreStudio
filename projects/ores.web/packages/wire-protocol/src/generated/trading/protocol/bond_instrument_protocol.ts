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
import type { BondInstrument } from '../domain/bond_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondInstrumentKey {
    trade_id: string;
}

export interface BondInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    issue_id: string;
    notional: string | null;
}

export interface BondInstrumentChange {
    write: BondInstrumentWrite;
    precondition: Precondition;
}

export interface BondInstrumentRemoval {
    key: BondInstrumentKey;
    precondition: Precondition;
}

export interface BondInstrumentLookup {
    key: BondInstrumentKey;
    bond_instrument: BondInstrument | null;
}

export interface BondInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface BondInstrumentEvent {
    event_id: string;
    key: BondInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondInstrumentVersionKey {
    bond_instrument: BondInstrumentKey;
    version: number;
}

export interface BondInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BondInstrumentsFilter | null;
}

export interface ListBondInstrumentsResponse {
    result: Result;
    bond_instruments: BondInstrument[];
    total: number;
}

export interface GetBondInstrumentRequest {
    key: BondInstrumentKey;
}

export interface GetBondInstrumentResponse {
    result: Result;
    bond_instrument: BondInstrument | null;
}

export interface GetManyBondInstrumentsRequest {
    keys: BondInstrumentKey[];
}

export interface GetManyBondInstrumentsResponse {
    result: Result;
    entries: BondInstrumentLookup[];
}

export interface PutBondInstrumentRequest {
    change: BondInstrumentChange;
    intent: ChangeIntent;
}

export interface PutBondInstrumentResponse {
    result: Result;
    bond_instrument: BondInstrument | null;
}

export interface PutManyBondInstrumentsRequest {
    changes: BondInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyBondInstrumentsResponse {
    result: Result;
    bond_instruments: BondInstrument[];
}

export interface DeleteBondInstrumentRequest {
    removal: BondInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondInstrumentResponse {
    result: Result;
}

export interface DeleteManyBondInstrumentsRequest {
    removals: BondInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondInstrumentsResponse {
    result: Result;
}

export interface ListBondInstrumentVersionsRequest {
    key: BondInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondInstrumentVersionsFilter | null;
}

export interface ListBondInstrumentVersionsResponse {
    result: Result;
    versions: BondInstrument[];
    total: number;
}

export interface GetBondInstrumentVersionRequest {
    key: BondInstrumentVersionKey;
}

export interface GetBondInstrumentVersionResponse {
    result: Result;
    version: BondInstrument | null;
}

export const subjects = {
    list_bond_instruments_request: 'trading.v1.bond_instruments.list',
    get_bond_instrument_request: 'trading.v1.bond_instruments.get',
    get_many_bond_instruments_request: 'trading.v1.bond_instruments.get_many',
    put_bond_instrument_request: 'trading.v1.bond_instruments.put',
    put_many_bond_instruments_request: 'trading.v1.bond_instruments.put_many',
    delete_bond_instrument_request: 'trading.v1.bond_instruments.delete',
    delete_many_bond_instruments_request: 'trading.v1.bond_instruments.delete_many',
    list_bond_instrument_versions_request: 'trading.v1.bond_instruments_versions.list',
    get_bond_instrument_version_request: 'trading.v1.bond_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_instruments_request: true,
    get_bond_instrument_request: true,
    get_many_bond_instruments_request: true,
    put_bond_instrument_request: true,
    put_many_bond_instruments_request: true,
    delete_bond_instrument_request: true,
    delete_many_bond_instruments_request: true,
    list_bond_instrument_versions_request: true,
    get_bond_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_instruments_events.created',
    updated: 'trading.v1.bond_instruments_events.updated',
    deleted: 'trading.v1.bond_instruments_events.deleted',
} as const;

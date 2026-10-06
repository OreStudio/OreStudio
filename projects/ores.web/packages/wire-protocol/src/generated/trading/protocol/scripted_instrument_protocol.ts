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
import type { ScriptedInstrument } from '../domain/scripted_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptedInstrumentKey {
    trade_id: string;
}

export interface ScriptedInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    script_name: string;
    script_body: string;
    events_json: string;
    underlyings_json: string;
    parameters_json: string;
    description: string;
}

export interface ScriptedInstrumentChange {
    write: ScriptedInstrumentWrite;
    precondition: Precondition;
}

export interface ScriptedInstrumentRemoval {
    key: ScriptedInstrumentKey;
    precondition: Precondition;
}

export interface ScriptedInstrumentLookup {
    key: ScriptedInstrumentKey;
    scripted_instrument: ScriptedInstrument | null;
}

export interface ScriptedInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface ScriptedInstrumentEvent {
    event_id: string;
    key: ScriptedInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptedInstrumentVersionKey {
    scripted_instrument: ScriptedInstrumentKey;
    version: number;
}

export interface ScriptedInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptedInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptedInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListScriptedInstrumentsResponse {
    result: Result;
    scripted_instruments: ScriptedInstrument[];
    total: number;
}

export interface GetScriptedInstrumentRequest {
    key: ScriptedInstrumentKey;
}

export interface GetScriptedInstrumentResponse {
    result: Result;
    scripted_instrument: ScriptedInstrument | null;
}

export interface GetManyScriptedInstrumentsRequest {
    keys: ScriptedInstrumentKey[];
}

export interface GetManyScriptedInstrumentsResponse {
    result: Result;
    entries: ScriptedInstrumentLookup[];
}

export interface PutScriptedInstrumentRequest {
    change: ScriptedInstrumentChange;
    intent: ChangeIntent;
}

export interface PutScriptedInstrumentResponse {
    result: Result;
    scripted_instrument: ScriptedInstrument | null;
}

export interface PutManyScriptedInstrumentsRequest {
    changes: ScriptedInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptedInstrumentsResponse {
    result: Result;
    scripted_instruments: ScriptedInstrument[];
}

export interface DeleteScriptedInstrumentRequest {
    removal: ScriptedInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptedInstrumentResponse {
    result: Result;
}

export interface DeleteManyScriptedInstrumentsRequest {
    removals: ScriptedInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptedInstrumentsResponse {
    result: Result;
}

export interface ListScriptedInstrumentVersionsRequest {
    key: ScriptedInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptedInstrumentVersionsFilter | null;
}

export interface ListScriptedInstrumentVersionsResponse {
    result: Result;
    versions: ScriptedInstrument[];
    total: number;
}

export interface GetScriptedInstrumentVersionRequest {
    key: ScriptedInstrumentVersionKey;
}

export interface GetScriptedInstrumentVersionResponse {
    result: Result;
    version: ScriptedInstrument | null;
}

export const subjects = {
    list_scripted_instruments_request: 'trading.v1.scripted_instruments.list',
    get_scripted_instrument_request: 'trading.v1.scripted_instruments.get',
    get_many_scripted_instruments_request: 'trading.v1.scripted_instruments.get_many',
    put_scripted_instrument_request: 'trading.v1.scripted_instruments.put',
    put_many_scripted_instruments_request: 'trading.v1.scripted_instruments.put_many',
    delete_scripted_instrument_request: 'trading.v1.scripted_instruments.delete',
    delete_many_scripted_instruments_request: 'trading.v1.scripted_instruments.delete_many',
    list_scripted_instrument_versions_request: 'trading.v1.scripted_instruments_versions.list',
    get_scripted_instrument_version_request: 'trading.v1.scripted_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_scripted_instruments_request: true,
    get_scripted_instrument_request: true,
    get_many_scripted_instruments_request: true,
    put_scripted_instrument_request: true,
    put_many_scripted_instruments_request: true,
    delete_scripted_instrument_request: true,
    delete_many_scripted_instruments_request: true,
    list_scripted_instrument_versions_request: true,
    get_scripted_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.scripted_instruments_events.created',
    updated: 'trading.v1.scripted_instruments_events.updated',
    deleted: 'trading.v1.scripted_instruments_events.deleted',
} as const;

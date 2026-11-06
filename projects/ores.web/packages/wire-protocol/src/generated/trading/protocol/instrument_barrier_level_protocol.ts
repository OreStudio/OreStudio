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
import type { InstrumentBarrierLevel } from '../domain/instrument_barrier_level.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface InstrumentBarrierLevelKey {
    trade_id: string;
    barrier_role: string;
    barrier_sequence_number: number;
    sequence_number: number;
}

export interface InstrumentBarrierLevelWrite {
    trade_id: string;
    barrier_role: string;
    barrier_sequence_number: number;
    sequence_number: number;
    trade_activity_id: string;
    level: string;
}

export interface InstrumentBarrierLevelChange {
    write: InstrumentBarrierLevelWrite;
    precondition: Precondition;
}

export interface InstrumentBarrierLevelRemoval {
    key: InstrumentBarrierLevelKey;
    precondition: Precondition;
}

export interface InstrumentBarrierLevelLookup {
    key: InstrumentBarrierLevelKey;
    instrument_barrier_level: InstrumentBarrierLevel | null;
}

export interface InstrumentBarrierLevelsFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface InstrumentBarrierLevelEvent {
    event_id: string;
    key: InstrumentBarrierLevelKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentBarrierLevelVersionKey {
    instrument_barrier_level: InstrumentBarrierLevelKey;
    version: number;
}

export interface InstrumentBarrierLevelVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentBarrierLevelsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentBarrierLevelsFilter | null;
    as_of: string | null;
}

export interface ListInstrumentBarrierLevelsResponse {
    result: Result;
    instrument_barrier_levels: InstrumentBarrierLevel[];
    total: number;
}

export interface GetInstrumentBarrierLevelRequest {
    key: InstrumentBarrierLevelKey;
}

export interface GetInstrumentBarrierLevelResponse {
    result: Result;
    instrument_barrier_level: InstrumentBarrierLevel | null;
}

export interface GetManyInstrumentBarrierLevelsRequest {
    keys: InstrumentBarrierLevelKey[];
}

export interface GetManyInstrumentBarrierLevelsResponse {
    result: Result;
    entries: InstrumentBarrierLevelLookup[];
}

export interface PutInstrumentBarrierLevelRequest {
    change: InstrumentBarrierLevelChange;
    intent: ChangeIntent;
}

export interface PutInstrumentBarrierLevelResponse {
    result: Result;
    instrument_barrier_level: InstrumentBarrierLevel | null;
}

export interface PutManyInstrumentBarrierLevelsRequest {
    changes: InstrumentBarrierLevelChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentBarrierLevelsResponse {
    result: Result;
    instrument_barrier_levels: InstrumentBarrierLevel[];
}

export interface DeleteInstrumentBarrierLevelRequest {
    removal: InstrumentBarrierLevelRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentBarrierLevelResponse {
    result: Result;
}

export interface DeleteManyInstrumentBarrierLevelsRequest {
    removals: InstrumentBarrierLevelRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentBarrierLevelsResponse {
    result: Result;
}

export interface ListByTradeIdInstrumentBarrierLevelsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentBarrierLevelsFilter | null;
}

export interface ListByTradeIdInstrumentBarrierLevelsResponse {
    result: Result;
    instrument_barrier_levels: InstrumentBarrierLevel[];
    total: number;
}

export interface ListInstrumentBarrierLevelVersionsRequest {
    key: InstrumentBarrierLevelKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentBarrierLevelVersionsFilter | null;
}

export interface ListInstrumentBarrierLevelVersionsResponse {
    result: Result;
    versions: InstrumentBarrierLevel[];
    total: number;
}

export interface GetInstrumentBarrierLevelVersionRequest {
    key: InstrumentBarrierLevelVersionKey;
}

export interface GetInstrumentBarrierLevelVersionResponse {
    result: Result;
    version: InstrumentBarrierLevel | null;
}

export const subjects = {
    list_instrument_barrier_levels_request: 'trading.v1.instrument_barrier_levels.list',
    get_instrument_barrier_level_request: 'trading.v1.instrument_barrier_levels.get',
    get_many_instrument_barrier_levels_request: 'trading.v1.instrument_barrier_levels.get_many',
    put_instrument_barrier_level_request: 'trading.v1.instrument_barrier_levels.put',
    put_many_instrument_barrier_levels_request: 'trading.v1.instrument_barrier_levels.put_many',
    delete_instrument_barrier_level_request: 'trading.v1.instrument_barrier_levels.delete',
    delete_many_instrument_barrier_levels_request:
        'trading.v1.instrument_barrier_levels.delete_many',
    list_by_trade_id_instrument_barrier_levels_request:
        'trading.v1.instrument_barrier_levels.list_by_trade_id',
    list_instrument_barrier_level_versions_request:
        'trading.v1.instrument_barrier_levels_versions.list',
    get_instrument_barrier_level_version_request:
        'trading.v1.instrument_barrier_levels_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_barrier_levels_request: true,
    get_instrument_barrier_level_request: true,
    get_many_instrument_barrier_levels_request: true,
    put_instrument_barrier_level_request: true,
    put_many_instrument_barrier_levels_request: true,
    delete_instrument_barrier_level_request: true,
    delete_many_instrument_barrier_levels_request: true,
    list_by_trade_id_instrument_barrier_levels_request: true,
    list_instrument_barrier_level_versions_request: true,
    get_instrument_barrier_level_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.instrument_barrier_levels.created',
    updated: 'trading.v1.events.instrument_barrier_levels.updated',
    deleted: 'trading.v1.events.instrument_barrier_levels.deleted',
} as const;

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
import type { InstrumentBarrier } from '../domain/instrument_barrier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface InstrumentBarrierKey {
    trade_id: string;
    barrier_role: string;
    sequence_number: number;
}

export interface InstrumentBarrierWrite {
    trade_id: string;
    barrier_role: string;
    sequence_number: number;
    trade_activity_id: string;
    barrier_type: string;
    style: string | null;
    strict_comparison: string | null;
    rebate: string | null;
    rebate_currency: string | null;
    rebate_pay_time: string | null;
    override_triggered: string | null;
}

export interface InstrumentBarrierChange {
    write: InstrumentBarrierWrite;
    precondition: Precondition;
}

export interface InstrumentBarrierRemoval {
    key: InstrumentBarrierKey;
    precondition: Precondition;
}

export interface InstrumentBarrierLookup {
    key: InstrumentBarrierKey;
    instrument_barrier: InstrumentBarrier | null;
}

export interface InstrumentBarriersFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface InstrumentBarrierEvent {
    event_id: string;
    key: InstrumentBarrierKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentBarrierVersionKey {
    instrument_barrier: InstrumentBarrierKey;
    version: number;
}

export interface InstrumentBarrierVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentBarriersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentBarriersFilter | null;
    as_of: string | null;
}

export interface ListInstrumentBarriersResponse {
    result: Result;
    instrument_barriers: InstrumentBarrier[];
    total: number;
}

export interface GetInstrumentBarrierRequest {
    key: InstrumentBarrierKey;
}

export interface GetInstrumentBarrierResponse {
    result: Result;
    instrument_barrier: InstrumentBarrier | null;
}

export interface GetManyInstrumentBarriersRequest {
    keys: InstrumentBarrierKey[];
}

export interface GetManyInstrumentBarriersResponse {
    result: Result;
    entries: InstrumentBarrierLookup[];
}

export interface PutInstrumentBarrierRequest {
    change: InstrumentBarrierChange;
    intent: ChangeIntent;
}

export interface PutInstrumentBarrierResponse {
    result: Result;
    instrument_barrier: InstrumentBarrier | null;
}

export interface PutManyInstrumentBarriersRequest {
    changes: InstrumentBarrierChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentBarriersResponse {
    result: Result;
    instrument_barriers: InstrumentBarrier[];
}

export interface DeleteInstrumentBarrierRequest {
    removal: InstrumentBarrierRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentBarrierResponse {
    result: Result;
}

export interface DeleteManyInstrumentBarriersRequest {
    removals: InstrumentBarrierRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentBarriersResponse {
    result: Result;
}

export interface ListByTradeIdInstrumentBarriersRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentBarriersFilter | null;
}

export interface ListByTradeIdInstrumentBarriersResponse {
    result: Result;
    instrument_barriers: InstrumentBarrier[];
    total: number;
}

export interface ListInstrumentBarrierVersionsRequest {
    key: InstrumentBarrierKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentBarrierVersionsFilter | null;
}

export interface ListInstrumentBarrierVersionsResponse {
    result: Result;
    versions: InstrumentBarrier[];
    total: number;
}

export interface GetInstrumentBarrierVersionRequest {
    key: InstrumentBarrierVersionKey;
}

export interface GetInstrumentBarrierVersionResponse {
    result: Result;
    version: InstrumentBarrier | null;
}

export const subjects = {
    list_instrument_barriers_request: 'trading.v1.instrument_barriers.list',
    get_instrument_barrier_request: 'trading.v1.instrument_barriers.get',
    get_many_instrument_barriers_request: 'trading.v1.instrument_barriers.get_many',
    put_instrument_barrier_request: 'trading.v1.instrument_barriers.put',
    put_many_instrument_barriers_request: 'trading.v1.instrument_barriers.put_many',
    delete_instrument_barrier_request: 'trading.v1.instrument_barriers.delete',
    delete_many_instrument_barriers_request: 'trading.v1.instrument_barriers.delete_many',
    list_by_trade_id_instrument_barriers_request: 'trading.v1.instrument_barriers.list_by_trade_id',
    list_instrument_barrier_versions_request: 'trading.v1.instrument_barriers_versions.list',
    get_instrument_barrier_version_request: 'trading.v1.instrument_barriers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_barriers_request: true,
    get_instrument_barrier_request: true,
    get_many_instrument_barriers_request: true,
    put_instrument_barrier_request: true,
    put_many_instrument_barriers_request: true,
    delete_instrument_barrier_request: true,
    delete_many_instrument_barriers_request: true,
    list_by_trade_id_instrument_barriers_request: true,
    list_instrument_barrier_versions_request: true,
    get_instrument_barrier_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.instrument_barriers.created',
    updated: 'trading.v1.events.instrument_barriers.updated',
    deleted: 'trading.v1.events.instrument_barriers.deleted',
} as const;

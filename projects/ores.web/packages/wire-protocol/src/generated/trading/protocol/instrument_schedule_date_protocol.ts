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
import type { InstrumentScheduleDate } from '../domain/instrument_schedule_date.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentScheduleDateKey {
    trade_id: string;
    owner_role: string;
    owner_number: number;
    schedule_role: string;
    schedule_sequence_number: number;
    sequence_number: number;
}

export interface InstrumentScheduleDateWrite {
    trade_id: string;
    owner_role: string;
    owner_number: number;
    schedule_role: string;
    schedule_sequence_number: number;
    sequence_number: number;
    schedule_date: string;
}

export interface InstrumentScheduleDateChange {
    write: InstrumentScheduleDateWrite;
    precondition: Precondition;
}

export interface InstrumentScheduleDateRemoval {
    key: InstrumentScheduleDateKey;
    precondition: Precondition;
}

export interface InstrumentScheduleDateLookup {
    key: InstrumentScheduleDateKey;
    instrument_schedule_date: InstrumentScheduleDate | null;
}

export interface InstrumentScheduleDateEvent {
    event_id: string;
    key: InstrumentScheduleDateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentScheduleDateVersionKey {
    instrument_schedule_date: InstrumentScheduleDateKey;
    version: number;
}

export interface InstrumentScheduleDateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentScheduleDatesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListInstrumentScheduleDatesResponse {
    result: Result;
    instrument_schedule_dates: InstrumentScheduleDate[];
    total: number;
}

export interface GetInstrumentScheduleDateRequest {
    key: InstrumentScheduleDateKey;
}

export interface GetInstrumentScheduleDateResponse {
    result: Result;
    instrument_schedule_date: InstrumentScheduleDate | null;
}

export interface GetManyInstrumentScheduleDatesRequest {
    keys: InstrumentScheduleDateKey[];
}

export interface GetManyInstrumentScheduleDatesResponse {
    result: Result;
    entries: InstrumentScheduleDateLookup[];
}

export interface PutInstrumentScheduleDateRequest {
    change: InstrumentScheduleDateChange;
    intent: ChangeIntent;
}

export interface PutInstrumentScheduleDateResponse {
    result: Result;
    instrument_schedule_date: InstrumentScheduleDate | null;
}

export interface PutManyInstrumentScheduleDatesRequest {
    changes: InstrumentScheduleDateChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentScheduleDatesResponse {
    result: Result;
    instrument_schedule_dates: InstrumentScheduleDate[];
}

export interface DeleteInstrumentScheduleDateRequest {
    removal: InstrumentScheduleDateRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentScheduleDateResponse {
    result: Result;
}

export interface DeleteManyInstrumentScheduleDatesRequest {
    removals: InstrumentScheduleDateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentScheduleDatesResponse {
    result: Result;
}

export interface ListInstrumentScheduleDateVersionsRequest {
    key: InstrumentScheduleDateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentScheduleDateVersionsFilter | null;
}

export interface ListInstrumentScheduleDateVersionsResponse {
    result: Result;
    versions: InstrumentScheduleDate[];
    total: number;
}

export interface GetInstrumentScheduleDateVersionRequest {
    key: InstrumentScheduleDateVersionKey;
}

export interface GetInstrumentScheduleDateVersionResponse {
    result: Result;
    version: InstrumentScheduleDate | null;
}

export const subjects = {
    list_instrument_schedule_dates_request: 'trading.v1.instrument_schedule_dates.list',
    get_instrument_schedule_date_request: 'trading.v1.instrument_schedule_dates.get',
    get_many_instrument_schedule_dates_request: 'trading.v1.instrument_schedule_dates.get_many',
    put_instrument_schedule_date_request: 'trading.v1.instrument_schedule_dates.put',
    put_many_instrument_schedule_dates_request: 'trading.v1.instrument_schedule_dates.put_many',
    delete_instrument_schedule_date_request: 'trading.v1.instrument_schedule_dates.delete',
    delete_many_instrument_schedule_dates_request:
        'trading.v1.instrument_schedule_dates.delete_many',
    list_instrument_schedule_date_versions_request:
        'trading.v1.instrument_schedule_dates_versions.list',
    get_instrument_schedule_date_version_request:
        'trading.v1.instrument_schedule_dates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_schedule_dates_request: true,
    get_instrument_schedule_date_request: true,
    get_many_instrument_schedule_dates_request: true,
    put_instrument_schedule_date_request: true,
    put_many_instrument_schedule_dates_request: true,
    delete_instrument_schedule_date_request: true,
    delete_many_instrument_schedule_dates_request: true,
    list_instrument_schedule_date_versions_request: true,
    get_instrument_schedule_date_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_schedule_dates_events.created',
    updated: 'trading.v1.instrument_schedule_dates_events.updated',
    deleted: 'trading.v1.instrument_schedule_dates_events.deleted',
} as const;

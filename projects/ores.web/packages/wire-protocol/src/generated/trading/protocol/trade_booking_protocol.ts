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
import type { TradeBooking } from '../domain/trade_booking.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeBookingKey {
    trade_id: string;
}

export interface TradeBookingWrite {
    trade_id: string;
    trade_activity_id: string;
    book_id: string;
    netting_set_id: string | null;
    counterparty_identifier_id: string | null;
    netting_set_identifier_id: string | null;
    trade_date: string | null;
    execution_timestamp: string | null;
}

export interface TradeBookingChange {
    write: TradeBookingWrite;
    precondition: Precondition;
}

export interface TradeBookingRemoval {
    key: TradeBookingKey;
    precondition: Precondition;
}

export interface TradeBookingLookup {
    key: TradeBookingKey;
    trade_booking: TradeBooking | null;
}

export interface TradeBookingsFilter {
    trade_id_one_of: string[] | null;
}

export interface TradeBookingEvent {
    event_id: string;
    key: TradeBookingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeBookingVersionKey {
    trade_booking: TradeBookingKey;
    version: number;
}

export interface TradeBookingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeBookingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeBookingsFilter | null;
    as_of: string | null;
}

export interface ListTradeBookingsResponse {
    result: Result;
    trade_bookings: TradeBooking[];
    total: number;
}

export interface GetTradeBookingRequest {
    key: TradeBookingKey;
}

export interface GetTradeBookingResponse {
    result: Result;
    trade_booking: TradeBooking | null;
}

export interface GetManyTradeBookingsRequest {
    keys: TradeBookingKey[];
}

export interface GetManyTradeBookingsResponse {
    result: Result;
    entries: TradeBookingLookup[];
}

export interface PutTradeBookingRequest {
    change: TradeBookingChange;
    intent: ChangeIntent;
}

export interface PutTradeBookingResponse {
    result: Result;
    trade_booking: TradeBooking | null;
}

export interface PutManyTradeBookingsRequest {
    changes: TradeBookingChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeBookingsResponse {
    result: Result;
    trade_bookings: TradeBooking[];
}

export interface DeleteTradeBookingRequest {
    removal: TradeBookingRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeBookingResponse {
    result: Result;
}

export interface DeleteManyTradeBookingsRequest {
    removals: TradeBookingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeBookingsResponse {
    result: Result;
}

export interface ListTradeBookingVersionsRequest {
    key: TradeBookingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeBookingVersionsFilter | null;
}

export interface ListTradeBookingVersionsResponse {
    result: Result;
    versions: TradeBooking[];
    total: number;
}

export interface GetTradeBookingVersionRequest {
    key: TradeBookingVersionKey;
}

export interface GetTradeBookingVersionResponse {
    result: Result;
    version: TradeBooking | null;
}

export const subjects = {
    list_trade_bookings_request: 'trading.v1.trade_bookings.list',
    get_trade_booking_request: 'trading.v1.trade_bookings.get',
    get_many_trade_bookings_request: 'trading.v1.trade_bookings.get_many',
    put_trade_booking_request: 'trading.v1.trade_bookings.put',
    put_many_trade_bookings_request: 'trading.v1.trade_bookings.put_many',
    delete_trade_booking_request: 'trading.v1.trade_bookings.delete',
    delete_many_trade_bookings_request: 'trading.v1.trade_bookings.delete_many',
    list_trade_booking_versions_request: 'trading.v1.trade_bookings_versions.list',
    get_trade_booking_version_request: 'trading.v1.trade_bookings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_bookings_request: true,
    get_trade_booking_request: true,
    get_many_trade_bookings_request: true,
    put_trade_booking_request: true,
    put_many_trade_bookings_request: true,
    delete_trade_booking_request: true,
    delete_many_trade_bookings_request: true,
    list_trade_booking_versions_request: true,
    get_trade_booking_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_bookings_events.created',
    updated: 'trading.v1.trade_bookings_events.updated',
    deleted: 'trading.v1.trade_bookings_events.deleted',
} as const;

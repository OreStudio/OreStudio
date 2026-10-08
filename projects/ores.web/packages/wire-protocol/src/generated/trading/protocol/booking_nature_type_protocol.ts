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
import type { BookingNatureType } from '../domain/booking_nature_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BookingNatureTypeKey {
    code: string;
}

export interface BookingNatureTypeWrite {
    code: string;
    description: string;
}

export interface BookingNatureTypeChange {
    write: BookingNatureTypeWrite;
    precondition: Precondition;
}

export interface BookingNatureTypeRemoval {
    key: BookingNatureTypeKey;
    precondition: Precondition;
}

export interface BookingNatureTypeLookup {
    key: BookingNatureTypeKey;
    booking_nature_type: BookingNatureType | null;
}

export interface BookingNatureTypesFilter {
    code_one_of: string[] | null;
}

export interface BookingNatureTypeEvent {
    event_id: string;
    key: BookingNatureTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BookingNatureTypeVersionKey {
    booking_nature_type: BookingNatureTypeKey;
    version: number;
}

export interface BookingNatureTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBookingNatureTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BookingNatureTypesFilter | null;
    as_of: string | null;
}

export interface ListBookingNatureTypesResponse {
    result: Result;
    booking_nature_types: BookingNatureType[];
    total: number;
}

export interface GetBookingNatureTypeRequest {
    key: BookingNatureTypeKey;
}

export interface GetBookingNatureTypeResponse {
    result: Result;
    booking_nature_type: BookingNatureType | null;
}

export interface GetManyBookingNatureTypesRequest {
    keys: BookingNatureTypeKey[];
}

export interface GetManyBookingNatureTypesResponse {
    result: Result;
    entries: BookingNatureTypeLookup[];
}

export interface PutBookingNatureTypeRequest {
    change: BookingNatureTypeChange;
    intent: ChangeIntent;
}

export interface PutBookingNatureTypeResponse {
    result: Result;
    booking_nature_type: BookingNatureType | null;
}

export interface PutManyBookingNatureTypesRequest {
    changes: BookingNatureTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyBookingNatureTypesResponse {
    result: Result;
    booking_nature_types: BookingNatureType[];
}

export interface DeleteBookingNatureTypeRequest {
    removal: BookingNatureTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteBookingNatureTypeResponse {
    result: Result;
}

export interface DeleteManyBookingNatureTypesRequest {
    removals: BookingNatureTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBookingNatureTypesResponse {
    result: Result;
}

export interface ListBookingNatureTypeVersionsRequest {
    key: BookingNatureTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BookingNatureTypeVersionsFilter | null;
}

export interface ListBookingNatureTypeVersionsResponse {
    result: Result;
    versions: BookingNatureType[];
    total: number;
}

export interface GetBookingNatureTypeVersionRequest {
    key: BookingNatureTypeVersionKey;
}

export interface GetBookingNatureTypeVersionResponse {
    result: Result;
    version: BookingNatureType | null;
}

export const subjects = {
    list_booking_nature_types_request: 'trading.v1.booking_nature_types.list',
    get_booking_nature_type_request: 'trading.v1.booking_nature_types.get',
    get_many_booking_nature_types_request: 'trading.v1.booking_nature_types.get_many',
    put_booking_nature_type_request: 'trading.v1.booking_nature_types.put',
    put_many_booking_nature_types_request: 'trading.v1.booking_nature_types.put_many',
    delete_booking_nature_type_request: 'trading.v1.booking_nature_types.delete',
    delete_many_booking_nature_types_request: 'trading.v1.booking_nature_types.delete_many',
    list_booking_nature_type_versions_request: 'trading.v1.booking_nature_types_versions.list',
    get_booking_nature_type_version_request: 'trading.v1.booking_nature_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_booking_nature_types_request: true,
    get_booking_nature_type_request: true,
    get_many_booking_nature_types_request: true,
    put_booking_nature_type_request: true,
    put_many_booking_nature_types_request: true,
    delete_booking_nature_type_request: true,
    delete_many_booking_nature_types_request: true,
    list_booking_nature_type_versions_request: true,
    get_booking_nature_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.booking_nature_types_events.created',
    updated: 'trading.v1.booking_nature_types_events.updated',
    deleted: 'trading.v1.booking_nature_types_events.deleted',
} as const;

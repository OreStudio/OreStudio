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
import type { DeliveryOutcomeType } from '../domain/delivery_outcome_type.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DeliveryOutcomeTypeKey {
    code: string;
}

export interface DeliveryOutcomeTypeLookup {
    key: DeliveryOutcomeTypeKey;
    delivery_outcome_type: DeliveryOutcomeType | null;
}

export interface DeliveryOutcomeTypesFilter {
    code_one_of: string[] | null;
}

export interface DeliveryOutcomeTypeEvent {
    event_id: string;
    key: DeliveryOutcomeTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListDeliveryOutcomeTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DeliveryOutcomeTypesFilter | null;
}

export interface ListDeliveryOutcomeTypesResponse {
    result: Result;
    delivery_outcome_types: DeliveryOutcomeType[];
    total: number;
}

export interface GetDeliveryOutcomeTypeRequest {
    key: DeliveryOutcomeTypeKey;
}

export interface GetDeliveryOutcomeTypeResponse {
    result: Result;
    delivery_outcome_type: DeliveryOutcomeType | null;
}

export interface GetManyDeliveryOutcomeTypesRequest {
    keys: DeliveryOutcomeTypeKey[];
}

export interface GetManyDeliveryOutcomeTypesResponse {
    result: Result;
    entries: DeliveryOutcomeTypeLookup[];
}

export const subjects = {
    list_delivery_outcome_types_request: 'inbox.v1.delivery_outcome_types.list',
    get_delivery_outcome_type_request: 'inbox.v1.delivery_outcome_types.get',
    get_many_delivery_outcome_types_request: 'inbox.v1.delivery_outcome_types.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_delivery_outcome_types_request: true,
    get_delivery_outcome_type_request: true,
    get_many_delivery_outcome_types_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.delivery_outcome_types_events.created',
    updated: 'inbox.v1.delivery_outcome_types_events.updated',
    deleted: 'inbox.v1.delivery_outcome_types_events.deleted',
} as const;

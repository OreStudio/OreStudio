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
import type { RateCardComponent } from '../domain/rate_card_component.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RateCardComponentKey {
    driver: string;
}

export interface RateCardComponentWrite {
    id: string;
    source_id: string;
    driver: string;
    licence_right: string;
    effective_from: string;
    currency: string;
    charge_micros: number;
    effective_to: string | null;
}

export interface RateCardComponentChange {
    write: RateCardComponentWrite;
    precondition: Precondition;
}

export interface RateCardComponentRemoval {
    key: RateCardComponentKey;
    precondition: Precondition;
}

export interface RateCardComponentLookup {
    key: RateCardComponentKey;
    rate_card_component: RateCardComponent | null;
}

export interface RateCardComponentsFilter {
    source_id: string | null;
    driver: string | null;
    id_one_of: string[] | null;
    source_id_one_of: string[] | null;
    driver_one_of: string[] | null;
}

export interface RateCardComponentEvent {
    event_id: string;
    key: RateCardComponentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RateCardComponentVersionKey {
    rate_card_component: RateCardComponentKey;
    version: number;
}

export interface RateCardComponentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRateCardComponentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RateCardComponentsFilter | null;
    as_of: string | null;
}

export interface ListRateCardComponentsResponse {
    result: Result;
    rate_card_components: RateCardComponent[];
    total: number;
}

export interface GetRateCardComponentRequest {
    key: RateCardComponentKey;
}

export interface GetRateCardComponentResponse {
    result: Result;
    rate_card_component: RateCardComponent | null;
}

export interface GetManyRateCardComponentsRequest {
    keys: RateCardComponentKey[];
}

export interface GetManyRateCardComponentsResponse {
    result: Result;
    entries: RateCardComponentLookup[];
}

export interface PutRateCardComponentRequest {
    change: RateCardComponentChange;
    intent: ChangeIntent;
}

export interface PutRateCardComponentResponse {
    result: Result;
    rate_card_component: RateCardComponent | null;
}

export interface PutManyRateCardComponentsRequest {
    changes: RateCardComponentChange[];
    intent: ChangeIntent;
}

export interface PutManyRateCardComponentsResponse {
    result: Result;
    rate_card_components: RateCardComponent[];
}

export interface DeleteRateCardComponentRequest {
    removal: RateCardComponentRemoval;
    intent: ChangeIntent;
}

export interface DeleteRateCardComponentResponse {
    result: Result;
}

export interface DeleteManyRateCardComponentsRequest {
    removals: RateCardComponentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRateCardComponentsResponse {
    result: Result;
}

export interface ListRateCardComponentVersionsRequest {
    key: RateCardComponentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RateCardComponentVersionsFilter | null;
}

export interface ListRateCardComponentVersionsResponse {
    result: Result;
    versions: RateCardComponent[];
    total: number;
}

export interface GetRateCardComponentVersionRequest {
    key: RateCardComponentVersionKey;
}

export interface GetRateCardComponentVersionResponse {
    result: Result;
    version: RateCardComponent | null;
}

export const subjects = {
    list_rate_card_components_request: 'marketdata.v1.rate_card_components.list',
    get_rate_card_component_request: 'marketdata.v1.rate_card_components.get',
    get_many_rate_card_components_request: 'marketdata.v1.rate_card_components.get_many',
    put_rate_card_component_request: 'marketdata.v1.rate_card_components.put',
    put_many_rate_card_components_request: 'marketdata.v1.rate_card_components.put_many',
    delete_rate_card_component_request: 'marketdata.v1.rate_card_components.delete',
    delete_many_rate_card_components_request: 'marketdata.v1.rate_card_components.delete_many',
    list_rate_card_component_versions_request: 'marketdata.v1.rate_card_components_versions.list',
    get_rate_card_component_version_request: 'marketdata.v1.rate_card_components_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_rate_card_components_request: true,
    get_rate_card_component_request: true,
    get_many_rate_card_components_request: true,
    put_rate_card_component_request: true,
    put_many_rate_card_components_request: true,
    delete_rate_card_component_request: true,
    delete_many_rate_card_components_request: true,
    list_rate_card_component_versions_request: true,
    get_rate_card_component_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.rate_card_components_events.created',
    updated: 'marketdata.v1.rate_card_components_events.updated',
    deleted: 'marketdata.v1.rate_card_components_events.deleted',
} as const;

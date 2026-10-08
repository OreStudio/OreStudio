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
import type { CounterpartyBusinessCentre } from '../domain/counterparty_business_centre.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CounterpartyBusinessCentreKey {
    counterparty_id: string;
    business_centre_code: string;
}

export interface CounterpartyBusinessCentreWrite {
    counterparty_id: string;
    business_centre_code: string;
}

export interface CounterpartyBusinessCentreChange {
    write: CounterpartyBusinessCentreWrite;
    precondition: Precondition;
}

export interface CounterpartyBusinessCentreRemoval {
    key: CounterpartyBusinessCentreKey;
    precondition: Precondition;
}

export interface CounterpartyBusinessCentreLookup {
    key: CounterpartyBusinessCentreKey;
    counterparty_business_centre: CounterpartyBusinessCentre | null;
}

export interface CounterpartyBusinessCentresFilter {
    counterparty_id: string | null;
    counterparty_id_one_of: string[] | null;
}

export interface ListCounterpartyBusinessCentresRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyBusinessCentresFilter | null;
}

export interface ListCounterpartyBusinessCentresResponse {
    result: Result;
    counterparty_business_centres: CounterpartyBusinessCentre[];
    total: number;
}

export interface GetCounterpartyBusinessCentreRequest {
    key: CounterpartyBusinessCentreKey;
}

export interface GetCounterpartyBusinessCentreResponse {
    result: Result;
    counterparty_business_centre: CounterpartyBusinessCentre | null;
}

export interface GetManyCounterpartyBusinessCentresRequest {
    keys: CounterpartyBusinessCentreKey[];
}

export interface GetManyCounterpartyBusinessCentresResponse {
    result: Result;
    entries: CounterpartyBusinessCentreLookup[];
}

export interface PutCounterpartyBusinessCentreRequest {
    change: CounterpartyBusinessCentreChange;
    intent: ChangeIntent;
}

export interface PutCounterpartyBusinessCentreResponse {
    result: Result;
    counterparty_business_centre: CounterpartyBusinessCentre | null;
}

export interface PutManyCounterpartyBusinessCentresRequest {
    changes: CounterpartyBusinessCentreChange[];
    intent: ChangeIntent;
}

export interface PutManyCounterpartyBusinessCentresResponse {
    result: Result;
    counterparty_business_centres: CounterpartyBusinessCentre[];
}

export interface DeleteCounterpartyBusinessCentreRequest {
    removal: CounterpartyBusinessCentreRemoval;
    intent: ChangeIntent;
}

export interface DeleteCounterpartyBusinessCentreResponse {
    result: Result;
}

export interface DeleteManyCounterpartyBusinessCentresRequest {
    removals: CounterpartyBusinessCentreRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCounterpartyBusinessCentresResponse {
    result: Result;
}

export interface ListByCounterpartyIdCounterpartyBusinessCentresRequest {
    counterparty_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyBusinessCentresFilter | null;
}

export interface ListByCounterpartyIdCounterpartyBusinessCentresResponse {
    result: Result;
    counterparty_business_centres: CounterpartyBusinessCentre[];
    total: number;
}

export const subjects = {
    list_counterparty_business_centres_request: 'refdata.v1.counterparty_business_centres.list',
    get_counterparty_business_centre_request: 'refdata.v1.counterparty_business_centres.get',
    get_many_counterparty_business_centres_request:
        'refdata.v1.counterparty_business_centres.get_many',
    put_counterparty_business_centre_request: 'refdata.v1.counterparty_business_centres.put',
    put_many_counterparty_business_centres_request:
        'refdata.v1.counterparty_business_centres.put_many',
    delete_counterparty_business_centre_request: 'refdata.v1.counterparty_business_centres.delete',
    delete_many_counterparty_business_centres_request:
        'refdata.v1.counterparty_business_centres.delete_many',
    list_by_counterparty_id_counterparty_business_centres_request:
        'refdata.v1.counterparty_business_centres.list_by_counterparty_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_counterparty_business_centres_request: true,
    get_counterparty_business_centre_request: true,
    get_many_counterparty_business_centres_request: true,
    put_counterparty_business_centre_request: true,
    put_many_counterparty_business_centres_request: true,
    delete_counterparty_business_centre_request: true,
    delete_many_counterparty_business_centres_request: true,
    list_by_counterparty_id_counterparty_business_centres_request: true,
} as const;

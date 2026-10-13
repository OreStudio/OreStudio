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
import type { CreditBasketConstituent } from '../domain/credit_basket_constituent.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditBasketConstituentKey {
    trade_id: string;
    sequence_number: number;
}

export interface CreditBasketConstituentWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    issuer_id: string;
    qualifier: string | null;
    credit_curve_id: string | null;
    notional: string | null;
    weight: number | null;
    currency: string | null;
}

export interface CreditBasketConstituentChange {
    write: CreditBasketConstituentWrite;
    precondition: Precondition;
}

export interface CreditBasketConstituentRemoval {
    key: CreditBasketConstituentKey;
    precondition: Precondition;
}

export interface CreditBasketConstituentLookup {
    key: CreditBasketConstituentKey;
    credit_basket_constituent: CreditBasketConstituent | null;
}

export interface CreditBasketConstituentsFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface CreditBasketConstituentEvent {
    event_id: string;
    key: CreditBasketConstituentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditBasketConstituentVersionKey {
    credit_basket_constituent: CreditBasketConstituentKey;
    version: number;
}

export interface CreditBasketConstituentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditBasketConstituentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditBasketConstituentsFilter | null;
    as_of: string | null;
}

export interface ListCreditBasketConstituentsResponse {
    result: Result;
    credit_basket_constituents: CreditBasketConstituent[];
    total: number;
}

export interface GetCreditBasketConstituentRequest {
    key: CreditBasketConstituentKey;
}

export interface GetCreditBasketConstituentResponse {
    result: Result;
    credit_basket_constituent: CreditBasketConstituent | null;
}

export interface GetManyCreditBasketConstituentsRequest {
    keys: CreditBasketConstituentKey[];
}

export interface GetManyCreditBasketConstituentsResponse {
    result: Result;
    entries: CreditBasketConstituentLookup[];
}

export interface PutCreditBasketConstituentRequest {
    change: CreditBasketConstituentChange;
    intent: ChangeIntent;
}

export interface PutCreditBasketConstituentResponse {
    result: Result;
    credit_basket_constituent: CreditBasketConstituent | null;
}

export interface PutManyCreditBasketConstituentsRequest {
    changes: CreditBasketConstituentChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditBasketConstituentsResponse {
    result: Result;
    credit_basket_constituents: CreditBasketConstituent[];
}

export interface DeleteCreditBasketConstituentRequest {
    removal: CreditBasketConstituentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditBasketConstituentResponse {
    result: Result;
}

export interface DeleteManyCreditBasketConstituentsRequest {
    removals: CreditBasketConstituentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditBasketConstituentsResponse {
    result: Result;
}

export interface ListByTradeIdCreditBasketConstituentsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditBasketConstituentsFilter | null;
}

export interface ListByTradeIdCreditBasketConstituentsResponse {
    result: Result;
    credit_basket_constituents: CreditBasketConstituent[];
    total: number;
}

export interface ListCreditBasketConstituentVersionsRequest {
    key: CreditBasketConstituentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditBasketConstituentVersionsFilter | null;
}

export interface ListCreditBasketConstituentVersionsResponse {
    result: Result;
    versions: CreditBasketConstituent[];
    total: number;
}

export interface GetCreditBasketConstituentVersionRequest {
    key: CreditBasketConstituentVersionKey;
}

export interface GetCreditBasketConstituentVersionResponse {
    result: Result;
    version: CreditBasketConstituent | null;
}

export const subjects = {
    list_credit_basket_constituents_request: 'trading.v1.credit_basket_constituents.list',
    get_credit_basket_constituent_request: 'trading.v1.credit_basket_constituents.get',
    get_many_credit_basket_constituents_request: 'trading.v1.credit_basket_constituents.get_many',
    put_credit_basket_constituent_request: 'trading.v1.credit_basket_constituents.put',
    put_many_credit_basket_constituents_request: 'trading.v1.credit_basket_constituents.put_many',
    delete_credit_basket_constituent_request: 'trading.v1.credit_basket_constituents.delete',
    delete_many_credit_basket_constituents_request:
        'trading.v1.credit_basket_constituents.delete_many',
    list_by_trade_id_credit_basket_constituents_request:
        'trading.v1.credit_basket_constituents.list_by_trade_id',
    list_credit_basket_constituent_versions_request:
        'trading.v1.credit_basket_constituents_versions.list',
    get_credit_basket_constituent_version_request:
        'trading.v1.credit_basket_constituents_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_basket_constituents_request: true,
    get_credit_basket_constituent_request: true,
    get_many_credit_basket_constituents_request: true,
    put_credit_basket_constituent_request: true,
    put_many_credit_basket_constituents_request: true,
    delete_credit_basket_constituent_request: true,
    delete_many_credit_basket_constituents_request: true,
    list_by_trade_id_credit_basket_constituents_request: true,
    list_credit_basket_constituent_versions_request: true,
    get_credit_basket_constituent_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.credit_basket_constituents_events.created',
    updated: 'trading.v1.credit_basket_constituents_events.updated',
    deleted: 'trading.v1.credit_basket_constituents_events.deleted',
} as const;

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
import type { CreditLimit } from '../domain/credit_limit.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CreditLimitKey {
    code: string;
}

export interface CreditLimitWrite {
    id: string;
    code: string;
    party_id: string;
    counterparty_id: string;
    limit_amount: string | null;
    description: string | null;
}

export interface CreditLimitChange {
    write: CreditLimitWrite;
    precondition: Precondition;
}

export interface CreditLimitRemoval {
    key: CreditLimitKey;
    precondition: Precondition;
}

export interface CreditLimitLookup {
    key: CreditLimitKey;
    credit_limit: CreditLimit | null;
}

export interface CreditLimitsFilter {
    counterparty_id: string | null;
    id_one_of: string[] | null;
    counterparty_id_one_of: string[] | null;
}

export interface CreditLimitEvent {
    event_id: string;
    key: CreditLimitKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditLimitVersionKey {
    credit_limit: CreditLimitKey;
    version: number;
}

export interface CreditLimitVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditLimitsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditLimitsFilter | null;
    as_of: string | null;
}

export interface ListCreditLimitsResponse {
    result: Result;
    credit_limits: CreditLimit[];
    total: number;
}

export interface GetCreditLimitRequest {
    key: CreditLimitKey;
}

export interface GetCreditLimitResponse {
    result: Result;
    credit_limit: CreditLimit | null;
}

export interface GetManyCreditLimitsRequest {
    keys: CreditLimitKey[];
}

export interface GetManyCreditLimitsResponse {
    result: Result;
    entries: CreditLimitLookup[];
}

export interface PutCreditLimitRequest {
    change: CreditLimitChange;
    intent: ChangeIntent;
}

export interface PutCreditLimitResponse {
    result: Result;
    credit_limit: CreditLimit | null;
}

export interface PutManyCreditLimitsRequest {
    changes: CreditLimitChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditLimitsResponse {
    result: Result;
    credit_limits: CreditLimit[];
}

export interface DeleteCreditLimitRequest {
    removal: CreditLimitRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditLimitResponse {
    result: Result;
}

export interface DeleteManyCreditLimitsRequest {
    removals: CreditLimitRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditLimitsResponse {
    result: Result;
}

export interface ListByCounterpartyIdCreditLimitsRequest {
    counterparty_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditLimitsFilter | null;
}

export interface ListByCounterpartyIdCreditLimitsResponse {
    result: Result;
    credit_limits: CreditLimit[];
    total: number;
}

export interface ListCreditLimitVersionsRequest {
    key: CreditLimitKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditLimitVersionsFilter | null;
}

export interface ListCreditLimitVersionsResponse {
    result: Result;
    versions: CreditLimit[];
    total: number;
}

export interface GetCreditLimitVersionRequest {
    key: CreditLimitVersionKey;
}

export interface GetCreditLimitVersionResponse {
    result: Result;
    version: CreditLimit | null;
}

export const subjects = {
    list_credit_limits_request: 'refdata.v1.credit_limits.list',
    get_credit_limit_request: 'refdata.v1.credit_limits.get',
    get_many_credit_limits_request: 'refdata.v1.credit_limits.get_many',
    put_credit_limit_request: 'refdata.v1.credit_limits.put',
    put_many_credit_limits_request: 'refdata.v1.credit_limits.put_many',
    delete_credit_limit_request: 'refdata.v1.credit_limits.delete',
    delete_many_credit_limits_request: 'refdata.v1.credit_limits.delete_many',
    list_by_counterparty_id_credit_limits_request:
        'refdata.v1.credit_limits.list_by_counterparty_id',
    list_credit_limit_versions_request: 'refdata.v1.credit_limits_versions.list',
    get_credit_limit_version_request: 'refdata.v1.credit_limits_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_limits_request: true,
    get_credit_limit_request: true,
    get_many_credit_limits_request: true,
    put_credit_limit_request: true,
    put_many_credit_limits_request: true,
    delete_credit_limit_request: true,
    delete_many_credit_limits_request: true,
    list_by_counterparty_id_credit_limits_request: true,
    list_credit_limit_versions_request: true,
    get_credit_limit_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.credit_limits_events.created',
    updated: 'refdata.v1.credit_limits_events.updated',
    deleted: 'refdata.v1.credit_limits_events.deleted',
} as const;

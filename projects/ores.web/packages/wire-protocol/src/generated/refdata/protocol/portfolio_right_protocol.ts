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
import type { PortfolioRight } from '../domain/portfolio_right.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface PortfolioRightKey {
    right_code: string;
}

export interface PortfolioRightWrite {
    id: string;
    account_id: string;
    portfolio_id: string;
    right_code: string;
}

export interface PortfolioRightChange {
    write: PortfolioRightWrite;
    precondition: Precondition;
}

export interface PortfolioRightRemoval {
    key: PortfolioRightKey;
    precondition: Precondition;
}

export interface PortfolioRightLookup {
    key: PortfolioRightKey;
    portfolio_right: PortfolioRight | null;
}

export interface PortfolioRightsFilter {
    account_id: string | null;
    portfolio_id: string | null;
    id_one_of: string[] | null;
    account_id_one_of: string[] | null;
    portfolio_id_one_of: string[] | null;
}

export interface PortfolioRightEvent {
    event_id: string;
    key: PortfolioRightKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PortfolioRightVersionKey {
    portfolio_right: PortfolioRightKey;
    version: number;
}

export interface PortfolioRightVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPortfolioRightsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PortfolioRightsFilter | null;
    as_of: string | null;
}

export interface ListPortfolioRightsResponse {
    result: Result;
    portfolio_rights: PortfolioRight[];
    total: number;
}

export interface GetPortfolioRightRequest {
    key: PortfolioRightKey;
}

export interface GetPortfolioRightResponse {
    result: Result;
    portfolio_right: PortfolioRight | null;
}

export interface GetManyPortfolioRightsRequest {
    keys: PortfolioRightKey[];
}

export interface GetManyPortfolioRightsResponse {
    result: Result;
    entries: PortfolioRightLookup[];
}

export interface PutPortfolioRightRequest {
    change: PortfolioRightChange;
    intent: ChangeIntent;
}

export interface PutPortfolioRightResponse {
    result: Result;
    portfolio_right: PortfolioRight | null;
}

export interface PutManyPortfolioRightsRequest {
    changes: PortfolioRightChange[];
    intent: ChangeIntent;
}

export interface PutManyPortfolioRightsResponse {
    result: Result;
    portfolio_rights: PortfolioRight[];
}

export interface DeletePortfolioRightRequest {
    removal: PortfolioRightRemoval;
    intent: ChangeIntent;
}

export interface DeletePortfolioRightResponse {
    result: Result;
}

export interface DeleteManyPortfolioRightsRequest {
    removals: PortfolioRightRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPortfolioRightsResponse {
    result: Result;
}

export interface ListByAccountIdPortfolioRightsRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PortfolioRightsFilter | null;
}

export interface ListByAccountIdPortfolioRightsResponse {
    result: Result;
    portfolio_rights: PortfolioRight[];
    total: number;
}

export interface ListByPortfolioIdPortfolioRightsRequest {
    portfolio_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PortfolioRightsFilter | null;
}

export interface ListByPortfolioIdPortfolioRightsResponse {
    result: Result;
    portfolio_rights: PortfolioRight[];
    total: number;
}

export interface ListPortfolioRightVersionsRequest {
    key: PortfolioRightKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PortfolioRightVersionsFilter | null;
}

export interface ListPortfolioRightVersionsResponse {
    result: Result;
    versions: PortfolioRight[];
    total: number;
}

export interface GetPortfolioRightVersionRequest {
    key: PortfolioRightVersionKey;
}

export interface GetPortfolioRightVersionResponse {
    result: Result;
    version: PortfolioRight | null;
}

export const subjects = {
    list_portfolio_rights_request: 'refdata.v1.portfolio_rights.list',
    get_portfolio_right_request: 'refdata.v1.portfolio_rights.get',
    get_many_portfolio_rights_request: 'refdata.v1.portfolio_rights.get_many',
    put_portfolio_right_request: 'refdata.v1.portfolio_rights.put',
    put_many_portfolio_rights_request: 'refdata.v1.portfolio_rights.put_many',
    delete_portfolio_right_request: 'refdata.v1.portfolio_rights.delete',
    delete_many_portfolio_rights_request: 'refdata.v1.portfolio_rights.delete_many',
    list_by_account_id_portfolio_rights_request: 'refdata.v1.portfolio_rights.list_by_account_id',
    list_by_portfolio_id_portfolio_rights_request:
        'refdata.v1.portfolio_rights.list_by_portfolio_id',
    list_portfolio_right_versions_request: 'refdata.v1.portfolio_rights_versions.list',
    get_portfolio_right_version_request: 'refdata.v1.portfolio_rights_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_portfolio_rights_request: true,
    get_portfolio_right_request: true,
    get_many_portfolio_rights_request: true,
    put_portfolio_right_request: true,
    put_many_portfolio_rights_request: true,
    delete_portfolio_right_request: true,
    delete_many_portfolio_rights_request: true,
    list_by_account_id_portfolio_rights_request: true,
    list_by_portfolio_id_portfolio_rights_request: true,
    list_portfolio_right_versions_request: true,
    get_portfolio_right_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.portfolio_rights_events.created',
    updated: 'refdata.v1.portfolio_rights_events.updated',
    deleted: 'refdata.v1.portfolio_rights_events.deleted',
} as const;

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
import type { Portfolio } from '../domain/portfolio.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PortfolioKey {
    name: string;
}

export interface PortfolioWrite {
    id: string;
    name: string;
    description: string;
    parent_portfolio_id: string | null;
    owner_unit_id: string | null;
    purpose_type: string;
    aggregation_ccy: string;
    is_virtual: boolean;
    status: string;
}

export interface PortfolioChange {
    write: PortfolioWrite;
    precondition: Precondition;
}

export interface PortfolioRemoval {
    key: PortfolioKey;
    precondition: Precondition;
}

export interface PortfolioLookup {
    key: PortfolioKey;
    portfolio: Portfolio | null;
}

export interface PortfolioEvent {
    event_id: string;
    key: PortfolioKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PortfolioVersionKey {
    portfolio: PortfolioKey;
    version: number;
}

export interface PortfolioVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPortfoliosRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPortfoliosResponse {
    result: Result;
    portfolios: Portfolio[];
    total: number;
}

export interface GetPortfolioRequest {
    key: PortfolioKey;
}

export interface GetPortfolioResponse {
    result: Result;
    portfolio: Portfolio | null;
}

export interface GetManyPortfoliosRequest {
    keys: PortfolioKey[];
}

export interface GetManyPortfoliosResponse {
    result: Result;
    entries: PortfolioLookup[];
}

export interface PutPortfolioRequest {
    change: PortfolioChange;
    intent: ChangeIntent;
}

export interface PutPortfolioResponse {
    result: Result;
    portfolio: Portfolio;
}

export interface PutManyPortfoliosRequest {
    changes: PortfolioChange[];
    intent: ChangeIntent;
}

export interface PutManyPortfoliosResponse {
    result: Result;
    portfolios: Portfolio[];
}

export interface DeletePortfolioRequest {
    removal: PortfolioRemoval;
    intent: ChangeIntent;
}

export interface DeletePortfolioResponse {
    result: Result;
}

export interface DeleteManyPortfoliosRequest {
    removals: PortfolioRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPortfoliosResponse {
    result: Result;
}

export interface ListPortfolioVersionsRequest {
    key: PortfolioKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PortfolioVersionsFilter | null;
}

export interface ListPortfolioVersionsResponse {
    result: Result;
    versions: Portfolio[];
    total: number;
}

export interface GetPortfolioVersionRequest {
    key: PortfolioVersionKey;
}

export interface GetPortfolioVersionResponse {
    result: Result;
    version: Portfolio;
}

export const subjects = {
    list_portfolios_request: "refdata.v1.portfolios.list",
    get_portfolio_request: "refdata.v1.portfolios.get",
    get_many_portfolios_request: "refdata.v1.portfolios.get_many",
    put_portfolio_request: "refdata.v1.portfolios.put",
    put_many_portfolios_request: "refdata.v1.portfolios.put_many",
    delete_portfolio_request: "refdata.v1.portfolios.delete",
    delete_many_portfolios_request: "refdata.v1.portfolios.delete_many",
    list_portfolio_versions_request: "refdata.v1.portfolios_versions.list",
    get_portfolio_version_request: "refdata.v1.portfolios_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_portfolios_request: true,
    get_portfolio_request: true,
    get_many_portfolios_request: true,
    put_portfolio_request: true,
    put_many_portfolios_request: true,
    delete_portfolio_request: true,
    delete_many_portfolios_request: true,
    list_portfolio_versions_request: true,
    get_portfolio_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.portfolios_events.created",
    updated: "refdata.v1.portfolios_events.updated",
    deleted: "refdata.v1.portfolios_events.deleted",
} as const;

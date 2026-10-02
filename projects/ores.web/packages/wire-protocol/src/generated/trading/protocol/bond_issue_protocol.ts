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
import type { BondIssue } from '../domain/bond_issue.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondIssueKey {
    issue_id: string;
}

export interface BondIssueWrite {
    issue_id: string;
    security_id: string;
    issuer: string;
    face_value: string | null;
    issue_date: string | null;
    settlement_days: number;
    calendar: string | null;
    credit_curve_id: string | null;
    reference_curve_id: string | null;
    income_curve_id: string | null;
    credit_group: string | null;
    volatility_curve_id: string | null;
    price_quote_method: string | null;
    price_quote_base_value: string | null;
    sub_type: string | null;
    price_type: string | null;
    payer: string | null;
    credit_risk: string | null;
}

export interface BondIssueChange {
    write: BondIssueWrite;
    precondition: Precondition;
}

export interface BondIssueRemoval {
    key: BondIssueKey;
    precondition: Precondition;
}

export interface BondIssueLookup {
    key: BondIssueKey;
    bond_issue: BondIssue | null;
}

export interface BondIssueEvent {
    event_id: string;
    key: BondIssueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondIssueVersionKey {
    bond_issue: BondIssueKey;
    version: number;
}

export interface BondIssueVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondIssuesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondIssuesResponse {
    result: Result;
    issues: BondIssue[];
    total: number;
}

export interface GetBondIssueRequest {
    key: BondIssueKey;
}

export interface GetBondIssueResponse {
    result: Result;
    bond_issue: BondIssue | null;
}

export interface GetManyBondIssuesRequest {
    keys: BondIssueKey[];
}

export interface GetManyBondIssuesResponse {
    result: Result;
    entries: BondIssueLookup[];
}

export interface PutBondIssueRequest {
    change: BondIssueChange;
    intent: ChangeIntent;
}

export interface PutBondIssueResponse {
    result: Result;
    bond_issue: BondIssue | null;
}

export interface PutManyBondIssuesRequest {
    changes: BondIssueChange[];
    intent: ChangeIntent;
}

export interface PutManyBondIssuesResponse {
    result: Result;
    issues: BondIssue[];
}

export interface DeleteBondIssueRequest {
    removal: BondIssueRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondIssueResponse {
    result: Result;
}

export interface DeleteManyBondIssuesRequest {
    removals: BondIssueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondIssuesResponse {
    result: Result;
}

export interface ListBondIssueVersionsRequest {
    key: BondIssueKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondIssueVersionsFilter | null;
}

export interface ListBondIssueVersionsResponse {
    result: Result;
    versions: BondIssue[];
    total: number;
}

export interface GetBondIssueVersionRequest {
    key: BondIssueVersionKey;
}

export interface GetBondIssueVersionResponse {
    result: Result;
    version: BondIssue | null;
}

export const subjects = {
    list_bond_issues_request: 'trading.v1.bond_issues.list',
    get_bond_issue_request: 'trading.v1.bond_issues.get',
    get_many_bond_issues_request: 'trading.v1.bond_issues.get_many',
    put_bond_issue_request: 'trading.v1.bond_issues.put',
    put_many_bond_issues_request: 'trading.v1.bond_issues.put_many',
    delete_bond_issue_request: 'trading.v1.bond_issues.delete',
    delete_many_bond_issues_request: 'trading.v1.bond_issues.delete_many',
    list_bond_issue_versions_request: 'trading.v1.bond_issues_versions.list',
    get_bond_issue_version_request: 'trading.v1.bond_issues_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_issues_request: true,
    get_bond_issue_request: true,
    get_many_bond_issues_request: true,
    put_bond_issue_request: true,
    put_many_bond_issues_request: true,
    delete_bond_issue_request: true,
    delete_many_bond_issues_request: true,
    list_bond_issue_versions_request: true,
    get_bond_issue_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_issues_events.created',
    updated: 'trading.v1.bond_issues_events.updated',
    deleted: 'trading.v1.bond_issues_events.deleted',
} as const;

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
import type { BondIssueConversionTarget } from '../domain/bond_issue_conversion_target.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondIssueConversionTargetKey {
    issue_id: string;
    sequence_number: number;
}

export interface BondIssueConversionTargetWrite {
    issue_id: string;
    sequence_number: number;
    underlying_id: string;
    conversion_ratio: number;
}

export interface BondIssueConversionTargetChange {
    write: BondIssueConversionTargetWrite;
    precondition: Precondition;
}

export interface BondIssueConversionTargetRemoval {
    key: BondIssueConversionTargetKey;
    precondition: Precondition;
}

export interface BondIssueConversionTargetLookup {
    key: BondIssueConversionTargetKey;
    bond_issue_conversion_target: BondIssueConversionTarget | null;
}

export interface BondIssueConversionTargetEvent {
    event_id: string;
    key: BondIssueConversionTargetKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondIssueConversionTargetVersionKey {
    bond_issue_conversion_target: BondIssueConversionTargetKey;
    version: number;
}

export interface BondIssueConversionTargetVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondIssueConversionTargetsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondIssueConversionTargetsResponse {
    result: Result;
    conversion_targets: BondIssueConversionTarget[];
    total: number;
}

export interface GetBondIssueConversionTargetRequest {
    key: BondIssueConversionTargetKey;
}

export interface GetBondIssueConversionTargetResponse {
    result: Result;
    bond_issue_conversion_target: BondIssueConversionTarget | null;
}

export interface GetManyBondIssueConversionTargetsRequest {
    keys: BondIssueConversionTargetKey[];
}

export interface GetManyBondIssueConversionTargetsResponse {
    result: Result;
    entries: BondIssueConversionTargetLookup[];
}

export interface PutBondIssueConversionTargetRequest {
    change: BondIssueConversionTargetChange;
    intent: ChangeIntent;
}

export interface PutBondIssueConversionTargetResponse {
    result: Result;
    bond_issue_conversion_target: BondIssueConversionTarget | null;
}

export interface PutManyBondIssueConversionTargetsRequest {
    changes: BondIssueConversionTargetChange[];
    intent: ChangeIntent;
}

export interface PutManyBondIssueConversionTargetsResponse {
    result: Result;
    conversion_targets: BondIssueConversionTarget[];
}

export interface DeleteBondIssueConversionTargetRequest {
    removal: BondIssueConversionTargetRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondIssueConversionTargetResponse {
    result: Result;
}

export interface DeleteManyBondIssueConversionTargetsRequest {
    removals: BondIssueConversionTargetRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondIssueConversionTargetsResponse {
    result: Result;
}

export interface ListBondIssueConversionTargetVersionsRequest {
    key: BondIssueConversionTargetKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondIssueConversionTargetVersionsFilter | null;
}

export interface ListBondIssueConversionTargetVersionsResponse {
    result: Result;
    versions: BondIssueConversionTarget[];
    total: number;
}

export interface GetBondIssueConversionTargetVersionRequest {
    key: BondIssueConversionTargetVersionKey;
}

export interface GetBondIssueConversionTargetVersionResponse {
    result: Result;
    version: BondIssueConversionTarget | null;
}

export const subjects = {
    list_bond_issue_conversion_targets_request: 'trading.v1.bond_issue_conversion_targets.list',
    get_bond_issue_conversion_target_request: 'trading.v1.bond_issue_conversion_targets.get',
    get_many_bond_issue_conversion_targets_request:
        'trading.v1.bond_issue_conversion_targets.get_many',
    put_bond_issue_conversion_target_request: 'trading.v1.bond_issue_conversion_targets.put',
    put_many_bond_issue_conversion_targets_request:
        'trading.v1.bond_issue_conversion_targets.put_many',
    delete_bond_issue_conversion_target_request: 'trading.v1.bond_issue_conversion_targets.delete',
    delete_many_bond_issue_conversion_targets_request:
        'trading.v1.bond_issue_conversion_targets.delete_many',
    list_bond_issue_conversion_target_versions_request:
        'trading.v1.bond_issue_conversion_targets_versions.list',
    get_bond_issue_conversion_target_version_request:
        'trading.v1.bond_issue_conversion_targets_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_issue_conversion_targets_request: true,
    get_bond_issue_conversion_target_request: true,
    get_many_bond_issue_conversion_targets_request: true,
    put_bond_issue_conversion_target_request: true,
    put_many_bond_issue_conversion_targets_request: true,
    delete_bond_issue_conversion_target_request: true,
    delete_many_bond_issue_conversion_targets_request: true,
    list_bond_issue_conversion_target_versions_request: true,
    get_bond_issue_conversion_target_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_issue_conversion_targets_events.created',
    updated: 'trading.v1.bond_issue_conversion_targets_events.updated',
    deleted: 'trading.v1.bond_issue_conversion_targets_events.deleted',
} as const;

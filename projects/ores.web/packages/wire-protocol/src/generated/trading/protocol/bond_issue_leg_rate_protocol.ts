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
import type { BondIssueLegRate } from '../domain/bond_issue_leg_rate.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondIssueLegRateKey {
    issue_id: string;
    leg_number: number;
}

export interface BondIssueLegRateWrite {
    issue_id: string;
    leg_number: number;
    rate_kind: string;
    index: string | null;
    is_in_arrears: boolean | null;
    fixing_days: number | null;
    fixing_calendar: string | null;
    last_recent_period: string | null;
    last_recent_period_calendar: string | null;
    lookback: string | null;
    rate_cutoff: number | null;
    is_averaged: boolean | null;
    has_sub_periods: boolean | null;
    include_spread: boolean | null;
    is_not_resetting_xccy: boolean | null;
    naked_option: boolean | null;
    local_cap_floor: boolean | null;
    stub_use_original_curve: boolean | null;
    observation_shift: boolean | null;
    front_stub_short_index: string | null;
    front_stub_long_index: string | null;
    front_stub_rounding_type: string | null;
    front_stub_rounding_precision: number | null;
    back_stub_short_index: string | null;
    back_stub_long_index: string | null;
    back_stub_rounding_type: string | null;
    back_stub_rounding_precision: number | null;
}

export interface BondIssueLegRateChange {
    write: BondIssueLegRateWrite;
    precondition: Precondition;
}

export interface BondIssueLegRateRemoval {
    key: BondIssueLegRateKey;
    precondition: Precondition;
}

export interface BondIssueLegRateLookup {
    key: BondIssueLegRateKey;
    bond_issue_leg_rate: BondIssueLegRate | null;
}

export interface BondIssueLegRateEvent {
    event_id: string;
    key: BondIssueLegRateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondIssueLegRateVersionKey {
    bond_issue_leg_rate: BondIssueLegRateKey;
    version: number;
}

export interface BondIssueLegRateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondIssueLegRatesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondIssueLegRatesResponse {
    result: Result;
    bond_issue_leg_rates: BondIssueLegRate[];
    total: number;
}

export interface GetBondIssueLegRateRequest {
    key: BondIssueLegRateKey;
}

export interface GetBondIssueLegRateResponse {
    result: Result;
    bond_issue_leg_rate: BondIssueLegRate | null;
}

export interface GetManyBondIssueLegRatesRequest {
    keys: BondIssueLegRateKey[];
}

export interface GetManyBondIssueLegRatesResponse {
    result: Result;
    entries: BondIssueLegRateLookup[];
}

export interface PutBondIssueLegRateRequest {
    change: BondIssueLegRateChange;
    intent: ChangeIntent;
}

export interface PutBondIssueLegRateResponse {
    result: Result;
    bond_issue_leg_rate: BondIssueLegRate | null;
}

export interface PutManyBondIssueLegRatesRequest {
    changes: BondIssueLegRateChange[];
    intent: ChangeIntent;
}

export interface PutManyBondIssueLegRatesResponse {
    result: Result;
    bond_issue_leg_rates: BondIssueLegRate[];
}

export interface DeleteBondIssueLegRateRequest {
    removal: BondIssueLegRateRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondIssueLegRateResponse {
    result: Result;
}

export interface DeleteManyBondIssueLegRatesRequest {
    removals: BondIssueLegRateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondIssueLegRatesResponse {
    result: Result;
}

export interface ListBondIssueLegRateVersionsRequest {
    key: BondIssueLegRateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondIssueLegRateVersionsFilter | null;
}

export interface ListBondIssueLegRateVersionsResponse {
    result: Result;
    versions: BondIssueLegRate[];
    total: number;
}

export interface GetBondIssueLegRateVersionRequest {
    key: BondIssueLegRateVersionKey;
}

export interface GetBondIssueLegRateVersionResponse {
    result: Result;
    version: BondIssueLegRate | null;
}

export const subjects = {
    list_bond_issue_leg_rates_request: 'trading.v1.bond_issue_leg_rates.list',
    get_bond_issue_leg_rate_request: 'trading.v1.bond_issue_leg_rates.get',
    get_many_bond_issue_leg_rates_request: 'trading.v1.bond_issue_leg_rates.get_many',
    put_bond_issue_leg_rate_request: 'trading.v1.bond_issue_leg_rates.put',
    put_many_bond_issue_leg_rates_request: 'trading.v1.bond_issue_leg_rates.put_many',
    delete_bond_issue_leg_rate_request: 'trading.v1.bond_issue_leg_rates.delete',
    delete_many_bond_issue_leg_rates_request: 'trading.v1.bond_issue_leg_rates.delete_many',
    list_bond_issue_leg_rate_versions_request: 'trading.v1.bond_issue_leg_rates_versions.list',
    get_bond_issue_leg_rate_version_request: 'trading.v1.bond_issue_leg_rates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_issue_leg_rates_request: true,
    get_bond_issue_leg_rate_request: true,
    get_many_bond_issue_leg_rates_request: true,
    put_bond_issue_leg_rate_request: true,
    put_many_bond_issue_leg_rates_request: true,
    delete_bond_issue_leg_rate_request: true,
    delete_many_bond_issue_leg_rates_request: true,
    list_bond_issue_leg_rate_versions_request: true,
    get_bond_issue_leg_rate_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_issue_leg_rates_events.created',
    updated: 'trading.v1.bond_issue_leg_rates_events.updated',
    deleted: 'trading.v1.bond_issue_leg_rates_events.deleted',
} as const;

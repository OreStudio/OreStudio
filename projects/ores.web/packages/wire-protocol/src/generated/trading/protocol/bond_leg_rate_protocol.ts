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
import type { BondLegRate } from '../domain/bond_leg_rate.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondLegRateKey {
    trade_id: string;
    leg_role: string;
    leg_number: number;
}

export interface BondLegRateWrite {
    trade_id: string;
    leg_role: string;
    leg_number: number;
    trade_activity_id: string;
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

export interface BondLegRateChange {
    write: BondLegRateWrite;
    precondition: Precondition;
}

export interface BondLegRateRemoval {
    key: BondLegRateKey;
    precondition: Precondition;
}

export interface BondLegRateLookup {
    key: BondLegRateKey;
    bond_leg_rate: BondLegRate | null;
}

export interface BondLegRateEvent {
    event_id: string;
    key: BondLegRateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondLegRateVersionKey {
    bond_leg_rate: BondLegRateKey;
    version: number;
}

export interface BondLegRateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondLegRatesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListBondLegRatesResponse {
    result: Result;
    bond_leg_rates: BondLegRate[];
    total: number;
}

export interface GetBondLegRateRequest {
    key: BondLegRateKey;
}

export interface GetBondLegRateResponse {
    result: Result;
    bond_leg_rate: BondLegRate | null;
}

export interface GetManyBondLegRatesRequest {
    keys: BondLegRateKey[];
}

export interface GetManyBondLegRatesResponse {
    result: Result;
    entries: BondLegRateLookup[];
}

export interface PutBondLegRateRequest {
    change: BondLegRateChange;
    intent: ChangeIntent;
}

export interface PutBondLegRateResponse {
    result: Result;
    bond_leg_rate: BondLegRate | null;
}

export interface PutManyBondLegRatesRequest {
    changes: BondLegRateChange[];
    intent: ChangeIntent;
}

export interface PutManyBondLegRatesResponse {
    result: Result;
    bond_leg_rates: BondLegRate[];
}

export interface DeleteBondLegRateRequest {
    removal: BondLegRateRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondLegRateResponse {
    result: Result;
}

export interface DeleteManyBondLegRatesRequest {
    removals: BondLegRateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondLegRatesResponse {
    result: Result;
}

export interface ListBondLegRateVersionsRequest {
    key: BondLegRateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondLegRateVersionsFilter | null;
}

export interface ListBondLegRateVersionsResponse {
    result: Result;
    versions: BondLegRate[];
    total: number;
}

export interface GetBondLegRateVersionRequest {
    key: BondLegRateVersionKey;
}

export interface GetBondLegRateVersionResponse {
    result: Result;
    version: BondLegRate | null;
}

export const subjects = {
    list_bond_leg_rates_request: 'trading.v1.bond_leg_rates.list',
    get_bond_leg_rate_request: 'trading.v1.bond_leg_rates.get',
    get_many_bond_leg_rates_request: 'trading.v1.bond_leg_rates.get_many',
    put_bond_leg_rate_request: 'trading.v1.bond_leg_rates.put',
    put_many_bond_leg_rates_request: 'trading.v1.bond_leg_rates.put_many',
    delete_bond_leg_rate_request: 'trading.v1.bond_leg_rates.delete',
    delete_many_bond_leg_rates_request: 'trading.v1.bond_leg_rates.delete_many',
    list_bond_leg_rate_versions_request: 'trading.v1.bond_leg_rates_versions.list',
    get_bond_leg_rate_version_request: 'trading.v1.bond_leg_rates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_leg_rates_request: true,
    get_bond_leg_rate_request: true,
    get_many_bond_leg_rates_request: true,
    put_bond_leg_rate_request: true,
    put_many_bond_leg_rates_request: true,
    delete_bond_leg_rate_request: true,
    delete_many_bond_leg_rates_request: true,
    list_bond_leg_rate_versions_request: true,
    get_bond_leg_rate_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_leg_rates_events.created',
    updated: 'trading.v1.bond_leg_rates_events.updated',
    deleted: 'trading.v1.bond_leg_rates_events.deleted',
} as const;

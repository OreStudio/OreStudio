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
import type { InstrumentOptionPremium } from '../domain/instrument_option_premium.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentOptionPremiumKey {
    trade_id: string;
    sequence_number: number;
}

export interface InstrumentOptionPremiumWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    amount: string;
    currency: string;
    pay_date: string;
    has_settlement: boolean;
    settlement_pay_currency: string | null;
    settlement_fx_index: string | null;
    settlement_fixing_date: string | null;
}

export interface InstrumentOptionPremiumChange {
    write: InstrumentOptionPremiumWrite;
    precondition: Precondition;
}

export interface InstrumentOptionPremiumRemoval {
    key: InstrumentOptionPremiumKey;
    precondition: Precondition;
}

export interface InstrumentOptionPremiumLookup {
    key: InstrumentOptionPremiumKey;
    instrument_option_premium: InstrumentOptionPremium | null;
}

export interface InstrumentOptionPremiumEvent {
    event_id: string;
    key: InstrumentOptionPremiumKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentOptionPremiumVersionKey {
    instrument_option_premium: InstrumentOptionPremiumKey;
    version: number;
}

export interface InstrumentOptionPremiumVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentOptionPremiumsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListInstrumentOptionPremiumsResponse {
    result: Result;
    option_premiums: InstrumentOptionPremium[];
    total: number;
}

export interface GetInstrumentOptionPremiumRequest {
    key: InstrumentOptionPremiumKey;
}

export interface GetInstrumentOptionPremiumResponse {
    result: Result;
    instrument_option_premium: InstrumentOptionPremium | null;
}

export interface GetManyInstrumentOptionPremiumsRequest {
    keys: InstrumentOptionPremiumKey[];
}

export interface GetManyInstrumentOptionPremiumsResponse {
    result: Result;
    entries: InstrumentOptionPremiumLookup[];
}

export interface PutInstrumentOptionPremiumRequest {
    change: InstrumentOptionPremiumChange;
    intent: ChangeIntent;
}

export interface PutInstrumentOptionPremiumResponse {
    result: Result;
    instrument_option_premium: InstrumentOptionPremium | null;
}

export interface PutManyInstrumentOptionPremiumsRequest {
    changes: InstrumentOptionPremiumChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentOptionPremiumsResponse {
    result: Result;
    option_premiums: InstrumentOptionPremium[];
}

export interface DeleteInstrumentOptionPremiumRequest {
    removal: InstrumentOptionPremiumRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentOptionPremiumResponse {
    result: Result;
}

export interface DeleteManyInstrumentOptionPremiumsRequest {
    removals: InstrumentOptionPremiumRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentOptionPremiumsResponse {
    result: Result;
}

export interface ListInstrumentOptionPremiumVersionsRequest {
    key: InstrumentOptionPremiumKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentOptionPremiumVersionsFilter | null;
}

export interface ListInstrumentOptionPremiumVersionsResponse {
    result: Result;
    versions: InstrumentOptionPremium[];
    total: number;
}

export interface GetInstrumentOptionPremiumVersionRequest {
    key: InstrumentOptionPremiumVersionKey;
}

export interface GetInstrumentOptionPremiumVersionResponse {
    result: Result;
    version: InstrumentOptionPremium | null;
}

export const subjects = {
    list_instrument_option_premiums_request: 'trading.v1.instrument_option_premiums.list',
    get_instrument_option_premium_request: 'trading.v1.instrument_option_premiums.get',
    get_many_instrument_option_premiums_request: 'trading.v1.instrument_option_premiums.get_many',
    put_instrument_option_premium_request: 'trading.v1.instrument_option_premiums.put',
    put_many_instrument_option_premiums_request: 'trading.v1.instrument_option_premiums.put_many',
    delete_instrument_option_premium_request: 'trading.v1.instrument_option_premiums.delete',
    delete_many_instrument_option_premiums_request:
        'trading.v1.instrument_option_premiums.delete_many',
    list_instrument_option_premium_versions_request:
        'trading.v1.instrument_option_premiums_versions.list',
    get_instrument_option_premium_version_request:
        'trading.v1.instrument_option_premiums_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_option_premiums_request: true,
    get_instrument_option_premium_request: true,
    get_many_instrument_option_premiums_request: true,
    put_instrument_option_premium_request: true,
    put_many_instrument_option_premiums_request: true,
    delete_instrument_option_premium_request: true,
    delete_many_instrument_option_premiums_request: true,
    list_instrument_option_premium_versions_request: true,
    get_instrument_option_premium_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_option_premiums_events.created',
    updated: 'trading.v1.instrument_option_premiums_events.updated',
    deleted: 'trading.v1.instrument_option_premiums_events.deleted',
} as const;

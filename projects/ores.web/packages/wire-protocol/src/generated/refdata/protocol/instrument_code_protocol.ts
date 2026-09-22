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
import type { InstrumentCode } from '../domain/instrument_code.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentCodeKey {
    code: string;
}

export interface InstrumentCodeWrite {
    code: string;
    name: string;
    description: string;
    asset_class: string | null;
    ore_trade_type: string | null;
    display_order: number;
    curve_role: string;
}

export interface InstrumentCodeChange {
    write: InstrumentCodeWrite;
    precondition: Precondition;
}

export interface InstrumentCodeRemoval {
    key: InstrumentCodeKey;
    precondition: Precondition;
}

export interface InstrumentCodeLookup {
    key: InstrumentCodeKey;
    instrument_code: InstrumentCode | null;
}

export interface InstrumentCodeEvent {
    event_id: string;
    key: InstrumentCodeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentCodeVersionKey {
    instrument_code: InstrumentCodeKey;
    version: number;
}

export interface InstrumentCodeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentCodesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListInstrumentCodesResponse {
    result: Result;
    instruments: InstrumentCode[];
    total: number;
}

export interface GetInstrumentCodeRequest {
    key: InstrumentCodeKey;
}

export interface GetInstrumentCodeResponse {
    result: Result;
    instrument_code: InstrumentCode | null;
}

export interface GetManyInstrumentCodesRequest {
    keys: InstrumentCodeKey[];
}

export interface GetManyInstrumentCodesResponse {
    result: Result;
    entries: InstrumentCodeLookup[];
}

export interface PutInstrumentCodeRequest {
    change: InstrumentCodeChange;
    intent: ChangeIntent;
}

export interface PutInstrumentCodeResponse {
    result: Result;
    instrument_code: InstrumentCode;
}

export interface PutManyInstrumentCodesRequest {
    changes: InstrumentCodeChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentCodesResponse {
    result: Result;
    instruments: InstrumentCode[];
}

export interface DeleteInstrumentCodeRequest {
    removal: InstrumentCodeRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentCodeResponse {
    result: Result;
}

export interface DeleteManyInstrumentCodesRequest {
    removals: InstrumentCodeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentCodesResponse {
    result: Result;
}

export interface ListInstrumentCodeVersionsRequest {
    key: InstrumentCodeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentCodeVersionsFilter | null;
}

export interface ListInstrumentCodeVersionsResponse {
    result: Result;
    versions: InstrumentCode[];
    total: number;
}

export interface GetInstrumentCodeVersionRequest {
    key: InstrumentCodeVersionKey;
}

export interface GetInstrumentCodeVersionResponse {
    result: Result;
    version: InstrumentCode;
}

export const subjects = {
    list_instrument_codes_request: "refdata.v1.instrument_codes.list",
    get_instrument_code_request: "refdata.v1.instrument_codes.get",
    get_many_instrument_codes_request: "refdata.v1.instrument_codes.get_many",
    put_instrument_code_request: "refdata.v1.instrument_codes.put",
    put_many_instrument_codes_request: "refdata.v1.instrument_codes.put_many",
    delete_instrument_code_request: "refdata.v1.instrument_codes.delete",
    delete_many_instrument_codes_request: "refdata.v1.instrument_codes.delete_many",
    list_instrument_code_versions_request: "refdata.v1.instrument_codes_versions.list",
    get_instrument_code_version_request: "refdata.v1.instrument_codes_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_codes_request: true,
    get_instrument_code_request: true,
    get_many_instrument_codes_request: true,
    put_instrument_code_request: true,
    put_many_instrument_codes_request: true,
    delete_instrument_code_request: true,
    delete_many_instrument_codes_request: true,
    list_instrument_code_versions_request: true,
    get_instrument_code_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.instrument_codes_events.created",
    updated: "refdata.v1.instrument_codes_events.updated",
    deleted: "refdata.v1.instrument_codes_events.deleted",
} as const;

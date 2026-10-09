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
import type { IrCurveTemplateEntry } from '../domain/ir_curve_template_entry.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IrCurveTemplateEntryKey {
    id: string;
}

export interface IrCurveTemplateEntryWrite {
    id: string;
    party_id: string;
    ir_curve_config_id: string;
    sequence_index: number;
    start_tenor_code: string;
    end_tenor_code: string;
    instrument_code: string;
}

export interface IrCurveTemplateEntryChange {
    write: IrCurveTemplateEntryWrite;
    precondition: Precondition;
}

export interface IrCurveTemplateEntryRemoval {
    key: IrCurveTemplateEntryKey;
    precondition: Precondition;
}

export interface IrCurveTemplateEntryLookup {
    key: IrCurveTemplateEntryKey;
    ir_curve_template_entry: IrCurveTemplateEntry | null;
}

export interface IrCurveTemplateEntriesFilter {
    id_one_of: string[] | null;
}

export interface IrCurveTemplateEntryEvent {
    event_id: string;
    key: IrCurveTemplateEntryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IrCurveTemplateEntryVersionKey {
    ir_curve_template_entry: IrCurveTemplateEntryKey;
    version: number;
}

export interface IrCurveTemplateEntryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIrCurveTemplateEntriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveTemplateEntriesFilter | null;
    as_of: string | null;
}

export interface ListIrCurveTemplateEntriesResponse {
    result: Result;
    ir_curve_template_entries: IrCurveTemplateEntry[];
    total: number;
}

export interface GetIrCurveTemplateEntryRequest {
    key: IrCurveTemplateEntryKey;
}

export interface GetIrCurveTemplateEntryResponse {
    result: Result;
    ir_curve_template_entry: IrCurveTemplateEntry | null;
}

export interface GetManyIrCurveTemplateEntriesRequest {
    keys: IrCurveTemplateEntryKey[];
}

export interface GetManyIrCurveTemplateEntriesResponse {
    result: Result;
    entries: IrCurveTemplateEntryLookup[];
}

export interface PutIrCurveTemplateEntryRequest {
    change: IrCurveTemplateEntryChange;
    intent: ChangeIntent;
}

export interface PutIrCurveTemplateEntryResponse {
    result: Result;
    ir_curve_template_entry: IrCurveTemplateEntry | null;
}

export interface PutManyIrCurveTemplateEntriesRequest {
    changes: IrCurveTemplateEntryChange[];
    intent: ChangeIntent;
}

export interface PutManyIrCurveTemplateEntriesResponse {
    result: Result;
    ir_curve_template_entries: IrCurveTemplateEntry[];
}

export interface DeleteIrCurveTemplateEntryRequest {
    removal: IrCurveTemplateEntryRemoval;
    intent: ChangeIntent;
}

export interface DeleteIrCurveTemplateEntryResponse {
    result: Result;
}

export interface DeleteManyIrCurveTemplateEntriesRequest {
    removals: IrCurveTemplateEntryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIrCurveTemplateEntriesResponse {
    result: Result;
}

export interface ListIrCurveTemplateEntryVersionsRequest {
    key: IrCurveTemplateEntryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveTemplateEntryVersionsFilter | null;
}

export interface ListIrCurveTemplateEntryVersionsResponse {
    result: Result;
    versions: IrCurveTemplateEntry[];
    total: number;
}

export interface GetIrCurveTemplateEntryVersionRequest {
    key: IrCurveTemplateEntryVersionKey;
}

export interface GetIrCurveTemplateEntryVersionResponse {
    result: Result;
    version: IrCurveTemplateEntry | null;
}

export const subjects = {
    list_ir_curve_template_entries_request: 'synthetic.v1.ir_curve_template_entries.list',
    get_ir_curve_template_entry_request: 'synthetic.v1.ir_curve_template_entries.get',
    get_many_ir_curve_template_entries_request: 'synthetic.v1.ir_curve_template_entries.get_many',
    put_ir_curve_template_entry_request: 'synthetic.v1.ir_curve_template_entries.put',
    put_many_ir_curve_template_entries_request: 'synthetic.v1.ir_curve_template_entries.put_many',
    delete_ir_curve_template_entry_request: 'synthetic.v1.ir_curve_template_entries.delete',
    delete_many_ir_curve_template_entries_request:
        'synthetic.v1.ir_curve_template_entries.delete_many',
    list_ir_curve_template_entry_versions_request:
        'synthetic.v1.ir_curve_template_entries_versions.list',
    get_ir_curve_template_entry_version_request:
        'synthetic.v1.ir_curve_template_entries_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ir_curve_template_entries_request: true,
    get_ir_curve_template_entry_request: true,
    get_many_ir_curve_template_entries_request: true,
    put_ir_curve_template_entry_request: true,
    put_many_ir_curve_template_entries_request: true,
    delete_ir_curve_template_entry_request: true,
    delete_many_ir_curve_template_entries_request: true,
    list_ir_curve_template_entry_versions_request: true,
    get_ir_curve_template_entry_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.ir_curve_template_entries_events.created',
    updated: 'synthetic.v1.ir_curve_template_entries_events.updated',
    deleted: 'synthetic.v1.ir_curve_template_entries_events.deleted',
} as const;

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
import type { ReportMarketBinding } from '../domain/report_market_binding.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportMarketBindingKey {
    id: string;
}

export interface ReportMarketBindingWrite {
    id: string;
    report_definition_id: string;
    role: string;
    configuration_name: string;
    position: number;
}

export interface ReportMarketBindingChange {
    write: ReportMarketBindingWrite;
    precondition: Precondition;
}

export interface ReportMarketBindingRemoval {
    key: ReportMarketBindingKey;
    precondition: Precondition;
}

export interface ReportMarketBindingLookup {
    key: ReportMarketBindingKey;
    report_market_binding: ReportMarketBinding | null;
}

export interface ReportMarketBindingsFilter {
    id_one_of: string[] | null;
}

export interface ReportMarketBindingEvent {
    event_id: string;
    key: ReportMarketBindingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportMarketBindingVersionKey {
    report_market_binding: ReportMarketBindingKey;
    version: number;
}

export interface ReportMarketBindingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportMarketBindingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportMarketBindingsFilter | null;
    as_of: string | null;
}

export interface ListReportMarketBindingsResponse {
    result: Result;
    bindings: ReportMarketBinding[];
    total: number;
}

export interface GetReportMarketBindingRequest {
    key: ReportMarketBindingKey;
}

export interface GetReportMarketBindingResponse {
    result: Result;
    report_market_binding: ReportMarketBinding | null;
}

export interface GetManyReportMarketBindingsRequest {
    keys: ReportMarketBindingKey[];
}

export interface GetManyReportMarketBindingsResponse {
    result: Result;
    entries: ReportMarketBindingLookup[];
}

export interface PutReportMarketBindingRequest {
    change: ReportMarketBindingChange;
    intent: ChangeIntent;
}

export interface PutReportMarketBindingResponse {
    result: Result;
    report_market_binding: ReportMarketBinding | null;
}

export interface PutManyReportMarketBindingsRequest {
    changes: ReportMarketBindingChange[];
    intent: ChangeIntent;
}

export interface PutManyReportMarketBindingsResponse {
    result: Result;
    bindings: ReportMarketBinding[];
}

export interface DeleteReportMarketBindingRequest {
    removal: ReportMarketBindingRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportMarketBindingResponse {
    result: Result;
}

export interface DeleteManyReportMarketBindingsRequest {
    removals: ReportMarketBindingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportMarketBindingsResponse {
    result: Result;
}

export interface ListReportMarketBindingVersionsRequest {
    key: ReportMarketBindingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportMarketBindingVersionsFilter | null;
}

export interface ListReportMarketBindingVersionsResponse {
    result: Result;
    versions: ReportMarketBinding[];
    total: number;
}

export interface GetReportMarketBindingVersionRequest {
    key: ReportMarketBindingVersionKey;
}

export interface GetReportMarketBindingVersionResponse {
    result: Result;
    version: ReportMarketBinding | null;
}

export const subjects = {
    list_report_market_bindings_request: 'reporting.v1.report_market_bindings.list',
    get_report_market_binding_request: 'reporting.v1.report_market_bindings.get',
    get_many_report_market_bindings_request: 'reporting.v1.report_market_bindings.get_many',
    put_report_market_binding_request: 'reporting.v1.report_market_bindings.put',
    put_many_report_market_bindings_request: 'reporting.v1.report_market_bindings.put_many',
    delete_report_market_binding_request: 'reporting.v1.report_market_bindings.delete',
    delete_many_report_market_bindings_request: 'reporting.v1.report_market_bindings.delete_many',
    list_report_market_binding_versions_request:
        'reporting.v1.report_market_bindings_versions.list',
    get_report_market_binding_version_request: 'reporting.v1.report_market_bindings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_market_bindings_request: true,
    get_report_market_binding_request: true,
    get_many_report_market_bindings_request: true,
    put_report_market_binding_request: true,
    put_many_report_market_bindings_request: true,
    delete_report_market_binding_request: true,
    delete_many_report_market_bindings_request: true,
    list_report_market_binding_versions_request: true,
    get_report_market_binding_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_market_bindings_events.created',
    updated: 'reporting.v1.report_market_bindings_events.updated',
    deleted: 'reporting.v1.report_market_bindings_events.deleted',
} as const;

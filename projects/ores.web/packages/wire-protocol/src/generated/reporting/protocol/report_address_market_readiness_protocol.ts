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
import type { ReportAddressMarketReadiness } from '../domain/report_address_market_readiness.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportAddressMarketReadinessKey {
    id: string;
}

export interface ReportAddressMarketReadinessWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    oresmd_uri: string;
    series_id: string | null;
    gap_cause: string;
}

export interface ReportAddressMarketReadinessChange {
    write: ReportAddressMarketReadinessWrite;
    precondition: Precondition;
}

export interface ReportAddressMarketReadinessRemoval {
    key: ReportAddressMarketReadinessKey;
    precondition: Precondition;
}

export interface ReportAddressMarketReadinessLookup {
    key: ReportAddressMarketReadinessKey;
    report_address_market_readiness: ReportAddressMarketReadiness | null;
}

export interface ReportAddressMarketReadinessesFilter {
    report_instance_id: string | null;
    oresmd_uri: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    oresmd_uri_one_of: string[] | null;
}

export interface ReportAddressMarketReadinessEvent {
    event_id: string;
    key: ReportAddressMarketReadinessKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportAddressMarketReadinessVersionKey {
    report_address_market_readiness: ReportAddressMarketReadinessKey;
    version: number;
}

export interface ReportAddressMarketReadinessVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportAddressMarketReadinessesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAddressMarketReadinessesFilter | null;
    as_of: string | null;
}

export interface ListReportAddressMarketReadinessesResponse {
    result: Result;
    address_readinesses: ReportAddressMarketReadiness[];
    total: number;
}

export interface GetReportAddressMarketReadinessRequest {
    key: ReportAddressMarketReadinessKey;
}

export interface GetReportAddressMarketReadinessResponse {
    result: Result;
    report_address_market_readiness: ReportAddressMarketReadiness | null;
}

export interface GetManyReportAddressMarketReadinessesRequest {
    keys: ReportAddressMarketReadinessKey[];
}

export interface GetManyReportAddressMarketReadinessesResponse {
    result: Result;
    entries: ReportAddressMarketReadinessLookup[];
}

export interface PutReportAddressMarketReadinessRequest {
    change: ReportAddressMarketReadinessChange;
    intent: ChangeIntent;
}

export interface PutReportAddressMarketReadinessResponse {
    result: Result;
    report_address_market_readiness: ReportAddressMarketReadiness | null;
}

export interface PutManyReportAddressMarketReadinessesRequest {
    changes: ReportAddressMarketReadinessChange[];
    intent: ChangeIntent;
}

export interface PutManyReportAddressMarketReadinessesResponse {
    result: Result;
    address_readinesses: ReportAddressMarketReadiness[];
}

export interface DeleteReportAddressMarketReadinessRequest {
    removal: ReportAddressMarketReadinessRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportAddressMarketReadinessResponse {
    result: Result;
}

export interface DeleteManyReportAddressMarketReadinessesRequest {
    removals: ReportAddressMarketReadinessRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportAddressMarketReadinessesResponse {
    result: Result;
}

export interface ListReportAddressMarketReadinessVersionsRequest {
    key: ReportAddressMarketReadinessKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportAddressMarketReadinessVersionsFilter | null;
}

export interface ListReportAddressMarketReadinessVersionsResponse {
    result: Result;
    versions: ReportAddressMarketReadiness[];
    total: number;
}

export interface GetReportAddressMarketReadinessVersionRequest {
    key: ReportAddressMarketReadinessVersionKey;
}

export interface GetReportAddressMarketReadinessVersionResponse {
    result: Result;
    version: ReportAddressMarketReadiness | null;
}

export const subjects = {
    list_report_address_market_readinesses_request:
        'reporting.v1.report_address_market_readinesses.list',
    get_report_address_market_readiness_request:
        'reporting.v1.report_address_market_readinesses.get',
    get_many_report_address_market_readinesses_request:
        'reporting.v1.report_address_market_readinesses.get_many',
    put_report_address_market_readiness_request:
        'reporting.v1.report_address_market_readinesses.put',
    put_many_report_address_market_readinesses_request:
        'reporting.v1.report_address_market_readinesses.put_many',
    delete_report_address_market_readiness_request:
        'reporting.v1.report_address_market_readinesses.delete',
    delete_many_report_address_market_readinesses_request:
        'reporting.v1.report_address_market_readinesses.delete_many',
    list_report_address_market_readiness_versions_request:
        'reporting.v1.report_address_market_readinesses_versions.list',
    get_report_address_market_readiness_version_request:
        'reporting.v1.report_address_market_readinesses_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_address_market_readinesses_request: true,
    get_report_address_market_readiness_request: true,
    get_many_report_address_market_readinesses_request: true,
    put_report_address_market_readiness_request: true,
    put_many_report_address_market_readinesses_request: true,
    delete_report_address_market_readiness_request: true,
    delete_many_report_address_market_readinesses_request: true,
    list_report_address_market_readiness_versions_request: true,
    get_report_address_market_readiness_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.events.report_address_market_readinesses.created',
    updated: 'reporting.v1.events.report_address_market_readinesses.updated',
    deleted: 'reporting.v1.events.report_address_market_readinesses.deleted',
} as const;

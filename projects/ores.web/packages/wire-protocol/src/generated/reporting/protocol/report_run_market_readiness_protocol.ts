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
import type { ReportRunMarketReadiness } from '../domain/report_run_market_readiness.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportRunMarketReadinessKey {
    id: string;
}

export interface ReportRunMarketReadinessWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    as_of: string;
    booked_count: number;
    complete_count: number;
    excluded_count: number;
}

export interface ReportRunMarketReadinessChange {
    write: ReportRunMarketReadinessWrite;
    precondition: Precondition;
}

export interface ReportRunMarketReadinessRemoval {
    key: ReportRunMarketReadinessKey;
    precondition: Precondition;
}

export interface ReportRunMarketReadinessLookup {
    key: ReportRunMarketReadinessKey;
    report_run_market_readiness: ReportRunMarketReadiness | null;
}

export interface ReportRunMarketReadinessesFilter {
    report_instance_id: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
}

export interface ReportRunMarketReadinessEvent {
    event_id: string;
    key: ReportRunMarketReadinessKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportRunMarketReadinessVersionKey {
    report_run_market_readiness: ReportRunMarketReadinessKey;
    version: number;
}

export interface ReportRunMarketReadinessVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportRunMarketReadinessesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportRunMarketReadinessesFilter | null;
    as_of: string | null;
}

export interface ListReportRunMarketReadinessesResponse {
    result: Result;
    run_readinesses: ReportRunMarketReadiness[];
    total: number;
}

export interface GetReportRunMarketReadinessRequest {
    key: ReportRunMarketReadinessKey;
}

export interface GetReportRunMarketReadinessResponse {
    result: Result;
    report_run_market_readiness: ReportRunMarketReadiness | null;
}

export interface GetManyReportRunMarketReadinessesRequest {
    keys: ReportRunMarketReadinessKey[];
}

export interface GetManyReportRunMarketReadinessesResponse {
    result: Result;
    entries: ReportRunMarketReadinessLookup[];
}

export interface PutReportRunMarketReadinessRequest {
    change: ReportRunMarketReadinessChange;
    intent: ChangeIntent;
}

export interface PutReportRunMarketReadinessResponse {
    result: Result;
    report_run_market_readiness: ReportRunMarketReadiness | null;
}

export interface PutManyReportRunMarketReadinessesRequest {
    changes: ReportRunMarketReadinessChange[];
    intent: ChangeIntent;
}

export interface PutManyReportRunMarketReadinessesResponse {
    result: Result;
    run_readinesses: ReportRunMarketReadiness[];
}

export interface DeleteReportRunMarketReadinessRequest {
    removal: ReportRunMarketReadinessRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportRunMarketReadinessResponse {
    result: Result;
}

export interface DeleteManyReportRunMarketReadinessesRequest {
    removals: ReportRunMarketReadinessRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportRunMarketReadinessesResponse {
    result: Result;
}

export interface ListReportRunMarketReadinessVersionsRequest {
    key: ReportRunMarketReadinessKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportRunMarketReadinessVersionsFilter | null;
}

export interface ListReportRunMarketReadinessVersionsResponse {
    result: Result;
    versions: ReportRunMarketReadiness[];
    total: number;
}

export interface GetReportRunMarketReadinessVersionRequest {
    key: ReportRunMarketReadinessVersionKey;
}

export interface GetReportRunMarketReadinessVersionResponse {
    result: Result;
    version: ReportRunMarketReadiness | null;
}

export const subjects = {
    list_report_run_market_readinesses_request: 'reporting.v1.report_run_market_readinesses.list',
    get_report_run_market_readiness_request: 'reporting.v1.report_run_market_readinesses.get',
    get_many_report_run_market_readinesses_request:
        'reporting.v1.report_run_market_readinesses.get_many',
    put_report_run_market_readiness_request: 'reporting.v1.report_run_market_readinesses.put',
    put_many_report_run_market_readinesses_request:
        'reporting.v1.report_run_market_readinesses.put_many',
    delete_report_run_market_readiness_request: 'reporting.v1.report_run_market_readinesses.delete',
    delete_many_report_run_market_readinesses_request:
        'reporting.v1.report_run_market_readinesses.delete_many',
    list_report_run_market_readiness_versions_request:
        'reporting.v1.report_run_market_readinesses_versions.list',
    get_report_run_market_readiness_version_request:
        'reporting.v1.report_run_market_readinesses_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_run_market_readinesses_request: true,
    get_report_run_market_readiness_request: true,
    get_many_report_run_market_readinesses_request: true,
    put_report_run_market_readiness_request: true,
    put_many_report_run_market_readinesses_request: true,
    delete_report_run_market_readiness_request: true,
    delete_many_report_run_market_readinesses_request: true,
    list_report_run_market_readiness_versions_request: true,
    get_report_run_market_readiness_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.events.report_run_market_readinesses.created',
    updated: 'reporting.v1.events.report_run_market_readinesses.updated',
    deleted: 'reporting.v1.events.report_run_market_readinesses.deleted',
} as const;

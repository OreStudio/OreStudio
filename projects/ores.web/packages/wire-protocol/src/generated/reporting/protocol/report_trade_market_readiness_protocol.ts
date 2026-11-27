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
import type { ReportTradeMarketReadiness } from '../domain/report_trade_market_readiness.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportTradeMarketReadinessKey {
    id: string;
}

export interface ReportTradeMarketReadinessWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    trade_id: string;
    complete: boolean;
    gap_count: number;
}

export interface ReportTradeMarketReadinessChange {
    write: ReportTradeMarketReadinessWrite;
    precondition: Precondition;
}

export interface ReportTradeMarketReadinessRemoval {
    key: ReportTradeMarketReadinessKey;
    precondition: Precondition;
}

export interface ReportTradeMarketReadinessLookup {
    key: ReportTradeMarketReadinessKey;
    report_trade_market_readiness: ReportTradeMarketReadiness | null;
}

export interface ReportTradeMarketReadinessesFilter {
    report_instance_id: string | null;
    trade_id: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    trade_id_one_of: string[] | null;
}

export interface ReportTradeMarketReadinessEvent {
    event_id: string;
    key: ReportTradeMarketReadinessKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportTradeMarketReadinessVersionKey {
    report_trade_market_readiness: ReportTradeMarketReadinessKey;
    version: number;
}

export interface ReportTradeMarketReadinessVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportTradeMarketReadinessesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTradeMarketReadinessesFilter | null;
    as_of: string | null;
}

export interface ListReportTradeMarketReadinessesResponse {
    result: Result;
    trade_readinesses: ReportTradeMarketReadiness[];
    total: number;
}

export interface GetReportTradeMarketReadinessRequest {
    key: ReportTradeMarketReadinessKey;
}

export interface GetReportTradeMarketReadinessResponse {
    result: Result;
    report_trade_market_readiness: ReportTradeMarketReadiness | null;
}

export interface GetManyReportTradeMarketReadinessesRequest {
    keys: ReportTradeMarketReadinessKey[];
}

export interface GetManyReportTradeMarketReadinessesResponse {
    result: Result;
    entries: ReportTradeMarketReadinessLookup[];
}

export interface PutReportTradeMarketReadinessRequest {
    change: ReportTradeMarketReadinessChange;
    intent: ChangeIntent;
}

export interface PutReportTradeMarketReadinessResponse {
    result: Result;
    report_trade_market_readiness: ReportTradeMarketReadiness | null;
}

export interface PutManyReportTradeMarketReadinessesRequest {
    changes: ReportTradeMarketReadinessChange[];
    intent: ChangeIntent;
}

export interface PutManyReportTradeMarketReadinessesResponse {
    result: Result;
    trade_readinesses: ReportTradeMarketReadiness[];
}

export interface DeleteReportTradeMarketReadinessRequest {
    removal: ReportTradeMarketReadinessRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportTradeMarketReadinessResponse {
    result: Result;
}

export interface DeleteManyReportTradeMarketReadinessesRequest {
    removals: ReportTradeMarketReadinessRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportTradeMarketReadinessesResponse {
    result: Result;
}

export interface ListReportTradeMarketReadinessVersionsRequest {
    key: ReportTradeMarketReadinessKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTradeMarketReadinessVersionsFilter | null;
}

export interface ListReportTradeMarketReadinessVersionsResponse {
    result: Result;
    versions: ReportTradeMarketReadiness[];
    total: number;
}

export interface GetReportTradeMarketReadinessVersionRequest {
    key: ReportTradeMarketReadinessVersionKey;
}

export interface GetReportTradeMarketReadinessVersionResponse {
    result: Result;
    version: ReportTradeMarketReadiness | null;
}

export const subjects = {
    list_report_trade_market_readinesses_request:
        'reporting.v1.report_trade_market_readinesses.list',
    get_report_trade_market_readiness_request: 'reporting.v1.report_trade_market_readinesses.get',
    get_many_report_trade_market_readinesses_request:
        'reporting.v1.report_trade_market_readinesses.get_many',
    put_report_trade_market_readiness_request: 'reporting.v1.report_trade_market_readinesses.put',
    put_many_report_trade_market_readinesses_request:
        'reporting.v1.report_trade_market_readinesses.put_many',
    delete_report_trade_market_readiness_request:
        'reporting.v1.report_trade_market_readinesses.delete',
    delete_many_report_trade_market_readinesses_request:
        'reporting.v1.report_trade_market_readinesses.delete_many',
    list_report_trade_market_readiness_versions_request:
        'reporting.v1.report_trade_market_readinesses_versions.list',
    get_report_trade_market_readiness_version_request:
        'reporting.v1.report_trade_market_readinesses_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_trade_market_readinesses_request: true,
    get_report_trade_market_readiness_request: true,
    get_many_report_trade_market_readinesses_request: true,
    put_report_trade_market_readiness_request: true,
    put_many_report_trade_market_readinesses_request: true,
    delete_report_trade_market_readiness_request: true,
    delete_many_report_trade_market_readinesses_request: true,
    list_report_trade_market_readiness_versions_request: true,
    get_report_trade_market_readiness_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.events.report_trade_market_readinesses.created',
    updated: 'reporting.v1.events.report_trade_market_readinesses.updated',
    deleted: 'reporting.v1.events.report_trade_market_readinesses.deleted',
} as const;

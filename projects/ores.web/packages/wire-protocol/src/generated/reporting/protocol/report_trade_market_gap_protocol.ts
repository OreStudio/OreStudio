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
import type { ReportTradeMarketGap } from '../domain/report_trade_market_gap.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportTradeMarketGapKey {
    id: string;
}

export interface ReportTradeMarketGapWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    trade_id: string;
    oresmd_uri: string;
}

export interface ReportTradeMarketGapChange {
    write: ReportTradeMarketGapWrite;
    precondition: Precondition;
}

export interface ReportTradeMarketGapRemoval {
    key: ReportTradeMarketGapKey;
    precondition: Precondition;
}

export interface ReportTradeMarketGapLookup {
    key: ReportTradeMarketGapKey;
    report_trade_market_gap: ReportTradeMarketGap | null;
}

export interface ReportTradeMarketGapsFilter {
    report_instance_id: string | null;
    trade_id: string | null;
    oresmd_uri: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    trade_id_one_of: string[] | null;
    oresmd_uri_one_of: string[] | null;
}

export interface ReportTradeMarketGapEvent {
    event_id: string;
    key: ReportTradeMarketGapKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportTradeMarketGapVersionKey {
    report_trade_market_gap: ReportTradeMarketGapKey;
    version: number;
}

export interface ReportTradeMarketGapVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportTradeMarketGapsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTradeMarketGapsFilter | null;
    as_of: string | null;
}

export interface ListReportTradeMarketGapsResponse {
    result: Result;
    trade_gaps: ReportTradeMarketGap[];
    total: number;
}

export interface GetReportTradeMarketGapRequest {
    key: ReportTradeMarketGapKey;
}

export interface GetReportTradeMarketGapResponse {
    result: Result;
    report_trade_market_gap: ReportTradeMarketGap | null;
}

export interface GetManyReportTradeMarketGapsRequest {
    keys: ReportTradeMarketGapKey[];
}

export interface GetManyReportTradeMarketGapsResponse {
    result: Result;
    entries: ReportTradeMarketGapLookup[];
}

export interface PutReportTradeMarketGapRequest {
    change: ReportTradeMarketGapChange;
    intent: ChangeIntent;
}

export interface PutReportTradeMarketGapResponse {
    result: Result;
    report_trade_market_gap: ReportTradeMarketGap | null;
}

export interface PutManyReportTradeMarketGapsRequest {
    changes: ReportTradeMarketGapChange[];
    intent: ChangeIntent;
}

export interface PutManyReportTradeMarketGapsResponse {
    result: Result;
    trade_gaps: ReportTradeMarketGap[];
}

export interface DeleteReportTradeMarketGapRequest {
    removal: ReportTradeMarketGapRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportTradeMarketGapResponse {
    result: Result;
}

export interface DeleteManyReportTradeMarketGapsRequest {
    removals: ReportTradeMarketGapRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportTradeMarketGapsResponse {
    result: Result;
}

export interface ListReportTradeMarketGapVersionsRequest {
    key: ReportTradeMarketGapKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTradeMarketGapVersionsFilter | null;
}

export interface ListReportTradeMarketGapVersionsResponse {
    result: Result;
    versions: ReportTradeMarketGap[];
    total: number;
}

export interface GetReportTradeMarketGapVersionRequest {
    key: ReportTradeMarketGapVersionKey;
}

export interface GetReportTradeMarketGapVersionResponse {
    result: Result;
    version: ReportTradeMarketGap | null;
}

export const subjects = {
    list_report_trade_market_gaps_request: 'reporting.v1.report_trade_market_gaps.list',
    get_report_trade_market_gap_request: 'reporting.v1.report_trade_market_gaps.get',
    get_many_report_trade_market_gaps_request: 'reporting.v1.report_trade_market_gaps.get_many',
    put_report_trade_market_gap_request: 'reporting.v1.report_trade_market_gaps.put',
    put_many_report_trade_market_gaps_request: 'reporting.v1.report_trade_market_gaps.put_many',
    delete_report_trade_market_gap_request: 'reporting.v1.report_trade_market_gaps.delete',
    delete_many_report_trade_market_gaps_request:
        'reporting.v1.report_trade_market_gaps.delete_many',
    list_report_trade_market_gap_versions_request:
        'reporting.v1.report_trade_market_gaps_versions.list',
    get_report_trade_market_gap_version_request:
        'reporting.v1.report_trade_market_gaps_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_trade_market_gaps_request: true,
    get_report_trade_market_gap_request: true,
    get_many_report_trade_market_gaps_request: true,
    put_report_trade_market_gap_request: true,
    put_many_report_trade_market_gaps_request: true,
    delete_report_trade_market_gap_request: true,
    delete_many_report_trade_market_gaps_request: true,
    list_report_trade_market_gap_versions_request: true,
    get_report_trade_market_gap_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.events.report_trade_market_gaps.created',
    updated: 'reporting.v1.events.report_trade_market_gaps.updated',
    deleted: 'reporting.v1.events.report_trade_market_gaps.deleted',
} as const;

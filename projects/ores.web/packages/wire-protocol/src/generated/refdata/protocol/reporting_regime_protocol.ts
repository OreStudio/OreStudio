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
import type { ReportingRegime } from '../domain/reporting_regime.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportingRegimeKey {
    code: string;
    coding_scheme_code: string;
}

export interface ReportingRegimeWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface ReportingRegimeChange {
    write: ReportingRegimeWrite;
    precondition: Precondition;
}

export interface ReportingRegimeRemoval {
    key: ReportingRegimeKey;
    precondition: Precondition;
}

export interface ReportingRegimeLookup {
    key: ReportingRegimeKey;
    reporting_regime: ReportingRegime | null;
}

export interface ReportingRegimeEvent {
    event_id: string;
    key: ReportingRegimeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportingRegimeVersionKey {
    reporting_regime: ReportingRegimeKey;
    version: number;
}

export interface ReportingRegimeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportingRegimesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListReportingRegimesResponse {
    result: Result;
    regimes: ReportingRegime[];
    total: number;
}

export interface GetReportingRegimeRequest {
    key: ReportingRegimeKey;
}

export interface GetReportingRegimeResponse {
    result: Result;
    reporting_regime: ReportingRegime | null;
}

export interface GetManyReportingRegimesRequest {
    keys: ReportingRegimeKey[];
}

export interface GetManyReportingRegimesResponse {
    result: Result;
    entries: ReportingRegimeLookup[];
}

export interface PutReportingRegimeRequest {
    change: ReportingRegimeChange;
    intent: ChangeIntent;
}

export interface PutReportingRegimeResponse {
    result: Result;
    reporting_regime: ReportingRegime | null;
}

export interface PutManyReportingRegimesRequest {
    changes: ReportingRegimeChange[];
    intent: ChangeIntent;
}

export interface PutManyReportingRegimesResponse {
    result: Result;
    regimes: ReportingRegime[];
}

export interface DeleteReportingRegimeRequest {
    removal: ReportingRegimeRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportingRegimeResponse {
    result: Result;
}

export interface DeleteManyReportingRegimesRequest {
    removals: ReportingRegimeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportingRegimesResponse {
    result: Result;
}

export interface ListReportingRegimeVersionsRequest {
    key: ReportingRegimeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportingRegimeVersionsFilter | null;
}

export interface ListReportingRegimeVersionsResponse {
    result: Result;
    versions: ReportingRegime[];
    total: number;
}

export interface GetReportingRegimeVersionRequest {
    key: ReportingRegimeVersionKey;
}

export interface GetReportingRegimeVersionResponse {
    result: Result;
    version: ReportingRegime | null;
}

export const subjects = {
    list_reporting_regimes_request: 'refdata.v1.reporting_regimes.list',
    get_reporting_regime_request: 'refdata.v1.reporting_regimes.get',
    get_many_reporting_regimes_request: 'refdata.v1.reporting_regimes.get_many',
    put_reporting_regime_request: 'refdata.v1.reporting_regimes.put',
    put_many_reporting_regimes_request: 'refdata.v1.reporting_regimes.put_many',
    delete_reporting_regime_request: 'refdata.v1.reporting_regimes.delete',
    delete_many_reporting_regimes_request: 'refdata.v1.reporting_regimes.delete_many',
    list_reporting_regime_versions_request: 'refdata.v1.reporting_regimes_versions.list',
    get_reporting_regime_version_request: 'refdata.v1.reporting_regimes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_reporting_regimes_request: true,
    get_reporting_regime_request: true,
    get_many_reporting_regimes_request: true,
    put_reporting_regime_request: true,
    put_many_reporting_regimes_request: true,
    delete_reporting_regime_request: true,
    delete_many_reporting_regimes_request: true,
    list_reporting_regime_versions_request: true,
    get_reporting_regime_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.reporting_regimes_events.created',
    updated: 'refdata.v1.reporting_regimes_events.updated',
    deleted: 'refdata.v1.reporting_regimes_events.deleted',
} as const;

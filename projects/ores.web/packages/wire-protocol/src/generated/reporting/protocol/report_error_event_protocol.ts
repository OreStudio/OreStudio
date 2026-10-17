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
import type { ReportErrorEvent } from '../domain/report_error_event.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ReportErrorEventKey {
    id: string;
}

export interface ReportErrorEventLookup {
    key: ReportErrorEventKey;
    report_error_event: ReportErrorEvent | null;
}

export interface ReportErrorEventsFilter {
    report_instance_id: string | null;
    severity: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    severity_one_of: string[] | null;
}

export interface ReportErrorEventEvent {
    event_id: string;
    key: ReportErrorEventKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListReportErrorEventsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportErrorEventsFilter | null;
}

export interface ListReportErrorEventsResponse {
    result: Result;
    errors: ReportErrorEvent[];
    total: number;
}

export interface GetReportErrorEventRequest {
    key: ReportErrorEventKey;
}

export interface GetReportErrorEventResponse {
    result: Result;
    report_error_event: ReportErrorEvent | null;
}

export interface GetManyReportErrorEventsRequest {
    keys: ReportErrorEventKey[];
}

export interface GetManyReportErrorEventsResponse {
    result: Result;
    entries: ReportErrorEventLookup[];
}

export interface ListByReportInstanceIdReportErrorEventsRequest {
    report_instance_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportErrorEventsFilter | null;
}

export interface ListByReportInstanceIdReportErrorEventsResponse {
    result: Result;
    errors: ReportErrorEvent[];
    total: number;
}

export const subjects = {
    list_report_error_events_request: 'reporting.v1.report_error_events.list',
    get_report_error_event_request: 'reporting.v1.report_error_events.get',
    get_many_report_error_events_request: 'reporting.v1.report_error_events.get_many',
    list_by_report_instance_id_report_error_events_request:
        'reporting.v1.report_error_events.list_by_report_instance_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_error_events_request: true,
    get_report_error_event_request: true,
    get_many_report_error_events_request: true,
    list_by_report_instance_id_report_error_events_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_error_events_events.created',
    updated: 'reporting.v1.report_error_events_events.updated',
    deleted: 'reporting.v1.report_error_events_events.deleted',
} as const;

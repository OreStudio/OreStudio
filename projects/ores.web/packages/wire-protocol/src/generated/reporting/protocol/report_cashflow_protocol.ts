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
import type { ReportCashflow } from '../domain/report_cashflow.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportCashflowKey {
    id: string;
}

export interface ReportCashflowWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    family: string;
    subject_type: string;
    subject_id: string;
    scenario: string;
    cashflow_no: string;
    leg_no: string;
    as_of: string;
    measure: string;
    part: string;
    report_definition_id: string;
    pay_date: string | null;
    flow_type: string;
    currency: string;
    qualifiers: string;
    value: number;
    source: string;
}

export interface ReportCashflowChange {
    write: ReportCashflowWrite;
    precondition: Precondition;
}

export interface ReportCashflowRemoval {
    key: ReportCashflowKey;
    precondition: Precondition;
}

export interface ReportCashflowLookup {
    key: ReportCashflowKey;
    report_cashflow: ReportCashflow | null;
}

export interface ReportCashflowsFilter {
    report_instance_id: string | null;
    family: string | null;
    subject_id: string | null;
    measure: string | null;
    part: string | null;
    flow_type: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    family_one_of: string[] | null;
    subject_id_one_of: string[] | null;
    measure_one_of: string[] | null;
    part_one_of: string[] | null;
    flow_type_one_of: string[] | null;
}

export interface ReportCashflowEvent {
    event_id: string;
    key: ReportCashflowKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListReportCashflowsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportCashflowsFilter | null;
    as_of: string | null;
}

export interface ListReportCashflowsResponse {
    result: Result;
    cashflows: ReportCashflow[];
    total: number;
}

export interface GetReportCashflowRequest {
    key: ReportCashflowKey;
}

export interface GetReportCashflowResponse {
    result: Result;
    report_cashflow: ReportCashflow | null;
}

export interface GetManyReportCashflowsRequest {
    keys: ReportCashflowKey[];
}

export interface GetManyReportCashflowsResponse {
    result: Result;
    entries: ReportCashflowLookup[];
}

export interface PutReportCashflowRequest {
    change: ReportCashflowChange;
    intent: ChangeIntent;
}

export interface PutReportCashflowResponse {
    result: Result;
    report_cashflow: ReportCashflow | null;
}

export interface PutManyReportCashflowsRequest {
    changes: ReportCashflowChange[];
    intent: ChangeIntent;
}

export interface PutManyReportCashflowsResponse {
    result: Result;
    cashflows: ReportCashflow[];
}

export interface DeleteReportCashflowRequest {
    removal: ReportCashflowRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportCashflowResponse {
    result: Result;
}

export interface DeleteManyReportCashflowsRequest {
    removals: ReportCashflowRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportCashflowsResponse {
    result: Result;
}

export const subjects = {
    list_report_cashflows_request: 'reporting.v1.report_cashflows.list',
    get_report_cashflow_request: 'reporting.v1.report_cashflows.get',
    get_many_report_cashflows_request: 'reporting.v1.report_cashflows.get_many',
    put_report_cashflow_request: 'reporting.v1.report_cashflows.put',
    put_many_report_cashflows_request: 'reporting.v1.report_cashflows.put_many',
    delete_report_cashflow_request: 'reporting.v1.report_cashflows.delete',
    delete_many_report_cashflows_request: 'reporting.v1.report_cashflows.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_cashflows_request: true,
    get_report_cashflow_request: true,
    get_many_report_cashflows_request: true,
    put_report_cashflow_request: true,
    put_many_report_cashflows_request: true,
    delete_report_cashflow_request: true,
    delete_many_report_cashflows_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_cashflows_events.created',
    updated: 'reporting.v1.report_cashflows_events.updated',
    deleted: 'reporting.v1.report_cashflows_events.deleted',
} as const;

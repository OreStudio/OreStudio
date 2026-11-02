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
import type { ReportTextValue } from '../domain/report_text_value.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportTextValueKey {
    id: string;
}

export interface ReportTextValueWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    result_id: string;
    source: string;
    line: number;
    as_of: string;
    report_definition_id: string;
    family: string;
    subject_type: string;
    subject_id: string;
    name: string;
    value_type: string;
    value: string;
    figure: number | null;
}

export interface ReportTextValueChange {
    write: ReportTextValueWrite;
    precondition: Precondition;
}

export interface ReportTextValueRemoval {
    key: ReportTextValueKey;
    precondition: Precondition;
}

export interface ReportTextValueLookup {
    key: ReportTextValueKey;
    report_text_value: ReportTextValue | null;
}

export interface ReportTextValuesFilter {
    report_instance_id: string | null;
    family: string | null;
    subject_id: string | null;
    name: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    family_one_of: string[] | null;
    subject_id_one_of: string[] | null;
    name_one_of: string[] | null;
}

export interface ReportTextValueEvent {
    event_id: string;
    key: ReportTextValueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListReportTextValuesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportTextValuesFilter | null;
    as_of: string | null;
}

export interface ListReportTextValuesResponse {
    result: Result;
    values: ReportTextValue[];
    total: number;
}

export interface GetReportTextValueRequest {
    key: ReportTextValueKey;
}

export interface GetReportTextValueResponse {
    result: Result;
    report_text_value: ReportTextValue | null;
}

export interface GetManyReportTextValuesRequest {
    keys: ReportTextValueKey[];
}

export interface GetManyReportTextValuesResponse {
    result: Result;
    entries: ReportTextValueLookup[];
}

export interface PutReportTextValueRequest {
    change: ReportTextValueChange;
    intent: ChangeIntent;
}

export interface PutReportTextValueResponse {
    result: Result;
    report_text_value: ReportTextValue | null;
}

export interface PutManyReportTextValuesRequest {
    changes: ReportTextValueChange[];
    intent: ChangeIntent;
}

export interface PutManyReportTextValuesResponse {
    result: Result;
    values: ReportTextValue[];
}

export interface DeleteReportTextValueRequest {
    removal: ReportTextValueRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportTextValueResponse {
    result: Result;
}

export interface DeleteManyReportTextValuesRequest {
    removals: ReportTextValueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportTextValuesResponse {
    result: Result;
}

export const subjects = {
    list_report_text_values_request: 'reporting.v1.report_text_values.list',
    get_report_text_value_request: 'reporting.v1.report_text_values.get',
    get_many_report_text_values_request: 'reporting.v1.report_text_values.get_many',
    put_report_text_value_request: 'reporting.v1.report_text_values.put',
    put_many_report_text_values_request: 'reporting.v1.report_text_values.put_many',
    delete_report_text_value_request: 'reporting.v1.report_text_values.delete',
    delete_many_report_text_values_request: 'reporting.v1.report_text_values.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_text_values_request: true,
    get_report_text_value_request: true,
    get_many_report_text_values_request: true,
    put_report_text_value_request: true,
    put_many_report_text_values_request: true,
    delete_report_text_value_request: true,
    delete_many_report_text_values_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_text_values_events.created',
    updated: 'reporting.v1.report_text_values_events.updated',
    deleted: 'reporting.v1.report_text_values_events.deleted',
} as const;

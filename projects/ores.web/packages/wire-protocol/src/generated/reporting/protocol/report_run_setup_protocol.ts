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
import type { ReportRunSetup } from '../domain/report_run_setup.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportRunSetupKey {
    id: string;
}

export interface ReportRunSetupWrite {
    id: string;
    report_definition_id: string;
    asof_date: string | null;
    accrual_date: string | null;
    input_path: string | null;
    input_path_market: string | null;
    input_path_portfolio: string | null;
    output_path: string | null;
    log_file: string | null;
    log_mask: number | null;
    n_threads: number | null;
    observation_model: string | null;
    base_currency: string | null;
    date_calendar: string | null;
    date_convention: string | null;
    fixing_cutoff: string | null;
    continue_on_error: string | null;
    build_failed_trades: string | null;
    imply_todays_fixings: string | null;
    ignore_fixing_lag: number | null;
    include_todays_cash_flows: string | null;
    include_reference_date_events: string | null;
    lazy_market_building: string | null;
    enrich_index_fixings: string | null;
    use_analytics: string | null;
    csv_comment_report_header: string | null;
    default_mapping_to_identity: string | null;
    portfolio_recurse_into_sub_directories: string | null;
    curve_config_file: string | null;
    conventions_file: string | null;
    market_config_file: string | null;
    pricing_engines_file: string | null;
    pricing_engines_file_scenario: string | null;
    portfolio_file: string | null;
    market_data_file: string | null;
    market_data_mapping_file: string | null;
    fixing_data_file: string | null;
    fixing_data_mapping_file: string | null;
    calendar_adjustment: string | null;
    currency_configuration: string | null;
    reference_data_file: string | null;
    counterparty_file: string | null;
    script_library: string | null;
    ibor_fallback_config: string | null;
    additional_results: string | null;
}

export interface ReportRunSetupChange {
    write: ReportRunSetupWrite;
    precondition: Precondition;
}

export interface ReportRunSetupRemoval {
    key: ReportRunSetupKey;
    precondition: Precondition;
}

export interface ReportRunSetupLookup {
    key: ReportRunSetupKey;
    report_run_setup: ReportRunSetup | null;
}

export interface ReportRunSetupEvent {
    event_id: string;
    key: ReportRunSetupKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportRunSetupVersionKey {
    report_run_setup: ReportRunSetupKey;
    version: number;
}

export interface ReportRunSetupVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportRunSetupsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListReportRunSetupsResponse {
    result: Result;
    setups: ReportRunSetup[];
    total: number;
}

export interface GetReportRunSetupRequest {
    key: ReportRunSetupKey;
}

export interface GetReportRunSetupResponse {
    result: Result;
    report_run_setup: ReportRunSetup | null;
}

export interface GetManyReportRunSetupsRequest {
    keys: ReportRunSetupKey[];
}

export interface GetManyReportRunSetupsResponse {
    result: Result;
    entries: ReportRunSetupLookup[];
}

export interface PutReportRunSetupRequest {
    change: ReportRunSetupChange;
    intent: ChangeIntent;
}

export interface PutReportRunSetupResponse {
    result: Result;
    report_run_setup: ReportRunSetup | null;
}

export interface PutManyReportRunSetupsRequest {
    changes: ReportRunSetupChange[];
    intent: ChangeIntent;
}

export interface PutManyReportRunSetupsResponse {
    result: Result;
    setups: ReportRunSetup[];
}

export interface DeleteReportRunSetupRequest {
    removal: ReportRunSetupRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportRunSetupResponse {
    result: Result;
}

export interface DeleteManyReportRunSetupsRequest {
    removals: ReportRunSetupRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportRunSetupsResponse {
    result: Result;
}

export interface ListReportRunSetupVersionsRequest {
    key: ReportRunSetupKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportRunSetupVersionsFilter | null;
}

export interface ListReportRunSetupVersionsResponse {
    result: Result;
    versions: ReportRunSetup[];
    total: number;
}

export interface GetReportRunSetupVersionRequest {
    key: ReportRunSetupVersionKey;
}

export interface GetReportRunSetupVersionResponse {
    result: Result;
    version: ReportRunSetup | null;
}

export const subjects = {
    list_report_run_setups_request: 'reporting.v1.report_run_setups.list',
    get_report_run_setup_request: 'reporting.v1.report_run_setups.get',
    get_many_report_run_setups_request: 'reporting.v1.report_run_setups.get_many',
    put_report_run_setup_request: 'reporting.v1.report_run_setups.put',
    put_many_report_run_setups_request: 'reporting.v1.report_run_setups.put_many',
    delete_report_run_setup_request: 'reporting.v1.report_run_setups.delete',
    delete_many_report_run_setups_request: 'reporting.v1.report_run_setups.delete_many',
    list_report_run_setup_versions_request: 'reporting.v1.report_run_setups_versions.list',
    get_report_run_setup_version_request: 'reporting.v1.report_run_setups_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_run_setups_request: true,
    get_report_run_setup_request: true,
    get_many_report_run_setups_request: true,
    put_report_run_setup_request: true,
    put_many_report_run_setups_request: true,
    delete_report_run_setup_request: true,
    delete_many_report_run_setups_request: true,
    list_report_run_setup_versions_request: true,
    get_report_run_setup_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_run_setups_events.created',
    updated: 'reporting.v1.report_run_setups_events.updated',
    deleted: 'reporting.v1.report_run_setups_events.deleted',
} as const;

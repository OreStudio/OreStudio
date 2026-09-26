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
import type { Result } from '../../../utility/protocol.js';

export interface TriggerReportInstanceRequest {
    report_definition_id: string;
    tenant_id: string;
    job_instance_id: number;
}

export interface TriggerReportInstanceResponse {
    result: Result;
}

export interface ScheduleReportDefinitionsRequest {
    ids: string[];
}

export interface ScheduleReportDefinitionsResponse {
    success: boolean;
    message: string;
    scheduled_count: number;
    failed_ids: string[];
}

export interface UnscheduleReportDefinitionsRequest {
    ids: string[];
}

export interface UnscheduleReportDefinitionsResponse {
    success: boolean;
    message: string;
    unscheduled_count: number;
    failed_ids: string[];
}

export interface GatherTradesRequest {
    report_instance_id: string;
    definition_id: string;
    tenant_id: string;
    correlation_id: string;
}

export interface GatherTradesResult {
    success: boolean;
    message: string;
    trade_count: number;
    storage_key: string;
}

export interface GatherMarketDataRequest {
    report_instance_id: string;
    definition_id: string;
    tenant_id: string;
    correlation_id: string;
}

export interface GatherMarketDataResult {
    success: boolean;
    message: string;
    series_count: number;
    storage_key: string;
}

export interface AssembleBundleRequest {
    report_instance_id: string;
    definition_id: string;
    tenant_id: string;
    correlation_id: string;
    trades_storage_key: string;
    market_data_storage_key: string;
    trade_count: number;
    series_count: number;
}

export interface AssembleBundleResult {
    success: boolean;
    message: string;
    bundle_id: string;
}

export interface PrepareOrePackageRequest {
    report_instance_id: string;
    bundle_id: string;
    tenant_id: string;
    correlation_id: string;
    trades_storage_key: string;
    market_data_storage_key: string;
}

export interface PrepareOrePackageResult {
    success: boolean;
    message: string;
    tarball_uris: string[];
}

export interface SubmitComputeRequest {
    report_instance_id: string;
    tenant_id: string;
    correlation_id: string;
    tarball_uris: string[];
}

export interface SubmitComputeResult {
    success: boolean;
    message: string;
    batch_id: string;
}

export interface CollectComputeResultsRequest {
    report_instance_id: string;
    tenant_id: string;
    correlation_id: string;
    batch_id: string;
}

export interface CollectComputeResultsResult {
    success: boolean;
    message: string;
}

export interface FinaliseReportRequest {
    report_instance_id: string;
    tenant_id: string;
    correlation_id: string;
}

export interface FinaliseReportResult {
    success: boolean;
    message: string;
}

export interface FailReportRequest {
    report_instance_id: string;
    tenant_id: string;
    correlation_id: string;
    error_message: string;
}

export interface FailReportResult {
    success: boolean;
    message: string;
}

export interface ReportExecutionRequest {
    report_instance_id: string;
    definition_id: string;
    tenant_id: string;
    correlation_id: string;
}

export const subjects = {
    trigger_report_instance_request: "reporting.v1.ops.trigger_report_instance",
    schedule_report_definitions_request: "reporting.v1.report-definitions.schedule",
    unschedule_report_definitions_request: "reporting.v1.report-definitions.unschedule",
    gather_trades_request: "reporting.v1.report.gather-trades",
    gather_market_data_request: "reporting.v1.report.gather-market-data",
    assemble_bundle_request: "reporting.v1.report.assemble-bundle",
    prepare_ore_package_request: "ore.v1.report.prepare-package",
    submit_compute_request: "compute.v1.report.submit",
    collect_compute_results_request: "reporting.v1.report.collect-compute-results",
    finalise_report_request: "reporting.v1.report.finalise",
    fail_report_request: "reporting.v1.report.fail",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    trigger_report_instance_request: true,
    schedule_report_definitions_request: true,
    unschedule_report_definitions_request: true,
    gather_trades_request: true,
    gather_market_data_request: true,
    assemble_bundle_request: true,
    prepare_ore_package_request: true,
    submit_compute_request: true,
    collect_compute_results_request: true,
    finalise_report_request: true,
    fail_report_request: true,
} as const;

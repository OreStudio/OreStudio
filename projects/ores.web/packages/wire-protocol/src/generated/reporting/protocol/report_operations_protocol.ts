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
    kind: string;
}

export interface TriggerReportInstanceResponse {
    result: Result;
    report_instance_id: string;
}

/**
 * @brief Reads one run's health word and the state it was derived from.
 *
 * The run is named rather than scoped to the caller's party: the answer is
 * scoped by row-level security on the tables, as the web's read of the same
 * runs is.
 */
export interface GetRunHealthRequest {
    /** @brief The run to state the health of. */
    report_instance_id: string;
}

export interface GetRunHealthResponse {
    result: Result;
    /**
     * @brief The name of the run's lifecycle state, as the store holds it.
     *
     * Empty when the run has no state, or when the session cannot read the
     * state. The word is then empty too, so a caller is told what the word was
     * derived from rather than left guessing.
     */
    state_name: string;
    /**
     * @brief One of =failed=, =completed with errors=, =running= or
     * =completed clean=, or empty when the state has no word.
     *
     * =never run= is not a word here: a run that never started has no run to
     * ask about.
     */
    health: string;
}

/**
 * @brief Records the date a run priced at, and why it moved when it did.
 *
 * The packaging step is the only place that knows both dates: the run document
 * asks for one, and the gathered market data carries another. The step states
 * the date it used on the run itself, so the screen that shows the run can say
 * which date it priced at without parsing a message. The run is named rather
 * than scoped to the caller's party: the write is scoped by row-level security
 * on the table, as the run reads are.
 */
export interface RecordRunPricingRequest {
    /** @brief The run the date was priced for. */
    report_instance_id: string;
    /**
     * @brief The date the run priced at, as an ISO 8601 date (YYYY-MM-DD).
     *
     * A date and not a timestamp: the engine prices one day, and the time of
     * day the packaging step ran is not part of the answer.
     */
    priced_at: string;
    /**
     * @brief Why the run priced at that date, in the step's own words.
     *
     * Empty when the run priced at the date it asked for.
     */
    price_note: string;
}

export interface RecordRunPricingResponse {
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

/**
 * @brief The workflow step that publishes a DQ-cleared report definitions bundle.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply, so it states a subject and no response. Its body is the DQ artefact
 * the server-side function knows how to expand, which is why it declares no
 * fields. It is declared here, and referenced rather than spelled out, because
 * the SQL function name the handler derives from it depends on the spelling.
 */
export interface PublishReportDefinitionsFromDqRequest {}

/**
 * @brief The workflow step that publishes a DQ-cleared risk report configs bundle.
 *
 * The same trigger shape as publish_report_definitions_from_dq_request, and
 * declared the same way: the SQL function name the handler derives from the
 * subject depends on the spelling.
 */
export interface PublishRiskReportConfigsFromDqRequest {}

/**
 * @brief The workflow step that publishes a DQ-cleared engine settings bundle.
 *
 * The same trigger shape as publish_risk_report_configs_from_dq_request, and
 * declared the same way: the SQL function name the handler derives from the
 * subject depends on the spelling.
 */
export interface PublishEngineSettingsFromDqRequest {}

/**
 * @brief The workflow step that publishes the DQ-cleared ORE parameter value domains.
 *
 * The same trigger shape as publish_engine_settings_from_dq_request, and
 * declared the same way: the SQL function name the handler derives from the
 * subject depends on the spelling.
 */
export interface PublishOreParameterValueDomainsFromDqRequest {}

/**
 * @brief The workflow step that publishes the DQ-cleared ORE parameter documentation.
 *
 * The same trigger shape as publish_engine_settings_from_dq_request. The
 * value domains publish first, because the dataset depends on theirs.
 */
export interface PublishOreParametersFromDqRequest {}

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
    /**
     * @brief The version of each trade the export wrote, as a JSON object keyed by
     * trade id. Empty when the export wrote none.
     *
     * The trades themselves stay in object storage, so only this manifest travels
     * with the run. A command line cannot fill a map, and the run carries the
     * manifest to the bundle unchanged, so the JSON text is what both messages
     * state.
     */
    trade_versions: string;
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
    /**
     * @brief Where the gather step put the fixings body.
     *
     * The engine reads fixings through a file its run document names, and the
     * export writes them apart from the market data, so the run carries the key
     * the packaging step fetches it from.
     */
    fixings_storage_key: string;
}

export interface AssembleBundleRequest {
    report_instance_id: string;
    definition_id: string;
    tenant_id: string;
    correlation_id: string;
    trades_storage_key: string;
    market_data_storage_key: string;
    /**
     * @brief Where the gather step put the fixings body.
     *
     * The engine reads fixings through a file its run document names, and the
     * export writes them apart from the market data, so the run carries the key
     * the packaging step fetches it from.
     */
    fixings_storage_key: string;
    trade_count: number;
    /**
     * @brief The version of each trade the run read, as the JSON object the gather
     * step received. Empty when the run read no trades.
     *
     * The bundle stores it in its own column, because an error the engine raises for
     * a trade names the version of the trade the engine was given.
     */
    trade_versions: string;
    series_count: number;
}

export interface AssembleBundleResult {
    success: boolean;
    message: string;
    bundle_id: string;
}

export interface PrepareOrePackageRequest {
    report_instance_id: string;
    definition_id: string;
    bundle_id: string;
    tenant_id: string;
    party_id: string;
    run_grant_id: string;
    correlation_id: string;
    trades_storage_key: string;
    market_data_storage_key: string;
    /**
     * @brief Where the run's fixings body is, so the package can place it where
     * the engine reads it.
     */
    fixings_storage_key: string;
}

export interface PrepareOrePackageResult {
    success: boolean;
    message: string;
    tarball_uris: string[];
}

export interface SubmitComputeRequest {
    report_instance_id: string;
    tenant_id: string;
    party_id: string;
    run_grant_id: string;
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

export interface PostProcessResultsRequest {
    report_instance_id: string;
    tenant_id: string;
    correlation_id: string;
    batch_id: string;
}

export interface PostProcessResultsResult {
    success: boolean;
    message: string;
    rows_loaded: number;
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
    party_id: string;
    run_grant_id: string;
    correlation_id: string;
    pre_processing: string;
    prepared_input_key: string;
    post_processing: string;
    kind: string | null;
}

export interface ResolvePreparedInputRequest {
    report_instance_id: string;
    tenant_id: string;
    correlation_id: string;
    prepared_input_key: string;
}

export interface IgnoreComputeResultsRequest {
    report_instance_id: string;
    tenant_id: string;
    correlation_id: string;
    batch_id: string;
}

export interface DefinitionNextFire {
    definition_id: string;
    next_fire_at: string | null;
}

export interface GetNextFireRequest {
    definition_ids: string[];
}

export interface GetNextFireResponse {
    result: Result;
    schedules: DefinitionNextFire[];
}

export const subjects = {
    trigger_report_instance_request: 'reporting.v1.ops.trigger_report_instance',
    get_run_health_request: 'reporting.v1.ops.get_run_health',
    record_run_pricing_request: 'reporting.v1.ops.record_run_pricing',
    schedule_report_definitions_request: 'reporting.v1.ops.schedule_report_definitions',
    unschedule_report_definitions_request: 'reporting.v1.ops.unschedule_report_definitions',
    publish_report_definitions_from_dq_request:
        'reporting.v1.ops.publish_report_definitions_from_dq',
    publish_risk_report_configs_from_dq_request:
        'reporting.v1.ops.publish_risk_report_configs_from_dq',
    publish_engine_settings_from_dq_request: 'reporting.v1.ops.publish_engine_settings_from_dq',
    publish_ore_parameter_value_domains_from_dq_request:
        'reporting.v1.ops.publish_ore_parameter_value_domains_from_dq',
    publish_ore_parameters_from_dq_request: 'reporting.v1.ops.publish_ore_parameters_from_dq',
    gather_trades_request: 'reporting.v1.ops.gather_trades',
    gather_market_data_request: 'reporting.v1.ops.gather_market_data',
    assemble_bundle_request: 'reporting.v1.ops.assemble_bundle',
    prepare_ore_package_request: 'ore.v1.ops.prepare_ore_package',
    submit_compute_request: 'compute.v1.ops.submit_compute',
    collect_compute_results_request: 'reporting.v1.ops.collect_compute_results',
    post_process_results_request: 'reporting.v1.ops.post_process_results',
    finalise_report_request: 'reporting.v1.ops.finalise_report',
    fail_report_request: 'reporting.v1.ops.fail_report',
    resolve_prepared_input_request: 'reporting.v1.ops.resolve_prepared_input',
    ignore_compute_results_request: 'reporting.v1.ops.ignore_compute_results',
    get_next_fire_request: 'reporting.v1.ops.get_next_fire',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    trigger_report_instance_request: true,
    get_run_health_request: true,
    record_run_pricing_request: true,
    schedule_report_definitions_request: true,
    unschedule_report_definitions_request: true,
    publish_report_definitions_from_dq_request: true,
    publish_risk_report_configs_from_dq_request: true,
    publish_engine_settings_from_dq_request: true,
    publish_ore_parameter_value_domains_from_dq_request: true,
    publish_ore_parameters_from_dq_request: true,
    gather_trades_request: true,
    gather_market_data_request: true,
    assemble_bundle_request: true,
    prepare_ore_package_request: true,
    submit_compute_request: true,
    collect_compute_results_request: true,
    post_process_results_request: true,
    finalise_report_request: true,
    fail_report_request: true,
    resolve_prepared_input_request: true,
    ignore_compute_results_request: true,
    get_next_fire_request: true,
} as const;

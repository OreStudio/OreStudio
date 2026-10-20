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
import type { ReportInstance } from '../domain/report_instance.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReportInstanceKey {
    name: string;
}

export interface ReportInstanceWrite {
    id: string;
    name: string;
    description: string;
    definition_id: string;
    kind: string;
    run_grant_id: string | null;
    fsm_state_id: string | null;
    trigger_id: string;
    output_message: string;
    output_storage_key: string;
}

export interface ReportInstanceChange {
    write: ReportInstanceWrite;
    precondition: Precondition;
}

export interface ReportInstanceRemoval {
    key: ReportInstanceKey;
    precondition: Precondition;
}

export interface ReportInstanceLookup {
    key: ReportInstanceKey;
    report_instance: ReportInstance | null;
}

export interface ReportInstancesFilter {
    definition_id: string | null;
    kind: string | null;
    id_one_of: string[] | null;
    definition_id_one_of: string[] | null;
    kind_one_of: string[] | null;
}

export interface ReportInstanceEvent {
    event_id: string;
    key: ReportInstanceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReportInstanceVersionKey {
    report_instance: ReportInstanceKey;
    version: number;
}

export interface ReportInstanceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReportInstancesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ReportInstancesFilter | null;
    as_of: string | null;
}

export interface ListReportInstancesResponse {
    result: Result;
    instances: ReportInstance[];
    total: number;
}

export interface GetReportInstanceRequest {
    key: ReportInstanceKey;
}

export interface GetReportInstanceResponse {
    result: Result;
    report_instance: ReportInstance | null;
}

export interface GetManyReportInstancesRequest {
    keys: ReportInstanceKey[];
}

export interface GetManyReportInstancesResponse {
    result: Result;
    entries: ReportInstanceLookup[];
}

export interface PutReportInstanceRequest {
    change: ReportInstanceChange;
    intent: ChangeIntent;
}

export interface PutReportInstanceResponse {
    result: Result;
    report_instance: ReportInstance | null;
}

export interface PutManyReportInstancesRequest {
    changes: ReportInstanceChange[];
    intent: ChangeIntent;
}

export interface PutManyReportInstancesResponse {
    result: Result;
    instances: ReportInstance[];
}

export interface DeleteReportInstanceRequest {
    removal: ReportInstanceRemoval;
    intent: ChangeIntent;
}

export interface DeleteReportInstanceResponse {
    result: Result;
}

export interface DeleteManyReportInstancesRequest {
    removals: ReportInstanceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReportInstancesResponse {
    result: Result;
}

export interface ListReportInstanceVersionsRequest {
    key: ReportInstanceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReportInstanceVersionsFilter | null;
}

export interface ListReportInstanceVersionsResponse {
    result: Result;
    versions: ReportInstance[];
    total: number;
}

export interface GetReportInstanceVersionRequest {
    key: ReportInstanceVersionKey;
}

export interface GetReportInstanceVersionResponse {
    result: Result;
    version: ReportInstance | null;
}

/**
 * @brief Reads the run of each named definition that started last.
 *
 * The caller asks once with the ids a read of the definitions returned, so it
 * pays for one row per definition. The request names no party: the session
 * scopes the answer through the row-level security on the table.
 */
export interface GetLatestReportInstancesByDefinitionRequest {
    /** @brief The definitions to answer for, in any order. */
    definition_ids: string[];
}

export interface GetLatestReportInstancesByDefinitionResponse {
    result: Result;
    /**
     * @brief At most one run per definition asked for, and no entry for a
     * definition that has never run.
     */
    instances: ReportInstance[];
}

export interface ReportInstanceHealth {
    /** @brief The run the word is of. */
    report_instance_id: string;
    /**
     * @brief The name of the run's lifecycle state, as the store holds it.
     *
     * Absent when the run has no state, or when the session cannot read the
     * state: the word is then absent too, and the screen is told what the word
     * was derived from rather than left guessing.
     */
    state_name: string;
    /**
     * @brief One of =failed=, =completed with errors=, =running= or
     * =completed clean=, or empty when the state has no word.
     *
     * =never run= is not a word here: a definition that never ran has no run,
     * so it has no entry in the reply.
     */
    health: string;
}

/**
 * @brief Reads the health word of each named run.
 *
 * The caller asks once with the runs a read of the latest runs returned, so it
 * pays for one row per run. The request names no party: the session scopes the
 * answer through the row-level security on the tables.
 */
export interface GetReportInstanceHealthRequest {
    /** @brief The runs to state the health of, in any order. */
    instance_ids: string[];
}

export interface GetReportInstanceHealthResponse {
    result: Result;
    /**
     * @brief One entry per run the session can read, and no entry for a run
     * that does not exist or is not the caller's to read.
     */
    health: ReportInstanceHealth[];
}

export interface ReportHealthCount {
    /**
     * @brief One of =failed=, =completed with errors=, =running=, =never run=
     * or =completed clean=, or empty for the definitions whose latest run is in
     * a state that has no word yet.
     */
    health: string;
    /** @brief How many of the counted definitions have this word. */
    count: number;
}

/**
 * @brief Counts the definitions at each health word.
 *
 * The request names the same party and search the list of definitions is
 * narrowed by, and the definitions counted are the ones that list would read.
 * It names no tenant: the session scopes the answer through the row-level
 * security on the tables.
 */
export interface CountReportDefinitionsByHealthRequest {
    /** @brief Counts only the definitions of this party, or every party in scope when absent. */
    party_id: string | null;
    /** @brief Counts only the definitions whose name or description holds this text. */
    search: string | null;
}

export interface CountReportDefinitionsByHealthResponse {
    result: Result;
    /**
     * @brief One entry per word, in the order failed, completed with errors,
     * running, never run, completed clean, then the entry with no word. A word
     * no definition holds is stated with a count of zero.
     */
    counts: ReportHealthCount[];
    /** @brief How many definitions were counted, which is the sum of the counts. */
    total: number;
}

export const subjects = {
    list_report_instances_request: 'reporting.v1.report_instances.list',
    get_report_instance_request: 'reporting.v1.report_instances.get',
    get_many_report_instances_request: 'reporting.v1.report_instances.get_many',
    put_report_instance_request: 'reporting.v1.report_instances.put',
    put_many_report_instances_request: 'reporting.v1.report_instances.put_many',
    delete_report_instance_request: 'reporting.v1.report_instances.delete',
    delete_many_report_instances_request: 'reporting.v1.report_instances.delete_many',
    list_report_instance_versions_request: 'reporting.v1.report_instances_versions.list',
    get_report_instance_version_request: 'reporting.v1.report_instances_versions.get',
    get_latest_report_instances_by_definition_request:
        'reporting.v1.ops.get_latest_report_instances_by_definition',
    get_report_instance_health_request: 'reporting.v1.ops.get_report_instance_health',
    count_report_definitions_by_health_request:
        'reporting.v1.ops.count_report_definitions_by_health',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_report_instances_request: true,
    get_report_instance_request: true,
    get_many_report_instances_request: true,
    put_report_instance_request: true,
    put_many_report_instances_request: true,
    delete_report_instance_request: true,
    delete_many_report_instances_request: true,
    list_report_instance_versions_request: true,
    get_report_instance_version_request: true,
    get_latest_report_instances_by_definition_request: true,
    get_report_instance_health_request: true,
    count_report_definitions_by_health_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.report_instances_events.created',
    updated: 'reporting.v1.report_instances_events.updated',
    deleted: 'reporting.v1.report_instances_events.deleted',
} as const;

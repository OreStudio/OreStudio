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
/**
 * @brief Asks for a page of job executions, newest first.
 *
 * Every job the caller's tenant owns is included, so the view answers
 * "what has the scheduler been running" without naming a job first.
 */
export interface GetJobInstancesRequest {
    offset: number;
    limit: number;
}

/**
 * @brief One execution, with its job's display fields folded in.
 *
 * The definition identifier is the link to the job; the name and action type
 * come from the definition row so the caller does not look it up.
 */
export interface JobInstanceSummary {
    /**
     * @brief The execution's storage key.
     */
    id: number;
    /**
     * @brief The job that was executed.
     */
    job_definition_id: string;
    /**
     * @brief The job's name, read from the definition.
     */
    job_name: string;
    /**
     * @brief How the job was executed: =execute_sql= or =nats_publish=.
     */
    action_type: string;
    /**
     * @brief The lifecycle state: =starting=, =succeeded= or =failed=.
     */
    status: string;
    /**
     * @brief When the cron expression fired, in ISO-8601 UTC.
     */
    triggered_at: string;
    /**
     * @brief When execution began, in ISO-8601 UTC.
     */
    started_at: string;
    /**
     * @brief When execution ended, if it has ended.
     */
    completed_at: string | null;
    /**
     * @brief How long execution took in milliseconds, if it has ended.
     */
    duration_ms: number | null;
    /**
     * @brief Why execution failed, when it failed.
     */
    error_message: string;
}

/**
 * @brief The page of executions.
 */
export interface GetJobInstancesResponse {
    success: boolean;
    message: string;
    instances: JobInstanceSummary[];
    total_available_count: number;
}

/**
 * @brief Asks for the live status of every job the caller can see.
 *
 * It carries no fields, because the caller's session decides the scope and
 * the scheduler holds the rest.
 */
export interface GetSchedulerStatusRequest {}

/**
 * @brief One job's schedule and its last run, as the monitor renders it.
 */
export interface JobScheduleStatus {
    job_definition_id: string;
    job_name: string;
    description: string;
    /**
     * @brief The cron expression, as the job stores it.
     */
    schedule_expression: string;
    is_active: boolean;
    /**
     * @brief When the job last started, in ISO-8601 UTC, if it ever has.
     */
    last_run_at: string | null;
    /**
     * @brief The last run's state: =starting=, =succeeded= or =failed=.
     */
    last_run_status: string | null;
    /**
     * @brief When the expression next fires, in ISO-8601 UTC.
     *
     * Absent for a paused job, and absent for an expression the evaluator
     * cannot advance.
     */
    next_fire_at: string | null;
    /**
     * @brief Executions of this job that are still starting.
     */
    running_count: number;
}

/**
 * @brief The status of every job, with the totals the monitor shows.
 */
export interface GetSchedulerStatusResponse {
    success: boolean;
    message: string;
    jobs: JobScheduleStatus[];
    total_running: number;
    total_active: number;
}

export const subjects = {
    get_job_instances_request: 'scheduler.v1.job_instances.list',
    get_scheduler_status_request: 'scheduler.v1.ops.get_scheduler_status',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_job_instances_request: true,
    get_scheduler_status_request: true,
} as const;

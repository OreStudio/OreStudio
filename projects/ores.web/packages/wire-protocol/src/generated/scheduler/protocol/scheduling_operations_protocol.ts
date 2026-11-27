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

/**
 * @brief Asks for every store the installation keeps, with its bound and size.
 *
 * The answer is one row per store and no paging: an installation keeps a dozen
 * stores, and the screen that reads this shows them all at once. The read is the
 * bounds view, so whether a store is bounded is a fact the database states rather
 * than an inference the screen makes.
 */
export interface ListStoreBoundsRequest {}

/**
 * @brief What bounds the store, and whether that mechanism applies today.
 *
 * The mechanism is not the action. A policy may ask for an object expiry the
 * sweep cannot perform, and the store is then not bounded however clear its
 * policy reads.
 */
export interface StoreBoundAction {
    /**
     * @brief What the bound does past its period, when it is a policy.
     */
    action: string | null;
    /**
     * @brief The period the bound states, in days.
     */
    retention_days: number | null;
    /**
     * @brief How many hours the bound waits between two applications of itself.
     *
     * It is the policy's own cadence when one bounds the store and the catalogue's
     * default when none does, so the form that creates a policy opens on the period
     * and the cadence the installation ships with.
     */
    cadence_hours: number | null;
    /**
     * @brief The policy that bounds the store today, if one does.
     */
    policy_code: string | null;
    /**
     * @brief Whether that policy is switched on.
     */
    policy_enabled: boolean;
    /**
     * @brief The version of that policy a write must still match.
     *
     * A change names the version it read, so a period somebody else moved in the
     * meantime is refused rather than overwritten. It is zero when no policy exists,
     * which is what a create claims: nothing is there yet.
     */
    policy_version: number;
    /**
     * @brief Why the store is kept for that period, in the policy's own words.
     *
     * A write carries it back unchanged, because a form that changes a period is not
     * the place that decides why the store is kept. When no policy bounds the store
     * it is the reason the catalogue ships with, so a person sees the words they are
     * about to adopt.
     */
    policy_description: string | null;
    /**
     * @brief What bounds the store: =sweep=, =storage_lifecycle=, =node_release=,
     * =content_keying=, =policy_off= or =none=.
     */
    mechanism: string;
    /**
     * @brief Whether the mechanism the bound names exists and runs today.
     */
    bounded_now: boolean;
}

/**
 * @brief What the bound last did, in its own words.
 *
 * A run that freed nothing is not a failure, and the detail line is what tells
 * the two apart; a screen that dropped it would have to guess.
 */
export interface StoreLastRun {
    /**
     * @brief =applied=, =skipped= or =failed=.
     */
    outcome: string | null;
    /**
     * @brief When it ran, in ISO-8601 UTC.
     */
    ran_at: string | null;
    /**
     * @brief What it removed: chunks, rows or objects.
     */
    removed: number;
    /**
     * @brief The run's own words.
     */
    detail: string | null;
}

/**
 * @brief What the store held when it was last measured.
 *
 * A store nothing measures answers with no sample rather than a size of zero, so
 * the absence of =sampled_at= is what tells an empty store from an unmeasured one.
 */
export interface StoreSize {
    size_bytes: number;
    measure_count: number;
    /**
     * @brief When it was measured, in ISO-8601 UTC.
     */
    sampled_at: string | null;
}

/**
 * @brief One store, with what bounds it, what it last did and how large it is.
 */
export interface StoreBound {
    /**
     * @brief The store's catalogue code.
     */
    code: string;
    /**
     * @brief The name a person reads.
     */
    display_name: string;
    description: string;
    owner_component: string;
    target_kind: string;
    target: string;
    /**
     * @brief The column a row delete compares against the period, where one does.
     *
     * A store bounded by age is bounded on a column of its own, and the column is
     * what a person has to read to know which age the period means. A store the
     * database measures as a whole, or one whose bound lives in another component,
     * names no column.
     */
    time_column: string | null;
    /**
     * @brief What the store is measured in: =relation_rows=, =objects= or =files=.
     *
     * The size is always bytes; this is the other number the sample carries, and
     * without it a count of twenty-seven says nothing about what was counted.
     */
    measure_kind: string | null;
    /**
     * @brief The kind of bound the store ships with: =policy=, =keying= or =none=.
     */
    bound_kind: string;
    bound: StoreBoundAction;
    last_run: StoreLastRun;
    size: StoreSize;
}

/**
 * @brief Every store, in catalogue order.
 */
export interface ListStoreBoundsResponse {
    success: boolean;
    message: string;
    stores: StoreBound[];
}

/**
 * @brief Asks for one store's measurements, oldest first, for the plot.
 */
export interface ListStoreSamplesRequest {
    /**
     * @brief The store whose series the caller wants.
     */
    store_code: string;
    /**
     * @brief How many of the most recent points to return.
     */
    limit: number;
}

/**
 * @brief One measurement of one store.
 */
export interface StoreSample {
    /**
     * @brief When the measurement was taken, in ISO-8601 UTC.
     */
    sampled_at: string;
    size_bytes: number;
    measure_count: number;
}

export interface ListStoreSamplesResponse {
    success: boolean;
    message: string;
    samples: StoreSample[];
}

export const subjects = {
    get_job_instances_request: 'scheduler.v1.job_instances.list',
    get_scheduler_status_request: 'scheduler.v1.ops.get_scheduler_status',
    list_store_bounds_request: 'scheduler.v1.store_bounds.list',
    list_store_samples_request: 'scheduler.v1.store_samples.list',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_job_instances_request: true,
    get_scheduler_status_request: true,
    list_store_bounds_request: true,
    list_store_samples_request: true,
} as const;

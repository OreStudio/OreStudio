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
 * @brief Asks the grid for one unit of work to run.
 *
 * Session gated: the handler checks compute::batches:write before it
 * assigns anything.
 */
export interface PullWorkRequest {
    /**
     * @brief The node that will run the work.
     */
    host_id: string;
}

/**
 * @brief The assignment, or the reason there is none.
 *
 * A successful reply names the result, its workunit and the app version
 * whose package the node needs.
 */
export interface PullWorkResponse {
    /**
     * @brief Whether work was assigned.
     */
    success: boolean;
    /**
     * @brief The result row the node must fill in.
     */
    result_id: string;
    /**
     * @brief The workunit the result belongs to.
     */
    workunit_id: string;
    /**
     * @brief The app version whose cached package the node runs.
     */
    app_version_id: string;
    /**
     * @brief Where the node fetches the job input.
     */
    input_uri: string;
    /**
     * @brief Where the node fetches the job config.
     */
    config_uri: string;
    /**
     * @brief Why no work was assigned, when none was.
     */
    message: string;
}

/**
 * @brief The payload published to the COMPUTE JetStream stream on dispatch.
 *
 * A producer addresses it at the subject below extended with the tenant and
 * the platform code, and carries everything the wrapper needs to execute the
 * job, so no additional round trip to the server is required. A wrapper
 * subscribes only to its own triplet, so it never sees an assignment it
 * could not run.
 */
export interface WorkAssignmentEvent {
    /**
     * @brief The result row the node must fill in.
     */
    result_id: string;
    /**
     * @brief The workunit the result belongs to.
     */
    workunit_id: string;
    /**
     * @brief Identifies the cached package.
     */
    app_version_id: string;
    /**
     * @brief Engine bundle (.tar.gz) to download and cache.
     */
    package_uri: string;
    /**
     * @brief Expected SHA256 of the downloaded package bundle.
     */
    package_sha256: string;
    /**
     * @brief Job input data (HTTP GET).
     */
    input_uri: string;
    /**
     * @brief Job config passed through to the engine (HTTP GET).
     */
    config_uri: string;
    /**
     * @brief Pre-assigned upload location for the result (HTTP PUT).
     */
    output_uri: string;
}

/**
 * @brief A wrapper node's liveness signal.
 *
 * Fire-and-forget and unauthenticated: the handler touches the host row
 * with the service context, and registers the host on its first heartbeat.
 */
export interface HeartbeatMessage {
    /**
     * @brief The node that is alive.
     */
    host_id: string;
}

/**
 * @brief Asks the service to requeue the work of stale hosts.
 *
 * It carries no fields, because the stale threshold is the service's own.
 */
export interface ReapWorkMessage {
}

/**
 * @brief Hands a finished job back from a wrapper node.
 *
 * This is the wrapper-to-service machine channel, kept distinct from the
 * generated save_result CRUD flow: wrapper nodes hold no user JWT, so the
 * user-session-gated save flow rejects them. Like the heartbeat, the submit
 * is trusted at the transport layer.
 */
export interface SubmitResultRequest {
    /**
     * @brief The result row that finished.
     */
    result_id: string;
    /**
     * @brief UUID string of the wrapper node that ran the job.
     */
    host_id: string;
    /**
     * @brief Where the result archive was uploaded.
     */
    output_uri: string;
    /**
     * @brief 1=Success, 3=ClientError, 4=NoReply.
     */
    outcome: number;
    /**
     * @brief Human-readable failure reason; empty on success.
     */
    error_message: string;
}

/**
 * @brief Whether the result was accepted.
 */
export interface SubmitResultResponse {
    /**
     * @brief Whether the result was accepted.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
}

export const subjects = {
    pull_work_request: "compute.v1.work.pull",
    work_assignment_event: "compute.v1.work.assignments",
    heartbeat_message: "compute.v1.work.heartbeat",
    reap_work_message: "compute.v1.work.reap",
    submit_result_request: "compute.v1.results.submit",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    pull_work_request: true,
    work_assignment_event: false,
    heartbeat_message: false,
    reap_work_message: false,
    submit_result_request: false,
} as const;

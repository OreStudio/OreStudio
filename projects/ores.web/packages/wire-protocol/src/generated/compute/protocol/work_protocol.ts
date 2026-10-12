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
     * @brief The tenant that owns the result the node must fill in.
     *
     * The node hands it back on submit. A wrapper fleet is shared across tenants,
     * so the work a node takes does not tell it which tenant it is serving, and
     * the result row is tenant-scoped.
     */
    tenant_id: string;
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
    /**
     * @brief The object capability the node presents to storage.
     *
     * Minted for this assignment alone: it names the package, the input and the
     * output, and nothing else. The node holds no standing storage credential, so
     * a node that serves many tenants reaches only the job it was handed.
     */
    storage_token: string;
}

/**
 * @brief A wrapper node's liveness signal.
 *
 * Fire-and-forget and unauthenticated: the handler registers the host on its
 * first heartbeat with the service context. It also carries what the computer
 * says about its hardware, which the handler writes to the host row only when
 * it differs from the row, so a known host's heartbeat still writes nothing.
 */
export interface HeartbeatMessage {
    /**
     * @brief The node that is alive.
     */
    host_id: string;
    /**
     * @brief Logical processors of the computer, 0 when the node could not read it.
     */
    cpu_count: number;
    /**
     * @brief Physical memory of the computer in MiB, 0 when the node could not read it.
     */
    ram_mb: number;
}

/**
 * @brief Asks the service to requeue the work of stale hosts.
 *
 * It carries no fields, because the stale threshold is the service's own.
 */
export interface ReapWorkMessage {}

/**
 * @brief A free node asks the service for its next job.
 *
 * The node sends it when it has no job to run. The service takes the next
 * waiting job for the node's platform from the work ledger, marks it as taken
 * under a lease, and replies with everything the node needs to run it. A node
 * therefore never holds a job it is not running. Like the heartbeat and the
 * submit, the channel is trusted at the transport layer, because a node holds
 * no user JWT.
 */
export interface ClaimWorkRequest {
    /**
     * @brief The node that asks, as its heartbeat names it.
     */
    host_id: string;
    /**
     * @brief The platform triplet of the node, such as x64-linux. The job named in the reply has a package for it.
     */
    platform: string;
}

/**
 * @brief The job the node is to run, or the fact that none waits.
 *
 * A reply with success and no work says the queue holds nothing for the node's
 * platform, and the node waits for its next poll. A reply with work carries the
 * same fields as a pushed assignment, and the storage token in it is minted at
 * the claim, so it lives from the moment the node takes the job and not from
 * the moment the job was queued.
 */
export interface ClaimWorkResponse {
    /**
     * @brief Whether the claim was served, with or without work.
     */
    success: boolean;
    /**
     * @brief Whether the reply names a job.
     */
    has_work: boolean;
    /**
     * @brief Why the claim was not served, when it was not.
     */
    message: string;
    /**
     * @brief The tenant that owns the result the node must fill in. The node hands it back on submit.
     */
    tenant_id: string;
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
     * @brief Input bundle of the job.
     */
    input_uri: string;
    /**
     * @brief Optional configuration of the job.
     */
    config_uri: string;
    /**
     * @brief Pre-assigned upload location for the result.
     */
    output_uri: string;
    /**
     * @brief A capability for exactly the package, the input and the output of this job, minted at the claim.
     */
    storage_token: string;
    /**
     * @brief The number of the attempt this claim starts, 1 for the first.
     */
    attempt: number;
}

/**
 * @brief Tells idle nodes that work has been queued.
 *
 * A plain NATS notice with no persistence. It holds no job and no credential, so
 * it cannot go stale or be redelivered, and it is lost if no node listens. A node
 * that hears it asks for work at once. A node also asks every few seconds with
 * random jitter, so a lost notice costs seconds and nothing else.
 */
export interface WorkAvailableEvent {
    /**
     * @brief The number of results the sender queued, for a log line. A node does not act on it.
     */
    queued: number;
}

/**
 * @brief Tells the service that a wrapper node has begun a job.
 *
 * The node waits for the reply before it runs the job, so the service has
 * recorded the result as in progress before the node can submit it. Without the
 * wait a fast job could submit first, and the late start would overwrite the
 * finished result. Like the submit, the channel is trusted at the transport
 * layer, because a node holds no user JWT.
 */
export interface ResultStartedRequest {
    /**
     * @brief The tenant the assignment named; the result is scoped to it.
     */
    tenant_id: string;
    /**
     * @brief The result the node has begun.
     */
    result_id: string;
    /**
     * @brief The node that runs it.
     */
    host_id: string;
}

/**
 * @brief Whether the start was recorded.
 *
 * A start for a result that is not unsent is not an error: the result is already
 * further on, and the reply says so in the message with success set.
 */
export interface ResultStartedResponse {
    /**
     * @brief Whether the start was recorded or was already moot.
     */
    success: boolean;
    /**
     * @brief Why it was not recorded, when it was not.
     */
    message: string;
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
     * @brief The tenant that owns the result, echoed from the assignment.
     *
     * The service scopes the write to this tenant, so the node must return the
     * one it was given. Without it the tenant-scoped result row is invisible to
     * the service's own system context.
     */
    tenant_id: string;
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
    pull_work_request: 'compute.v1.ops.pull_work',
    work_assignment_event: 'compute.v1.ops.work_assignment',
    heartbeat_message: 'compute.v1.ops.heartbeat',
    reap_work_message: 'compute.v1.ops.reap_work',
    claim_work_request: 'compute.v1.ops.claim_work',
    work_available_event: 'compute.v1.ops.work_available',
    result_started_request: 'compute.v1.ops.result_started',
    submit_result_request: 'compute.v1.ops.submit_result',
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
    claim_work_request: false,
    work_available_event: false,
    result_started_request: false,
    submit_result_request: false,
} as const;

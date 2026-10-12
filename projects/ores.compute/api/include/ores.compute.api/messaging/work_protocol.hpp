/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_COMPUTE_API_MESSAGING_WORK_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_WORK_PROTOCOL_HPP

#include <string>
#include <string_view>

namespace ores::compute::messaging {

/**
 * @brief The job a node runs, as the wrapper builds it from a claim.
 *
 * It carries everything the wrapper needs to execute the job. It is not
 * published: the claim reply holds the same fields, and the wrapper copies
 * them into this payload to hand them to the code that runs the job.
 */
struct work_assignment_event {
    /**
     * @brief The tenant that owns the result the node must fill in.
     *
     * The node hands it back on submit. A wrapper fleet is shared across tenants,
     * so the work a node takes does not tell it which tenant it is serving, and
     * the result row is tenant-scoped.
     */
    std::string tenant_id;
    /**
     * @brief The result row the node must fill in.
     */
    std::string result_id;
    /**
     * @brief The workunit the result belongs to.
     */
    std::string workunit_id;
    /**
     * @brief Identifies the cached package.
     */
    std::string app_version_id;
    /**
     * @brief Engine bundle (.tar.gz) to download and cache.
     */
    std::string package_uri;
    /**
     * @brief Expected SHA256 of the downloaded package bundle.
     */
    std::string package_sha256;
    /**
     * @brief Job input data (HTTP GET).
     */
    std::string input_uri;
    /**
     * @brief Job config passed through to the engine (HTTP GET).
     */
    std::string config_uri;
    /**
     * @brief Pre-assigned upload location for the result (HTTP PUT).
     */
    std::string output_uri;
    /**
     * @brief The object capability the node presents to storage.
     *
     * Minted for this assignment alone: it names the package, the input and the
     * output, and nothing else. The node holds no standing storage credential, so
     * a node that serves many tenants reaches only the job it was handed.
     */
    std::string storage_token;
};

/**
 * @brief A wrapper node's liveness signal.
 *
 * Fire-and-forget and unauthenticated: the handler registers the host on its
 * first heartbeat with the service context. It also carries what the computer
 * says about its hardware, which the handler writes to the host row only when
 * it differs from the row, so a known host's heartbeat still writes nothing.
 */
struct heartbeat_message {
    static constexpr std::string_view nats_subject = "compute.v1.ops.heartbeat";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /**
     * @brief The node that is alive.
     */
    std::string host_id;
    /**
     * @brief Logical processors of the computer, 0 when the node could not read it.
     */
    int cpu_count = 0;
    /**
     * @brief Physical memory of the computer in MiB, 0 when the node could not read it.
     */
    std::int64_t ram_mb = 0;
};

/**
 * @brief Asks the service to requeue the work of stale hosts.
 *
 * It carries no fields, because the stale threshold is the service's own.
 */
struct reap_work_message {
    static constexpr std::string_view nats_subject = "compute.v1.ops.reap_work";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
};

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
struct claim_work_request {
    using response_type = struct claim_work_response;
    static constexpr std::string_view nats_subject = "compute.v1.ops.claim_work";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /**
     * @brief The node that asks, as its heartbeat names it.
     */
    std::string host_id;
    /**
     * @brief The platform triplet of the node, such as x64-linux. The job named in the reply has a
     * package for it.
     */
    std::string platform;
};

/**
 * @brief The job the node is to run, or the fact that none waits.
 *
 * A reply with success and no work says the queue holds nothing for the node's
 * platform, and the node waits for its next poll. A reply with work carries the
 * same fields as a pushed assignment, and the storage token in it is minted at
 * the claim, so it lives from the moment the node takes the job and not from
 * the moment the job was queued.
 */
struct claim_work_response {
    /**
     * @brief Whether the claim was served, with or without work.
     */
    bool success = false;
    /**
     * @brief Whether the reply names a job.
     */
    bool has_work = false;
    /**
     * @brief Why the claim was not served, when it was not.
     */
    std::string message;
    /**
     * @brief The tenant that owns the result the node must fill in. The node hands it back on
     * submit.
     */
    std::string tenant_id;
    /**
     * @brief The result row the node must fill in.
     */
    std::string result_id;
    /**
     * @brief The workunit the result belongs to.
     */
    std::string workunit_id;
    /**
     * @brief Identifies the cached package.
     */
    std::string app_version_id;
    /**
     * @brief Engine bundle (.tar.gz) to download and cache.
     */
    std::string package_uri;
    /**
     * @brief Expected SHA256 of the downloaded package bundle.
     */
    std::string package_sha256;
    /**
     * @brief Input bundle of the job.
     */
    std::string input_uri;
    /**
     * @brief Optional configuration of the job.
     */
    std::string config_uri;
    /**
     * @brief Pre-assigned upload location for the result.
     */
    std::string output_uri;
    /**
     * @brief A capability for exactly the package, the input and the output of this job, minted at
     * the claim.
     */
    std::string storage_token;
    /**
     * @brief The number of the attempt this claim starts, 1 for the first.
     */
    int attempt = 0;
};

/**
 * @brief Tells idle nodes that work has been queued.
 *
 * A plain NATS notice with no persistence. It holds no job and no credential, so
 * it cannot go stale or be redelivered, and it is lost if no node listens. A node
 * that hears it asks for work at once. A node also asks every few seconds with
 * random jitter, so a lost notice costs seconds and nothing else.
 */
struct work_available_event {
    static constexpr std::string_view nats_subject = "compute.v1.ops.work_available";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /**
     * @brief The number of results the sender queued, for a log line. A node does not act on it.
     */
    int queued = 0;
};

/**
 * @brief Hands a finished job back from a wrapper node.
 *
 * This is the wrapper-to-service machine channel, kept distinct from the
 * generated save_result CRUD flow: wrapper nodes hold no user JWT, so the
 * user-session-gated save flow rejects them. Like the heartbeat, the submit
 * is trusted at the transport layer.
 */
struct submit_result_request {
    using response_type = struct submit_result_response;
    static constexpr std::string_view nats_subject = "compute.v1.ops.submit_result";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /**
     * @brief The tenant that owns the result, echoed from the assignment.
     *
     * The service scopes the write to this tenant, so the node must return the
     * one it was given. Without it the tenant-scoped result row is invisible to
     * the service's own system context.
     */
    std::string tenant_id;
    /**
     * @brief The result row that finished.
     */
    std::string result_id;
    /**
     * @brief UUID string of the wrapper node that ran the job.
     */
    std::string host_id;
    /**
     * @brief Where the result archive was uploaded.
     */
    std::string output_uri;
    /**
     * @brief 1=Success, 3=ClientError, 4=NoReply.
     */
    int outcome = 0;
    /**
     * @brief Human-readable failure reason; empty on success.
     */
    std::string error_message;
};

/**
 * @brief Whether the result was accepted.
 */
struct submit_result_response {
    /**
     * @brief Whether the result was accepted.
     */
    bool success = false;
    /**
     * @brief Why it was not, when it was not.
     */
    std::string message;
};

}

#endif

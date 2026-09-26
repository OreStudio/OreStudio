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
 * @brief Asks the grid for one unit of work to run.
 *
 * Session gated: the handler checks compute::batches:write before it
 * assigns anything.
 */
struct pull_work_request {
    using response_type = struct pull_work_response;
    static constexpr std::string_view nats_subject = "compute.v1.work.pull";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The node that will run the work.
     */
    std::string host_id;
};

/**
 * @brief The assignment, or the reason there is none.
 *
 * A successful reply names the result, its workunit and the app version
 * whose package the node needs.
 */
struct pull_work_response {
    /**
     * @brief Whether work was assigned.
     */
    bool success = false;
    /**
     * @brief The result row the node must fill in.
     */
    std::string result_id;
    /**
     * @brief The workunit the result belongs to.
     */
    std::string workunit_id;
    /**
     * @brief The app version whose cached package the node runs.
     */
    std::string app_version_id;
    /**
     * @brief Where the node fetches the job input.
     */
    std::string input_uri;
    /**
     * @brief Where the node fetches the job config.
     */
    std::string config_uri;
    /**
     * @brief Why no work was assigned, when none was.
     */
    std::string message;
};

/**
 * @brief The payload published to the COMPUTE JetStream stream on dispatch.
 *
 * A producer addresses it at the subject below extended with the tenant and
 * the platform code, and carries everything the wrapper needs to execute the
 * job, so no additional round trip to the server is required. A wrapper
 * subscribes only to its own triplet, so it never sees an assignment it
 * could not run.
 */
struct work_assignment_event {
    static constexpr std::string_view nats_subject = "compute.v1.work.assignments";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
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
};

/**
 * @brief A wrapper node's liveness signal.
 *
 * Fire-and-forget and unauthenticated: the handler touches the host row
 * with the service context, and registers the host on its first heartbeat.
 */
struct heartbeat_message {
    static constexpr std::string_view nats_subject = "compute.v1.work.heartbeat";
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
};

/**
 * @brief Asks the service to requeue the work of stale hosts.
 *
 * It carries no fields, because the stale threshold is the service's own.
 */
struct reap_work_message {
    static constexpr std::string_view nats_subject = "compute.v1.work.reap";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
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
    static constexpr std::string_view nats_subject = "compute.v1.results.submit";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
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

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
#ifndef ORES_IAM_SERVICE_INTERNAL_REQUEST_CLIENT_HPP
#define ORES_IAM_SERVICE_INTERNAL_REQUEST_CLIENT_HPP

#include "ores.iam.core/export.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include <chrono>
#include <cstddef>
#include <functional>
#include <span>
#include <stdexcept>
#include <string>

namespace ores::iam::service {

/**
 * @brief Issues synchronous NATS requests carrying an internally-minted
 * impersonation token, driving the real handler pipeline as a real client
 * would -- see internal_impersonation_service for how the token is minted.
 *
 * A thin wrapper over ores::nats::service::client::request_sync(): attaches
 * the token as a plain Authorization header (not X-Delegated-Authorization
 * -- this request isn't forwarding an inbound caller's identity, it's
 * originating a new one), serialises/deserialises via reflect-cpp exactly
 * like every other request-type pair in this codebase, and throws on
 * transport/parse failure rather than returning std::expected, since a
 * server-side orchestrator has no user to show a graceful error to --
 * letting the exception propagate to the caller's existing top-level
 * try/catch (see tenant_handler.hpp's handlers) is the right failure mode.
 */
class ORES_IAM_CORE_EXPORT internal_request_client {
public:
    /**
     * @param refresh_token Optional. Mints a fresh, short-lived token for
     *        the same impersonated identity. Impersonation tokens are
     *        deliberately short-lived (internal_impersonation_service::
     *        mint_token()'s default TTL) -- fine for a single request, but
     *        this client is also used to drive multi-minute polling loops
     *        (wait_for_workflow_instance()), which will outlive that TTL. Without a refresh
     *        callback, request() throws service_error("token_expired") the
     *        first time that happens; with one, it re-mints and retries
     *        transparently.
     */
    internal_request_client(ores::nats::service::client& nats,
                            std::string token,
                            std::function<std::string()> refresh_token = {});

    /**
     * @brief Thrown when the server rejected the request at the protocol
     * level (an @c X-Error reply header, e.g. an expired/invalid
     * impersonation token) rather than failing to parse or transport the
     * request. Distinct from the generic @c std::runtime_error thrown by
     * request() on transport/parse failure so callers can tell "the server
     * told us why, and retrying with the same token will not help" apart
     * from a transient hiccup worth retrying.
     */
    class service_error : public std::runtime_error {
    public:
        service_error(std::string error_code, std::string_view subject)
            : std::runtime_error("Request to " + std::string(subject) +
                                 " rejected by server: " + error_code)
            , error_code_(std::move(error_code)) {}

        [[nodiscard]] const std::string& error_code() const noexcept {
            return error_code_;
        }

    private:
        std::string error_code_;
    };

    /**
     * @brief Issues @p req and returns the parsed response.
     *
     * On a @c token_expired rejection, re-mints the token via
     * @c refresh_token (if one was supplied) and retries once -- expected,
     * routine behaviour for any call outliving the impersonation token's
     * short TTL, not an error worth surfacing to the caller.
     *
     * @throws service_error if the server rejected the request (an
     *         @c X-Error reply header) and either no @c refresh_token was
     *         supplied, the rejection wasn't @c token_expired, or it was
     *         still rejected after one refreshed retry.
     * @throws std::runtime_error on transport failure or response parse
     *         failure.
     */
    template <typename Request>
    typename Request::response_type
    request(const Request& req, std::chrono::seconds timeout = std::chrono::seconds{30}) {
        const auto& codec = ores::nats::default_wire_codec();
        for (int attempt = 0;; ++attempt) {
            const auto bytes = codec.encode(req);
            const auto reply = nats_.request_sync(
                Request::nats_subject, std::span<const std::byte>(bytes), headers(), timeout);
            if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
                it != reply.headers.end()) {
                if (attempt == 0 && refresh_token_ && it->second == "token_expired") {
                    token_ = refresh_token_();
                    continue;
                }
                throw service_error(it->second, Request::nats_subject);
            }

            auto result = codec.decode<typename Request::response_type>(reply.data);
            if (!result)
                throw std::runtime_error("Failed to parse response to " +
                                         std::string(Request::nats_subject) + ": " +
                                         result.error().what());
            return *result;
        }
    }

    /**
     * @brief Outcome of a workflow-instance wait, carrying the exact reason
     * on failure so an orchestrator can surface it verbatim instead of a
     * generic "workflow did not complete".
     */
    struct workflow_wait_result {
        bool success = false;

        /**
         * @brief Human-readable failure reason: the failed step's error, the
         * instance-level error of a workflow that never started, or the
         * timeout detail with step progress. Empty on success.
         */
        std::string error;
    };

    /**
     * @brief Polls a dq.v1.bundles.publish / dq.v1.datasets.publish workflow
     * instance until it reaches a terminal state, mirroring
     * ores.shell's workflow_commands::wait_for_instance (client-type-coupled,
     * so not directly reusable here).
     *
     * @param on_progress Called with a human-readable line each time a
     *                    step's status changes, so the caller can surface
     *                    step-by-step progress to its own response stream.
     * @return success=false on failure or timeout, with error carrying the
     *         exact reason (details also logged, not thrown -- an
     *         incomplete workflow is a reportable orchestration outcome,
     *         not a transport-level exception).
     */
    workflow_wait_result
    wait_for_workflow_instance(const std::string& instance_id,
                               std::chrono::seconds timeout,
                               std::size_t expected_steps,
                               const std::function<void(const std::string&)>& on_progress = {});

private:
    [[nodiscard]] std::unordered_map<std::string, std::string> headers() const;

    ores::nats::service::client& nats_;
    std::string token_;
    std::function<std::string()> refresh_token_;
};

}

#endif

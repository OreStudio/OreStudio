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
#include "ores.nats/service/client.hpp"
#include <chrono>
#include <cstddef>
#include <functional>
#include <rfl/json.hpp>
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
    internal_request_client(ores::nats::service::client& nats, std::string token);

    /**
     * @brief Issues @p req and returns the parsed response.
     *
     * @throws std::runtime_error on transport failure or response parse
     *         failure.
     */
    template <typename Request>
    typename Request::response_type
    request(const Request& req, std::chrono::seconds timeout = std::chrono::seconds{30}) {
        const auto json = rfl::json::write(req);
        const auto* data = reinterpret_cast<const std::byte*>(json.data());
        const auto reply = nats_.request_sync(Request::nats_subject,
                                              std::span<const std::byte>(data, json.size()),
                                              headers(),
                                              timeout);
        const std::string_view sv(reinterpret_cast<const char*>(reply.data.data()),
                                  reply.data.size());
        auto result = rfl::json::read<typename Request::response_type>(sv);
        if (!result)
            throw std::runtime_error("Failed to parse response to " +
                                     std::string(Request::nats_subject) +
                                     ": " + result.error().what());
        return *result;
    }

    /**
     * @brief Polls a dq.v1.bundles.publish / dq.v1.datasets.publish workflow
     * instance until it reaches a terminal state, mirroring
     * ores.shell's workflow_commands::wait_for_instance (client-type-coupled,
     * so not directly reusable here).
     *
     * @param on_progress Called with a human-readable line each time a
     *                    step's status changes, so the caller can surface
     *                    step-by-step progress to its own response stream.
     * @return true if all expected steps completed; false on failure or
     *         timeout (details logged, not thrown -- an incomplete
     *         workflow is a reportable orchestration outcome, not a
     *         transport-level exception).
     */
    bool wait_for_workflow_instance(const std::string& instance_id,
                                    std::chrono::seconds timeout,
                                    std::size_t expected_steps,
                                    const std::function<void(const std::string&)>& on_progress = {});

private:
    [[nodiscard]] std::unordered_map<std::string, std::string> headers() const;

    ores::nats::service::client& nats_;
    std::string token_;
};

}

#endif

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
#include "ores.iam.core/service/internal_request_client.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"
#include <chrono>
#include <map>
#include <thread>

namespace ores::iam::service {

namespace {
using namespace ores::logging;
inline static std::string_view logger_name = "ores.iam.service.internal_request_client";
auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

constexpr auto poll_interval = std::chrono::milliseconds(500);
constexpr int max_consecutive_poll_failures = 5;
} // namespace

internal_request_client::internal_request_client(ores::nats::service::client& nats,
                                                  std::string token)
    : nats_(nats), token_(std::move(token)) {}

std::unordered_map<std::string, std::string> internal_request_client::headers() const {
    return {{std::string(ores::nats::headers::authorization),
            std::string(ores::nats::headers::bearer_prefix) + token_}};
}

bool internal_request_client::wait_for_workflow_instance(
    const std::string& instance_id,
    std::chrono::seconds timeout,
    std::size_t expected_steps,
    const std::function<void(const std::string&)>& on_progress) {
    BOOST_LOG_SEV(lg(), info) << "Waiting for workflow instance: " << instance_id
                              << " (timeout: " << timeout.count() << "s)";

    const auto deadline = std::chrono::steady_clock::now() + timeout;
    std::map<int, std::string> last_status;
    int consecutive_failures = 0;

    while (true) {
        ores::workflow::messaging::get_workflow_steps_request req;
        req.workflow_instance_id = instance_id;

        ores::workflow::messaging::get_workflow_steps_response result;
        bool ok = true;
        try {
            result = request(req);
            ok = result.success;
        } catch (const std::exception& e) {
            ok = false;
            result.message = e.what();
        }

        if (!ok) {
            ++consecutive_failures;
            BOOST_LOG_SEV(lg(), warn) << "Poll " << consecutive_failures << " failed for "
                                      << instance_id << ": " << result.message;
            if (consecutive_failures >= max_consecutive_poll_failures) {
                BOOST_LOG_SEV(lg(), error) << "Aborting wait after " << max_consecutive_poll_failures
                                          << " consecutive poll failures for " << instance_id;
                return false;
            }
        } else {
            consecutive_failures = 0;

            for (const auto& step : result.steps) {
                auto& last = last_status[step.step_index];
                if (last != step.status) {
                    last = step.status;
                    if (on_progress)
                        on_progress(step.name + ": " + step.status);
                }
            }

            const auto total = result.steps.size();
            std::size_t completed = 0;
            for (const auto& step : result.steps) {
                if (step.status == "failed") {
                    BOOST_LOG_SEV(lg(), error) << "Workflow instance " << instance_id
                                              << " failed at step " << step.step_index << ": "
                                              << step.error;
                    return false;
                }
                if (step.status == "completed" || step.status == "completed_with_warnings")
                    ++completed;
            }
            if (total > 0 && completed == total && total >= expected_steps) {
                BOOST_LOG_SEV(lg(), info) << "Workflow instance " << instance_id << " completed ("
                                         << total << " step(s)).";
                return true;
            }
        }

        if (std::chrono::steady_clock::now() + poll_interval > deadline) {
            BOOST_LOG_SEV(lg(), error) << "Timed out waiting for workflow instance " << instance_id;
            return false;
        }
        std::this_thread::sleep_for(poll_interval);
    }
}

}

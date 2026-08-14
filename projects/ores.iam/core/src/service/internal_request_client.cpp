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
} // namespace

internal_request_client::internal_request_client(ores::nats::service::client& nats,
                                                 std::string token,
                                                 std::function<std::string()> refresh_token)
    : nats_(nats)
    , token_(std::move(token))
    , refresh_token_(std::move(refresh_token)) {}

std::unordered_map<std::string, std::string> internal_request_client::headers() const {
    return {{std::string(ores::nats::headers::authorization),
             std::string(ores::nats::headers::bearer_prefix) + token_}};
}

internal_request_client::workflow_wait_result internal_request_client::wait_for_workflow_instance(
    const std::string& instance_id,
    std::chrono::seconds timeout,
    std::size_t expected_steps,
    const std::function<void(const std::string&)>& on_progress) {
    BOOST_LOG_SEV(lg(), info) << "Waiting for workflow instance: " << instance_id
                              << " (timeout: " << timeout.count() << "s)";

    const auto deadline = std::chrono::steady_clock::now() + timeout;
    std::map<int, std::string> last_status;
    int poll_failures = 0;
    // Instance-level state from the most recent successful poll, used to
    // build the timeout detail.
    std::string last_instance_status;
    std::string last_instance_error;
    std::string last_poll_failure;
    std::size_t last_completed = 0;
    int last_step_count = 0;
    std::string last_in_progress_step;

    while (true) {
        ores::workflow::messaging::get_workflow_steps_request req;
        req.workflow_instance_id = instance_id;

        ores::workflow::messaging::get_workflow_steps_response result;
        bool ok = true;
        try {
            result = request(req);
            ok = result.success;
        } catch (const service_error& e) {
            // The server told us exactly why it rejected the request (e.g.
            // an expired internal impersonation token). That will not
            // change on retry -- unlike a transient transport hiccup, so
            // fail fast instead of burning the rest of the timeout.
            BOOST_LOG_SEV(lg(), error)
                << "Aborting wait for workflow instance " << instance_id << ": " << e.what();
            return {false, std::string(e.what())};
        } catch (const std::exception& e) {
            ok = false;
            result.message = e.what();
        }

        if (!ok) {
            // A busy workflow.service under heavy insert load (e.g. the
            // ~13k-row GLEIF counterparty bundle) can go unresponsive to
            // this query for the bundle's *entire* run, not just a brief
            // blip -- so failures here are not a reason to give up early;
            // the deadline below is the only backstop.
            ++poll_failures;
            last_poll_failure = result.message;
            BOOST_LOG_SEV(lg(), warn) << "Poll " << poll_failures << " failed for " << instance_id
                                      << ": " << result.message;
        } else {
            poll_failures = 0;
            last_poll_failure.clear();
            last_instance_status = result.status;
            last_instance_error = result.error;
            last_step_count = result.step_count;

            for (const auto& step : result.steps) {
                auto& last = last_status[step.step_index];
                if (last != step.status) {
                    last = step.status;
                    if (on_progress)
                        on_progress(step.name + ": " + step.status);
                }
            }

            // An instance the engine failed to start reports failed with no
            // steps; surface the instance-level error before the step scan.
            if (result.status == "failed") {
                const auto msg = result.error.empty() ? std::string("workflow failed") :
                                                        "workflow failed: " + result.error;
                BOOST_LOG_SEV(lg(), error) << "Workflow instance " << instance_id << ": " << msg;
                return {false, msg};
            }

            const auto total = result.steps.size();
            std::size_t completed = 0;
            for (const auto& step : result.steps) {
                if (step.status == "failed") {
                    const auto msg = "workflow failed at step " +
                                     std::to_string(step.step_index + 1) + " (" + step.name +
                                     "): " + step.error;
                    BOOST_LOG_SEV(lg(), error)
                        << "Workflow instance " << instance_id << ": " << msg;
                    return {false, msg};
                }
                if (step.status == "completed" || step.status == "completed_with_warnings")
                    ++completed;
                if (step.status == "in_progress")
                    last_in_progress_step = step.name + " (" + step.status + ")";
            }
            last_completed = completed;
            if (total > 0 && completed == total && total >= expected_steps) {
                BOOST_LOG_SEV(lg(), info) << "Workflow instance " << instance_id << " completed ("
                                          << total << " step(s)).";
                return {true, {}};
            }
        }

        if (std::chrono::steady_clock::now() + poll_interval > deadline) {
            // Build the timeout reason: instance-level progress when any
            // poll succeeded, otherwise the last poll failure (e.g. the
            // workflow never got created at all).
            std::string detail;
            if (!last_instance_status.empty()) {
                detail = "timed out after " + std::to_string(timeout.count()) + "s; " +
                         std::to_string(last_completed) + "/" + std::to_string(last_step_count) +
                         " steps completed";
                if (!last_in_progress_step.empty())
                    detail += "; last step: " + last_in_progress_step;
                if (!last_instance_error.empty())
                    detail += "; instance error: " + last_instance_error;
            } else if (!last_poll_failure.empty()) {
                detail = "instance not found or unqueryable; the workflow did not start (" +
                         last_poll_failure + ")";
            } else {
                detail = "timed out after " + std::to_string(timeout.count()) + "s";
            }
            BOOST_LOG_SEV(lg(), error)
                << "Timed out waiting for workflow instance " << instance_id << ": " << detail;
            return {false, detail};
        }
        std::this_thread::sleep_for(poll_interval);
    }
}

}

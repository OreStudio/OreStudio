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
#include "ores.shell/app/commands/workflow_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"
#include <cli/cli.h>
#include <map>
#include <ostream>
#include <rfl/json.hpp>
#include <thread>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

constexpr std::chrono::seconds poll_interval(3);
constexpr std::chrono::seconds default_timeout(300);

template <typename Response>
std::optional<Response> do_auth_request(std::ostream& out,
                                        nats_client& session,
                                        const std::string& subject,
                                        const std::string& body) {
    try {
        auto reply = session.authenticated_request(subject, body);
        auto data_str =
            std::string(reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            fail(out) << "Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

std::optional<workflow::messaging::get_workflow_steps_response>
fetch_steps(std::ostream& out, nats_client& session, const std::string& instance_id) {
    workflow::messaging::get_workflow_steps_request req;
    req.workflow_instance_id = instance_id;

    auto result = do_auth_request<workflow::messaging::get_workflow_steps_response>(
        out, session, std::string(req.nats_subject), rfl::json::write(req));
    if (!result)
        return std::nullopt;
    if (!result->success) {
        fail(out) << "Failed to fetch workflow steps: " << result->message << std::endl;
        return std::nullopt;
    }
    return result;
}

void print_step(std::ostream& out,
                const workflow::messaging::workflow_step_summary& step,
                std::size_t total) {
    out << "  [" << (step.step_index + 1) << "/" << total << "] " << step.name << ": "
        << step.status;
    if (!step.error.empty())
        out << " — " << step.error;
    out << std::endl;
}

}

void workflow_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto workflow_menu = std::make_unique<cli::Menu>("workflow");

    workflow_menu->Insert(
        "steps",
        [&session](std::ostream& out, std::string instance_id) {
            process_steps(std::ref(out), std::ref(session), instance_id);
        },
        "Show the steps of a workflow instance",
        {"instance_id"});

    workflow_menu->Insert(
        "wait",
        [&session](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(args, {
                {.name = "timeout", .requires_value = true,
                 .default_value = std::to_string(default_timeout.count())}
            });
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Usage: workflow wait <instance_id> [--timeout <seconds>]"
                          << std::endl;
                return;
            }

            std::chrono::seconds timeout(0);
            try {
                timeout = std::chrono::seconds(std::stol(parsed->flag("timeout")));
            } catch (const std::exception&) {
                fail(out) << "Invalid timeout: " << parsed->flag("timeout") << std::endl;
                return;
            }

            wait_for_instance(std::ref(out), std::ref(session),
                              parsed->positionals.front(), timeout);
        },
        "Wait for a workflow instance to reach a terminal state",
        {"instance_id [--timeout <seconds>]"});

    root_menu.Insert(std::move(workflow_menu));
}

void workflow_commands::process_steps(std::ostream& out,
                                      nats_client& session,
                                      const std::string& instance_id) {
    BOOST_LOG_SEV(lg(), debug) << "Fetching steps for workflow instance: " << instance_id;

    auto result = fetch_steps(out, session, instance_id);
    if (!result)
        return;

    if (result->steps.empty()) {
        out << "No steps recorded for instance " << instance_id << "." << std::endl;
        return;
    }

    for (const auto& step : result->steps)
        print_step(out, step, result->steps.size());
}

bool workflow_commands::wait_for_instance(std::ostream& out,
                                          nats_client& session,
                                          const std::string& instance_id,
                                          std::chrono::seconds timeout) {
    BOOST_LOG_SEV(lg(), info) << "Waiting for workflow instance: " << instance_id
                              << " (timeout: " << timeout.count() << "s)";

    const auto deadline = std::chrono::steady_clock::now() + timeout;
    std::map<int, std::string> last_status;

    while (true) {
        auto result = fetch_steps(out, session, instance_id);
        if (!result)
            return false;

        // Print transitions since the previous poll.
        for (const auto& step : result->steps) {
            auto& last = last_status[step.step_index];
            if (last != step.status) {
                last = step.status;
                print_step(out, step, result->steps.size());
            }
        }

        // Terminal-state detection, as the GUI's WorkflowStepsWidget:
        // any failed step is a terminal failure; all steps completed
        // (with or without warnings) is terminal success.
        const auto total = result->steps.size();
        std::size_t completed = 0;
        for (const auto& step : result->steps) {
            if (step.status == "failed") {
                fail(out) << "Workflow failed at step " << (step.step_index + 1) << " of "
                          << total << ": " << step.error << std::endl;
                BOOST_LOG_SEV(lg(), error) << "Workflow instance " << instance_id
                                           << " failed at step " << step.step_index << ": "
                                           << step.error;
                return false;
            }
            if (step.status == "completed" || step.status == "completed_with_warnings")
                ++completed;
        }
        if (total > 0 && completed == total) {
            out << "✓ All " << total << " step(s) completed." << std::endl;
            BOOST_LOG_SEV(lg(), info) << "Workflow instance " << instance_id << " completed.";
            return true;
        }

        if (std::chrono::steady_clock::now() + poll_interval > deadline) {
            fail(out) << "Timed out after " << timeout.count()
                      << "s waiting for workflow instance " << instance_id
                      << ". Check progress with: workflow steps " << instance_id << std::endl;
            BOOST_LOG_SEV(lg(), error) << "Timed out waiting for workflow instance "
                                       << instance_id;
            return false;
        }
        std::this_thread::sleep_for(poll_interval);
    }
}

}

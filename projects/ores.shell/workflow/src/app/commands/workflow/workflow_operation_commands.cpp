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
#include "ores.shell/app/commands/workflow/workflow_operation_commands.hpp"
#include "ores.nats/service/request_helpers.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <expected>
#include <map>
#include <ostream>
#include <thread>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

constexpr std::chrono::seconds poll_interval(3);
constexpr std::chrono::seconds default_timeout(300);
constexpr int max_consecutive_poll_failures = 5;

/**
 * @brief Fetch the steps of an instance without reporting failure.
 *
 * Transport and parse errors are returned as the error string so the
 * caller decides whether they are fatal. The polling loop tolerates a few
 * in a row, so it cannot use the shell's do_request helpers, which report
 * every failure as a command failure. A server response with success ==
 * false is a definitive answer, also left to the caller.
 */
std::expected<workflow::messaging::get_workflow_steps_response, std::string>
fetch_steps(nats_client& session, const std::string& instance_id) {
    workflow::messaging::get_workflow_steps_request req;
    req.workflow_instance_id = instance_id;

    try {
        auto result = ores::nats::service::authenticated_request_and_decode<
            workflow::messaging::get_workflow_steps_response>(
            session, std::string(req.nats_subject), req);
        if (!result)
            return std::unexpected("Failed to parse response: " + result.error().what());
        return *result;
    } catch (const std::exception& e) {
        return std::unexpected(e.what());
    }
}

/**
 * @brief Print the entries a step handler recorded while it ran.
 *
 * A step can report success and still have skipped work: the import
 * handler marks itself completed_with_warnings and puts the reason in its
 * log rather than in the step error, so a reader that prints only the
 * status cannot tell a clean run from a lossy one.
 */
void print_step_log(std::ostream& out, const workflow::messaging::workflow_step_summary& step) {
    for (const auto& entry : step.log) {
        out << "      " << workflow::messaging::to_string(entry.level) << ": " << entry.message;
        if (!entry.context.empty())
            out << " [" << entry.context << "]";
        out << std::endl;
    }
}

void print_step(std::ostream& out,
                const workflow::messaging::workflow_step_summary& step,
                std::size_t total) {
    out << "  [" << (step.step_index + 1) << "/" << total << "] " << step.name << ": "
        << step.status;
    if (!step.error.empty())
        out << " — " << step.error;
    out << std::endl;
    print_step_log(out, step);
}

}

void workflow_operation_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto workflow_menu = std::make_unique<cli::Menu>("workflow");

    workflow_menu->Insert(
        "wait",
        [&session](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(
                args,
                {{.name = "timeout",
                  .requires_value = true,
                  .default_value = std::to_string(default_timeout.count())},
                 {.name = "expect-steps", .requires_value = true, .default_value = "0"}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Usage: workflow wait <instance_id> [--timeout <seconds>] "
                             "[--expect-steps <n>]"
                          << std::endl;
                return;
            }

            auto timeout = parse_positive_seconds(parsed->flag("timeout"));
            if (!timeout) {
                fail(out) << "Timeout must be a positive number of seconds: "
                          << parsed->flag("timeout") << std::endl;
                return;
            }

            auto expected = parse_uint32(parsed->flag("expect-steps"));
            if (!expected) {
                fail(out) << "Flag --expect-steps must be an unsigned integer: "
                          << parsed->flag("expect-steps") << std::endl;
                return;
            }

            wait_for_instance(
                std::ref(out), std::ref(session), parsed->positionals.front(), *timeout, *expected);
        },
        "Wait for a workflow instance to reach a terminal state",
        {"instance_id [--timeout <seconds>] [--expect-steps <n>]"});

    workflow_menu->Insert(
        "definitions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_definitions(std::ref(out), std::ref(session), args);
        },
        "List the workflow types the service has registered",
        {});

    workflow_menu->Insert(
        "start",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_start(std::ref(out), std::ref(session), args);
        },
        "Start a workflow and print the instance id to follow",
        {"<type> <request_json>"});

    root_menu.Insert(std::move(workflow_menu));
}

bool workflow_operation_commands::wait_for_instance(std::ostream& out,
                                               nats_client& session,
                                               const std::string& instance_id,
                                               std::chrono::seconds timeout,
                                               std::size_t expected_steps) {
    BOOST_LOG_SEV(lg(), info) << "Waiting for workflow instance: " << instance_id
                              << " (timeout: " << timeout.count() << "s)";

    const auto deadline = std::chrono::steady_clock::now() + timeout;
    std::map<int, std::string> last_status;
    int consecutive_failures = 0;

    while (true) {
        auto result = fetch_steps(session, instance_id);
        if (!result || !result->success) {
            // Tolerated for a few polls: transport and parse errors, which a
            // long wait routinely survives, and unsuccessful replies, because
            // an instance is not queryable the instant it is dispatched, so
            // even "not found" is transient. The warning deliberately avoids
            // fail(): were the wait to recover and succeed, an earlier mark
            // would still abort a load script.
            const auto reason = !result ? result.error() : result->message;
            if (result)
                BOOST_LOG_SEV(lg(), info)
                    << "Treating unsuccessful steps reply as transient: " << reason;
            ++consecutive_failures;
            out << "⚠ Poll failed (" << consecutive_failures << "/" << max_consecutive_poll_failures
                << "): " << reason << std::endl;
            BOOST_LOG_SEV(lg(), warn) << "Poll " << consecutive_failures << " failed for "
                                      << instance_id << ": " << reason;
            if (consecutive_failures >= max_consecutive_poll_failures) {
                fail(out) << "Aborting wait after " << max_consecutive_poll_failures
                          << " consecutive poll failures." << std::endl;
                return false;
            }
        } else {
            consecutive_failures = 0;

            // Print transitions since the previous poll.
            for (const auto& step : result->steps) {
                auto& last = last_status[step.step_index];
                if (last != step.status) {
                    last = step.status;
                    print_step(out, step, result->steps.size());
                }
            }

            // Any failed step is a terminal failure; all steps completed,
            // with or without warnings, is terminal success.
            const auto total = result->steps.size();
            std::size_t completed = 0;
            for (const auto& step : result->steps) {
                if (step.status == "failed") {
                    fail(out) << "Workflow failed at step " << (step.step_index + 1) << " of "
                              << total << ": " << step.error << std::endl;
                    BOOST_LOG_SEV(lg(), error)
                        << "Workflow instance " << instance_id << " failed at step "
                        << step.step_index << ": " << step.error;
                    return false;
                }
                if (step.status == "completed" || step.status == "completed_with_warnings")
                    ++completed;
            }
            if (total > 0 && completed == total && total < expected_steps) {
                BOOST_LOG_SEV(lg(), debug)
                    << "All visible steps complete but more expected: " << total << "/"
                    << expected_steps;
            }
            if (total > 0 && completed == total && total >= expected_steps) {
                out << "✓ All " << total << " step(s) completed." << std::endl;
                BOOST_LOG_SEV(lg(), info) << "Workflow instance " << instance_id << " completed.";
                return true;
            }
        }

        if (std::chrono::steady_clock::now() + poll_interval > deadline) {
            fail(out) << "Timed out after " << timeout.count() << "s waiting for workflow instance "
                      << instance_id << ". Check progress with: workflow_steps by-workflow-id "
                      << instance_id << std::endl;
            BOOST_LOG_SEV(lg(), error) << "Timed out waiting for workflow instance " << instance_id;
            return false;
        }
        std::this_thread::sleep_for(poll_interval);
    }
}

void workflow_operation_commands::process_definitions(std::ostream& out,
                                                      nats_client& session,
                                                      const std::vector<std::string>& args) {
    (void)args;
    BOOST_LOG_SEV(lg(), debug) << "Listing workflow definitions.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to list workflow definitions." << std::endl;
        return;
    }

    workflow::messaging::list_workflow_definitions_request req;
    auto result = do_auth_request<workflow::messaging::list_workflow_definitions_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    if (!result->success) {
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->definitions.empty()) {
        out << "No workflow definitions are registered." << std::endl;
        return;
    }

    for (const auto& def : result->definitions) {
        out << def.type_name << "  (" << def.step_count << " step(s))" << std::endl;
        if (!def.description.empty())
            out << "  " << def.description << std::endl;
    }
}

void workflow_operation_commands::process_start(std::ostream& out,
                                               nats_client& session,
                                               const std::vector<std::string>& args) {
    if (args.size() != 2) {
        fail(out) << "Usage: workflow start <type> <request_json>" << std::endl;
        fail(out) << "Wrap the request in single quotes. The command line reads a double quote "
                     "as a quote character, so unquoted JSON loses its own."
                  << std::endl;
        fail(out) << "For example: workflow start identity_workflow "
                     "'{\"steps\":[{\"name\":\"a\"}]}'"
                  << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to start a workflow." << std::endl;
        return;
    }

    const auto& type = args[0];
    const auto& request_json = args[1];

    BOOST_LOG_SEV(lg(), info) << "Starting workflow of type: " << type;

    // Generated here so the caller can follow the run: the engine acknowledges
    // the start rather than answering with the result.
    boost::uuids::random_generator rng;
    const auto instance_id = boost::uuids::to_string(rng());

    workflow::messaging::start_workflow_message msg;
    msg.type = type;
    msg.tenant_id = session.auth().tenant_id;
    msg.request_json = request_json;
    msg.instance_id = instance_id;

    try {
        session.transport().js_publish(
            workflow::messaging::start_workflow_message::nats_subject,
            ores::nats::default_wire_codec().encode(msg));
    } catch (const std::exception& e) {
        fail(out) << "Failed to start the workflow: " << e.what() << std::endl;
        return;
    }

    out << "Dispatched " << type << "." << std::endl;
    out << "workflow_instance_id: " << instance_id << std::endl;
    out << "Follow progress with: workflow wait " << instance_id << std::endl;
}

}

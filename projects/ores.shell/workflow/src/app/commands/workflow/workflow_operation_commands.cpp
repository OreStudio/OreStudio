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
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.nats/service/request_helpers.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cli/cli.h>
#include <expected>
#include <map>
#include <optional>
#include <ostream>
#include <ranges>
#include <sstream>
#include <thread>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

constexpr std::chrono::seconds poll_interval(3);
constexpr std::chrono::seconds default_timeout(300);
constexpr int max_consecutive_poll_failures = 5;

/// How long a start waits for the engine to create its instance before
/// reporting that the request was refused.
constexpr std::chrono::seconds acceptance_timeout(5);
constexpr std::chrono::milliseconds acceptance_poll(100);

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

/**
 * @brief The canonical spelling of a UUID, or nothing when the value is not one.
 *
 * A supplied instance id is how a caller addresses a run it may already have
 * asked for, so the text is parsed rather than trusted. Parsing buys more than a
 * rejection: the canonical spelling is what gets echoed and dispatched, so a
 * caller who wrote the id with braces or without dashes still reads back the id
 * the engine holds; and the nil UUID is refused, because it parses but every nil
 * run would be the same run, so a placeholder id would quietly collapse
 * unrelated workflows into one.
 */
std::optional<std::string> canonical_uuid(const std::string& value) {
    try {
        const auto parsed = boost::uuids::string_generator()(value);
        if (parsed.is_nil())
            return std::nullopt;
        return boost::uuids::to_string(parsed);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

}

void workflow_operation_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto workflow_menu = std::make_unique<cli::Menu>("workflow");

    workflow_menu->Insert(
        "wait",
        [&session](std::ostream& out, std::vector<std::string> args) {
            auto parsed =
                parse_args(args,
                           {{.name = "timeout",
                             .requires_value = true,
                             .default_value = std::to_string(default_timeout.count())},
                            {.name = "expect-steps", .requires_value = true, .default_value = "0"},
                            {.name = "expect-state", .requires_value = true, .default_value = ""}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Usage: workflow wait <instance_id> [--timeout <seconds>] "
                             "[--expect-steps <n>] [--expect-state <state>]"
                          << std::endl;
                fail(out) << "Without --expect-state the wait asserts that every step completed. "
                             "With it, it asserts the instance's own terminal state, which is "
                             "what a run that is meant to fail has to assert: completed, failed "
                             "or compensated."
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

            const auto& expected_state = parsed->flag("expect-state");
            if (!expected_state.empty() && expected_state != "completed" &&
                expected_state != "failed" && expected_state != "compensated") {
                fail(out) << "--expect-state must be completed, failed or compensated: "
                          << expected_state << std::endl;
                return;
            }

            wait_for_instance(std::ref(out),
                              std::ref(session),
                              parsed->positionals.front(),
                              *timeout,
                              *expected,
                              expected_state);
        },
        "Wait for a workflow instance to reach a terminal state",
        {"instance_id [--timeout <seconds>] [--expect-steps <n>] [--expect-state <state>]"});

    workflow_menu->Insert("definitions",
                          [&session](std::ostream& out, std::vector<std::string> args) {
                              process_definitions(std::ref(out), std::ref(session), args);
                          },
                          "List the workflow types the service has registered",
                          {});

    workflow_menu->Insert("start",
                          [&session](std::ostream& out, std::vector<std::string> args) {
                              process_start(std::ref(out), std::ref(session), args);
                          },
                          "Start a workflow and print the instance id to follow",
                          {"<type> <request_json> [--instance-id <uuid>]"});

    root_menu.Insert(std::move(workflow_menu));
}

bool workflow_operation_commands::wait_for_instance(std::ostream& out,
                                                    nats_client& session,
                                                    const std::string& instance_id,
                                                    std::chrono::seconds timeout,
                                                    std::size_t expected_steps,
                                                    const std::string& expected_state) {
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

            // An expected state asserts something about the instance rather
            // than about its steps. A run that is meant to fail never completes
            // its steps, so "all steps completed" cannot be what a script for
            // that path waits for, and without this the only possible outcome
            // was an abort that could not be told from a broken script.
            if (!expected_state.empty()) {
                if (result->status == expected_state) {
                    out << "✓ Workflow instance " << instance_id << " reached " << expected_state
                        << "." << std::endl;
                    return true;
                }
                if (result->status == "completed" || result->status == "failed" ||
                    result->status == "compensated") {
                    fail(out) << "Workflow instance " << instance_id << " reached "
                              << result->status << ", not " << expected_state << "." << std::endl;
                    return false;
                }
                // Anything else is still running: keep polling.
            }

            // Print transitions since the previous poll.
            for (const auto& step : result->steps) {
                auto& last = last_status[step.step_index];
                if (last != step.status) {
                    last = step.status;
                    print_step(out, step, result->steps.size());
                }
            }

            // Any failed step is a terminal failure; all steps completed,
            // with or without warnings, is terminal success. Neither holds when
            // the caller named the state it expects: a run on its way to
            // compensated has a failed step by definition, and treating that as
            // the verdict would abort before the state it is headed for.
            if (expected_state.empty()) {
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
                    BOOST_LOG_SEV(lg(), info)
                        << "Workflow instance " << instance_id << " completed.";
                    return true;
                }
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
    auto parsed = parse_args(args, {{.name = "instance-id", .requires_value = true}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    if (parsed->positionals.size() != 2) {
        fail(out) << "Usage: workflow start <type> <request_json> [--instance-id <uuid>]"
                  << std::endl;
        fail(out) << "Wrap the request in single quotes. The command line reads a double quote "
                     "as a quote character, so unquoted JSON loses its own."
                  << std::endl;
        fail(out) << "For example: workflow start identity_workflow "
                     "'{\"steps\":[{\"name\":\"a\"}]}'"
                  << std::endl;
        return;
    }

    const auto& type = parsed->positionals[0];
    const auto& request_json = parsed->positionals[1];
    const auto& supplied_id = parsed->flag("instance-id");

    // A supplied id is what makes a start repeatable: a script that runs twice
    // asks for the same run rather than for a second one. Left out, the id is
    // minted here so the caller can follow it, as the engine would otherwise
    // keep the one it made to itself.
    std::string instance_id;
    if (supplied_id.empty()) {
        boost::uuids::random_generator rng;
        instance_id = boost::uuids::to_string(rng());
    } else if (const auto canonical = canonical_uuid(supplied_id)) {
        instance_id = *canonical;
    } else {
        fail(out) << "--instance-id must be a UUID and not the nil UUID: " << supplied_id
                  << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to start a workflow." << std::endl;
        return;
    }

    // With a supplied id the run can be looked for before it is asked for, so a
    // repeat reports the run that exists instead of dispatching a request the
    // engine would only have to refuse.
    if (!supplied_id.empty()) {
        auto existing = fetch_steps(session, instance_id);
        if (existing && existing->success) {
            out << "Workflow instance " << instance_id << " already exists (" << existing->status
                << "); nothing was started." << std::endl;
            out << "Follow progress with: workflow wait " << instance_id << std::endl;
            return;
        }
    }

    // Refuse a type nobody registered before dispatching it. The engine only
    // discovers that when it reads the message, so without this the command
    // printed an instance id to follow for a run that would never exist.
    workflow::messaging::list_workflow_definitions_request definitions_request;
    auto definitions = do_auth_request<workflow::messaging::list_workflow_definitions_response>(
        out, session, std::string(definitions_request.nats_subject), definitions_request);
    if (!definitions)
        return;
    if (!definitions->success) {
        fail(out) << definitions->message << std::endl;
        return;
    }

    const auto known = std::ranges::any_of(definitions->definitions,
                                           [&type](const auto& d) { return d.type_name == type; });
    if (!known) {
        fail(out) << "No workflow type named '" << type << "' is registered." << std::endl;
        if (!definitions->definitions.empty()) {
            // Built as one string: fail() marks each insertion, so a type per
            // call would print the marker between every name.
            std::ostringstream registered;
            for (const auto& d : definitions->definitions)
                registered << " " << d.type_name;
            fail(out) << "Registered types:" << registered.str() << std::endl;
        }
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Starting workflow of type: " << type;

    workflow::messaging::start_workflow_message msg;
    msg.type = type;
    msg.tenant_id = session.auth().tenant_id;
    msg.request_json = request_json;
    msg.instance_id = instance_id;

    try {
        // The engine attributes the run to whoever the message names, and the
        // raw transport carries no headers of its own, so the logged-in user's
        // token goes with the request.
        session.transport().js_publish(
            workflow::messaging::start_workflow_message::nats_subject,
            ores::nats::default_wire_codec().encode(msg),
            ores::nats::service::forwarded_caller_headers(session.auth().jwt));
    } catch (const std::exception& e) {
        fail(out) << "Failed to start the workflow: " << e.what() << std::endl;
        return;
    }

    // The engine acknowledges nothing, so acceptance is confirmed by the row it
    // creates. Without this the command reported success for a request the
    // engine had refused -- a type it did not know, or a definition that built
    // no steps -- and the caller waited on an instance that never existed.
    const auto deadline = std::chrono::steady_clock::now() + acceptance_timeout;
    while (std::chrono::steady_clock::now() < deadline) {
        auto accepted = fetch_steps(session, instance_id);
        if (accepted && accepted->success) {
            out << "Started " << type << "." << std::endl;
            out << "workflow_instance_id: " << instance_id << std::endl;
            out << "Follow progress with: workflow wait " << instance_id << std::endl;
            return;
        }
        std::this_thread::sleep_for(acceptance_poll);
    }

    fail(out) << "The engine did not accept " << type << " for instance " << instance_id
              << " within " << acceptance_timeout.count() << "s." << std::endl;
    fail(out) << "Nothing is following it. The workflow service log says why it was refused."
              << std::endl;
}

}

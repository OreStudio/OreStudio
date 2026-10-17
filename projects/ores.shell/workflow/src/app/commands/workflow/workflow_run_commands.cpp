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
#include "ores.shell/app/commands/workflow/workflow_run_commands.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.nats/service/request_helpers.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
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
#include <rfl/json.hpp>
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

/// The fields a step's result carries when it starts the run that finishes its
/// work. A step that hands nothing on leaves both out.
struct step_result_handoff {
    std::optional<std::string> child_instance_id;
    std::optional<std::string> tenant_id;
};

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
 * @brief Fetch one step's recorded result without reporting failure.
 *
 * Returns the error string the transport or the decoder gave, so the caller
 * can report why the result is unknown. A server reply is always returned,
 * whatever its found flag says: only the caller knows what a missing result
 * means for the question it is asking.
 */
std::expected<workflow::messaging::get_step_result_response, std::string>
fetch_step_result(nats_client& session, const std::string& step_id, const std::string& tenant_id) {
    workflow::messaging::get_step_result_request req;
    req.step_id = step_id;
    req.tenant_id = tenant_id;

    try {
        auto result = ores::nats::service::authenticated_request_and_decode<
            workflow::messaging::get_step_result_response>(
            session, std::string(req.nats_subject), req);
        if (!result)
            return std::unexpected("Failed to parse the step result: " + result.error().what());
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

void workflow_run_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    // The workflow menu belongs to the generated operations unit. These two verbs
    // are not messages, so they join that menu rather than claim one.
    ores::shell::app::extend_menu(root_menu, "workflow", [&session](cli::Menu& workflow_menu) {
        workflow_menu.Insert(
            "wait",
            [&session](std::ostream& out, std::vector<std::string> args) {
                auto parsed = parse_args(
                    args,
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
                    fail(out)
                        << "Without --expect-state the wait asserts that every step completed. "
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

        workflow_menu.Insert("start",
                             [&session](std::ostream& out, std::vector<std::string> args) {
                                 process_start(std::ref(out), std::ref(session), args);
                             },
                             "Start a workflow and print the instance id to follow",
                             {"<type> <request_json> [--instance-id <uuid>]"});
    });
}

bool workflow_run_commands::wait_for_instance(std::ostream& out,
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
            if (report_poll_failure(consecutive_failures, result.has_value())) {
                out << "⚠ Poll failed (" << consecutive_failures << "/"
                    << max_consecutive_poll_failures << "): " << reason << std::endl;
                BOOST_LOG_SEV(lg(), warn) << "Poll " << consecutive_failures << " failed for "
                                          << instance_id << ": " << reason;
            } else {
                BOOST_LOG_SEV(lg(), info) << "First poll refused instance " << instance_id
                                          << "; the engine creates the row after it answers.";
            }
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

            // Any failed step is a terminal failure; every planned step
            // completed, with or without warnings, is terminal success. Neither
            // holds when the caller named the state it expects: a run on its way
            // to compensated has a failed step by definition, and treating that
            // as the verdict would abort before the state it is headed for.
            if (expected_state.empty()) {
                const auto total = result->steps.size();
                for (const auto& step : result->steps) {
                    if (step.status != "failed")
                        continue;
                    fail(out) << "Workflow failed at step " << (step.step_index + 1) << " of "
                              << total << ": " << step.error << std::endl;
                    BOOST_LOG_SEV(lg(), error)
                        << "Workflow instance " << instance_id << " failed at step "
                        << step.step_index << ": " << step.error;
                    return false;
                }
                if (run_completed(*result, expected_steps)) {
                    out << "✓ All " << total << " step(s) completed." << std::endl;
                    BOOST_LOG_SEV(lg(), info)
                        << "Workflow instance " << instance_id << " completed.";
                    return true;
                }
                // Every step this run showed is done, but the run owes more:
                // the engine has not created them yet, or nothing is driving
                // it. Worth a line, because the wait now outlives the visible
                // list instead of calling the run finished.
                const auto owed = steps_expected(*result, expected_steps);
                if (total < owed &&
                    std::ranges::all_of(result->steps, &workflow_run_commands::is_step_complete))
                    BOOST_LOG_SEV(lg(), debug)
                        << "Every visible step is complete but the run owes more: " << total << "/"
                        << owed;
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

bool workflow_run_commands::is_step_complete(
    const workflow::messaging::workflow_step_summary& step) {
    return step.status == "completed" || step.status == "completed_with_warnings";
}

const workflow::messaging::workflow_step_summary* workflow_run_commands::last_completed_step(
    const std::vector<workflow::messaging::workflow_step_summary>& steps) {
    const workflow::messaging::workflow_step_summary* last = nullptr;
    for (const auto& step : steps) {
        if (!is_step_complete(step))
            continue;
        if (!last || step.step_index > last->step_index)
            last = &step;
    }
    return last;
}

std::size_t workflow_run_commands::steps_expected(
    const workflow::messaging::get_workflow_steps_response& reply,
    std::size_t expected_steps) {
    const auto planned = reply.step_count > 0 ? static_cast<std::size_t>(reply.step_count) : 0;
    return std::max(planned, expected_steps);
}

bool workflow_run_commands::run_completed(
    const workflow::messaging::get_workflow_steps_response& reply,
    std::size_t expected_steps) {
    return !reply.steps.empty() && reply.steps.size() >= steps_expected(reply, expected_steps) &&
           std::ranges::all_of(reply.steps, &workflow_run_commands::is_step_complete);
}

std::expected<workflow_run_commands::handoff, std::string>
workflow_run_commands::handoff_of_result(
    const workflow::messaging::get_step_result_response& reply) {
    if (!reply.success || !reply.found)
        return std::unexpected("the server holds no result for the step");
    if (reply.result_json.empty())
        return handoff{};

    // A step's result is the step's own JSON, so a result that is not an object
    // naming a child names no handoff. Only a result the read did not produce
    // leaves the question open, and that was answered above.
    const auto decoded = rfl::json::read<step_result_handoff>(reply.result_json);
    if (!decoded)
        return handoff{};
    return handoff{.instance_id = decoded->child_instance_id.value_or(""),
                   .tenant_id = decoded->tenant_id.value_or("")};
}

bool workflow_run_commands::can_read_run_of(std::string_view caller_tenant,
                                            std::string_view run_tenant) {
    return run_tenant.empty() || run_tenant == caller_tenant ||
           caller_tenant == ores::utility::uuid::max_uuid_str;
}

bool workflow_run_commands::report_poll_failure(int consecutive_failures, bool replied) {
    return !(consecutive_failures == 1 && replied);
}

workflow_run_commands::follow_result workflow_run_commands::follow_child_instance(
    std::ostream& out,
    nats_client& session,
    const std::string& instance_id,
    std::chrono::seconds timeout) {
    const auto steps = fetch_steps(session, instance_id);
    if (!steps || !steps->success) {
        const auto reason = !steps ? steps.error() : steps->message;
        fail(out) << "Could not read the steps of workflow instance " << instance_id
                  << " to see whether it handed work on: " << reason << std::endl;
        return follow_result::failed;
    }

    const auto* step = last_completed_step(steps->steps);
    if (!step)
        return follow_result::complete;

    // Only the last completed step is read. A handoff is the last thing a run
    // does, so a child an earlier step named was already followed by the work
    // that step reported, and reading it here would follow the wrong run.
    const auto tenant_id = session.auth().tenant_id;
    const auto reply = fetch_step_result(session, step->id, tenant_id);
    if (!reply) {
        fail(out) << "Could not read the result of step '" << step->name
                  << "' of workflow instance " << instance_id << ": " << reply.error()
                  << std::endl;
        return follow_result::failed;
    }
    const auto handoff = handoff_of_result(*reply);
    if (!handoff) {
        fail(out) << "Could not read the result of step '" << step->name
                  << "' of workflow instance " << instance_id
                  << ", so whether it handed work on is unknown: " << handoff.error() << std::endl;
        return follow_result::failed;
    }
    if (handoff->instance_id.empty())
        return follow_result::complete;

    if (!can_read_run_of(tenant_id, handoff->tenant_id)) {
        out << "  The run handed its work to instance " << handoff->instance_id
            << ", which belongs to tenant " << handoff->tenant_id << "." << std::endl;
        out << "  That run continues the work in that tenant, whose steps this session cannot "
               "read."
            << std::endl;
        return follow_result::continues_elsewhere;
    }

    out << "  The run handed its work to instance " << handoff->instance_id << "; following it."
        << std::endl;
    return wait_for_instance(out, session, handoff->instance_id, timeout) ? follow_result::complete
                                                                         : follow_result::failed;
}

void workflow_run_commands::process_start(std::ostream& out,
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
            ores::nats::headers::with_publish_identity(
                ores::nats::service::forwarded_caller_headers(session.auth().jwt),
                msg.instance_id));
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

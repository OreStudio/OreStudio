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
#include "ores.shell/app/commands/scheduler_commands.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.scheduler.api/messaging/job_definition_protocol.hpp"
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <cli/cli.h>
#include <cstdint>
#include <optional>
#include <ostream>
#include <string>
#include <thread>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
using namespace ores::scheduler::messaging;

namespace {

/**
 * @brief The subject the scheduler loop publishes executions on.
 *
 * The loop owns this subject; it is not part of the generated entity
 * protocol, because the entity it reports on has no model of its own yet.
 */
constexpr std::string_view job_instance_events_subject = "scheduler.v1.job-instance-events";

constexpr std::string_view default_reason_code =
    dq::domain::change_reason_constants::codes::new_record;

constexpr unsigned default_watch_seconds = 60;
constexpr unsigned max_watch_seconds = 3600;
constexpr std::size_t watch_buffer_capacity = 4096;
constexpr auto watch_poll_interval = std::chrono::milliseconds(200);
constexpr std::uint32_t default_page_limit = 100;

auto& lg() {
    static auto instance = make_logger("ores.shell.commands.scheduler");
    return instance;
}

std::string display_time(std::chrono::system_clock::time_point tp) {
    return ores::platform::time::datetime::to_local_display_string(tp);
}

/**
 * @brief Render a server timestamp the same way the events are rendered.
 *
 * The operation reads answer in ISO-8601 UTC while an event carries a time
 * point, so a reader comparing the two would see two clocks unless both are
 * shown where the reader is.
 */
std::string display_iso(std::string_view iso) {
    if (iso.empty())
        return std::string(iso);
    try {
        // Either form: an operation read states its designator and a stored
        // timestamp does not. from_db_string takes both, so a stored value
        // renders where it used to fall through to the raw string.
        return display_time(ores::platform::time::datetime::from_db_string(std::string(iso)));
    } catch (const std::exception&) {
        return std::string(iso);
    }
}

void print_definition(std::ostream& out, const ores::scheduler::domain::job_definition& d) {
    out << "  " << d.job_name << (d.is_active ? " [active]" : " [paused]") << std::endl
        << "    id:      " << boost::uuids::to_string(d.id) << std::endl
        << "    cron:    " << d.schedule_expression.to_string() << std::endl
        << "    action:  " << d.action_type << std::endl;
    if (!d.description.empty())
        out << "    about:   " << d.description << std::endl;
    if (!d.command.empty())
        out << "    command: " << d.command << std::endl;
}

bool report_result(std::ostream& out,
                   const ores::utility::domain::result& r,
                   const std::string& what) {
    if (r.outcome == ores::utility::domain::outcome::ok)
        return true;
    fail(out) << what << " was refused: " << r.code << ": " << r.message << std::endl;
    for (const auto& f : r.fields)
        fail(out) << "  " << f.field << ": " << f.message << std::endl;
    return false;
}

/**
 * @brief Find a definition by its natural key.
 *
 * The protocol addresses a removal by identifier, while a person types the
 * name, so the name is resolved through the same list the jobs command
 * renders before anything is deleted.
 */
std::optional<ores::scheduler::domain::job_definition>
find_by_name(std::ostream& out, nats_client& session, const std::string& job_name) {
    list_job_definitions_request req;
    req.limit = default_page_limit;

    auto response = do_auth_request<list_job_definitions_response>(
        out, session, std::string(req.nats_subject), req);
    if (!response)
        return std::nullopt;
    if (!report_result(out, response->result, "Listing the job definitions"))
        return std::nullopt;

    for (const auto& d : response->definitions) {
        if (d.job_name == job_name)
            return d;
    }
    fail(out) << "No job named '" << job_name << "' is visible to this session." << std::endl;
    return std::nullopt;
}

} // anonymous namespace

void scheduler_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto scheduler_menu = std::make_unique<cli::Menu>("scheduler");

    scheduler_menu->Insert(
        "jobs",
        [&session](std::ostream& out) { process_jobs(std::ref(out), std::ref(session)); },
        "List the job definitions and their schedules");

    scheduler_menu->Insert(
        "schedule",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_schedule(std::ref(out), std::ref(session), std::move(args));
        },
        "Create a job definition: schedule <job_name> <cron> [--action <type>] "
        "[--payload <json>] [--description <text>] [--inactive] [--reason <code>]");

    scheduler_menu->Insert("remove",
                           [&session](std::ostream& out, std::string job_name) {
                               process_remove(std::ref(out), std::ref(session), job_name);
                           },
                           "Delete a job definition by name",
                           {"job_name"});

    scheduler_menu->Insert(
        "instances",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_instances(std::ref(out), std::ref(session), std::move(args));
        },
        "List executions: instances [--job <name>] [--limit <n>]");

    scheduler_menu->Insert(
        "status",
        [&session](std::ostream& out) { process_status(std::ref(out), std::ref(session)); },
        "Show each job's schedule, last run and next firing");

    scheduler_menu->Insert(
        "watch",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_watch(std::ref(out), std::ref(session), std::move(args));
        },
        "Print execution events as they arrive: watch [--seconds <n>]");

    ores::shell::app::insert_menu(root_menu, std::move(scheduler_menu));
}

void scheduler_commands::process_jobs(std::ostream& out, nats_client& session) {
    BOOST_LOG_SEV(lg(), debug) << "Listing job definitions.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to list jobs." << std::endl;
        return;
    }

    list_job_definitions_request req;
    req.limit = default_page_limit;

    auto response = do_auth_request<list_job_definitions_response>(
        out, session, std::string(req.nats_subject), req);
    if (!response)
        return;
    if (!report_result(out, response->result, "Listing the job definitions"))
        return;

    if (response->definitions.empty()) {
        out << "No job definitions are visible to this session." << std::endl;
        return;
    }

    out << response->total << " job definition(s), showing " << response->definitions.size() << ":"
        << std::endl;
    for (const auto& d : response->definitions)
        print_definition(out, d);
}

void scheduler_commands::process_schedule(std::ostream& out,
                                          nats_client& session,
                                          const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Scheduling a job.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to schedule a job." << std::endl;
        return;
    }

    const std::vector<flag_spec> specs{
        {.name = "action", .requires_value = true, .default_value = "execute_sql"},
        {.name = "payload", .requires_value = true, .default_value = "{}"},
        {.name = "description", .requires_value = true, .default_value = ""},
        {.name = "command", .requires_value = true, .default_value = ""},
        {.name = "inactive", .requires_value = false, .default_value = "false"},
        {.name = "reason",
         .requires_value = true,
         .default_value = std::string(default_reason_code)}};

    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 2) {
        fail(out) << "Usage: scheduler schedule <job_name> <cron> [--action <type>] "
                     "[--payload <json>] [--description <text>] [--inactive] [--reason <code>]"
                  << std::endl;
        return;
    }

    const auto& job_name = parsed->positionals[0];
    const auto& cron = parsed->positionals[1];

    // The expression is validated here rather than on the server, because a
    // cron the evaluator cannot parse is a typo the person can fix in place.
    auto expression = ores::scheduler::domain::cron_expression::from_string(cron);
    if (!expression) {
        fail(out) << "'" << cron << "' is not a cron expression: " << expression.error()
                  << std::endl;
        return;
    }

    put_job_definition_request req;
    req.change.write.id = boost::uuids::random_generator()();
    req.change.write.job_name = job_name;
    req.change.write.description = parsed->flag("description");
    req.change.write.command = parsed->flag("command");
    req.change.write.schedule_expression = *expression;
    req.change.write.action_type = parsed->flag("action");
    req.change.write.action_payload = parsed->flag("payload");
    req.change.write.is_active = !parsed->flag_set("inactive");
    req.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
    req.intent.reason_code = parsed->flag("reason");
    req.intent.commentary = "Scheduled from the shell";

    auto response = do_auth_request<put_job_definition_response>(
        out, session, std::string(req.nats_subject), req);
    if (!response)
        return;
    if (!report_result(out, response->result, "Scheduling the job"))
        return;

    const auto& created = response->job_definition;
    out << "Scheduled '" << created.job_name << "' as " << boost::uuids::to_string(created.id)
        << " on '" << created.schedule_expression.to_string() << "'"
        << (created.is_active ? "." : ", paused.") << std::endl;
    out << "Run 'scheduler watch' to see it fire." << std::endl;
}

void scheduler_commands::process_remove(std::ostream& out,
                                        nats_client& session,
                                        const std::string& job_name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing job " << job_name;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to remove a job." << std::endl;
        return;
    }

    const auto existing = find_by_name(out, session, job_name);
    if (!existing)
        return;

    delete_job_definition_request req;
    req.removal.key.id = existing->id;
    req.intent.reason_code = std::string(default_reason_code);
    req.intent.commentary = "Removed from the shell";

    auto response = do_auth_request<delete_job_definition_response>(
        out, session, std::string(req.nats_subject), req);
    if (!response)
        return;
    if (!report_result(out, response->result, "Removing the job"))
        return;

    out << "Removed '" << job_name << "'." << std::endl;
}

void scheduler_commands::process_instances(std::ostream& out,
                                           nats_client& session,
                                           const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Listing job instances.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to list executions." << std::endl;
        return;
    }

    const std::vector<flag_spec> specs{{.name = "job", .requires_value = true, .default_value = ""},
                                       {.name = "limit",
                                        .requires_value = true,
                                        .default_value = std::to_string(default_page_limit)}};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "Usage: scheduler instances [--job <name>] [--limit <n>]" << std::endl;
        return;
    }

    const auto limit = parse_uint32(parsed->flag("limit"));
    if (!limit || *limit == 0) {
        fail(out) << "Flag --limit must be a positive integer: " << parsed->flag("limit")
                  << std::endl;
        return;
    }

    get_job_instances_request req;
    req.limit = *limit;

    auto response = do_auth_request<get_job_instances_response>(
        out, session, std::string(req.nats_subject), req);
    if (!response)
        return;
    if (!response->success) {
        fail(out) << "Listing the executions failed: " << response->message << std::endl;
        return;
    }

    const auto& job_filter = parsed->flag("job");
    std::size_t shown = 0;
    for (const auto& i : response->instances) {
        if (!job_filter.empty() && i.job_name != job_filter)
            continue;
        ++shown;
        out << "  #" << i.id << " " << i.job_name << " " << i.status << " "
            << display_iso(i.triggered_at);
        if (i.completed_at)
            out << " (" << *i.duration_ms << " ms)";
        out << std::endl;
        if (!i.error_message.empty())
            out << "    error: " << i.error_message << std::endl;
    }

    if (shown == 0) {
        out << (job_filter.empty() ? "No executions have been recorded." :
                                     "No executions recorded for '" + job_filter + "'.")
            << std::endl;
        return;
    }
    out << shown << " execution(s) of " << response->total_available_count << " available."
        << std::endl;
}

void scheduler_commands::process_status(std::ostream& out, nats_client& session) {
    BOOST_LOG_SEV(lg(), debug) << "Reading the scheduler status.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to read the scheduler status." << std::endl;
        return;
    }

    get_scheduler_status_request req;
    auto response = do_auth_request<get_scheduler_status_response>(
        out, session, std::string(req.nats_subject), req);
    if (!response)
        return;
    if (!response->success) {
        fail(out) << "Reading the scheduler status failed: " << response->message << std::endl;
        return;
    }

    if (response->jobs.empty()) {
        out << "No jobs are visible to this session." << std::endl;
        return;
    }

    out << response->total_active << " active, " << response->total_running
        << " running:" << std::endl;
    for (const auto& j : response->jobs) {
        out << "  " << j.job_name << (j.is_active ? " [active]" : " [paused]") << " on '"
            << j.schedule_expression << "'" << std::endl;
        out << "    next:   "
            << (j.next_fire_at ? display_iso(*j.next_fire_at) : std::string("no next firing"))
            << std::endl;
        out << "    last:   "
            << (j.last_run_at ? display_iso(*j.last_run_at) : std::string("never run"));
        if (j.last_run_status)
            out << " (" << *j.last_run_status << ")";
        out << std::endl;
        if (j.running_count > 0)
            out << "    running now: " << j.running_count << std::endl;
    }
}

void scheduler_commands::process_watch(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Watching scheduler execution events.";

    const std::vector<flag_spec> specs{{.name = "seconds",
                                        .requires_value = true,
                                        .default_value = std::to_string(default_watch_seconds)}};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "Usage: scheduler watch [--seconds <n>]" << std::endl;
        return;
    }

    const auto seconds = parse_uint32(parsed->flag("seconds"));
    if (!seconds || *seconds == 0 || *seconds > max_watch_seconds) {
        fail(out) << "Flag --seconds must be between 1 and " << max_watch_seconds << ": "
                  << parsed->flag("seconds") << std::endl;
        return;
    }

    // A subscription rather than a request: the scheduler publishes when a
    // job fires, and nothing answers a question about that.
    auto subscription = session.transport().subscribe_buffered(
        std::string(job_instance_events_subject), watch_buffer_capacity);

    out << "Listening on '" << job_instance_events_subject << "' for " << *seconds << " second(s)."
        << std::endl;

    std::size_t printed = 0;
    const auto deadline = std::chrono::system_clock::now() + std::chrono::seconds(*seconds);
    while (std::chrono::system_clock::now() < deadline) {
        std::this_thread::sleep_for(watch_poll_interval);

        const auto snapshot = subscription.snapshot();
        for (; printed < snapshot.size(); ++printed) {
            const auto& msg = snapshot[printed];
            const auto event = ores::nats::default_wire_codec()
                                   .decode<ores::eventing::domain::entity_change_event>(msg.data);
            if (!event) {
                out << "  " << msg.subject << ": undecodable message, " << msg.data.size()
                    << " bytes" << std::endl;
                continue;
            }
            out << "  " << display_time(event->timestamp) << " " << event->entity;
            for (const auto& id : event->entity_ids)
                out << " " << id;
            if (!event->tenant_id.empty())
                out << " for tenant " << event->tenant_id;
            out << std::endl;
        }
    }

    out << printed << " event(s) in " << *seconds << " second(s)." << std::endl;
}

}

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
 * Template: cpp_shell_operation_implementation.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.shell/app/commands/reporting/report_operations_operations_commands.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <cstddef>
#include <functional>
#include <optional>
#include <ostream>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

/**
 * @brief Split a comma-separated token into the elements of a list field.
 */
std::vector<std::string> split_list_token(const std::string& value) {
    std::vector<std::string> parts;
    std::string current;
    for (const char c : value) {
        if (c == ',') {
            parts.push_back(current);
            current.clear();
        } else {
            current.push_back(c);
        }
    }
    parts.push_back(current);
    return parts;
}

} // namespace

void report_operations_operations_commands::register_commands(cli::Menu& root_menu,
                                                              nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("report_operations");

    menu->Insert(
        "trigger-report-instance",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_trigger_report_instance(std::ref(out), std::ref(session), std::move(args));
        },
        "trigger-report-instance <report_definition_id> <tenant_id> [--job_instance_id <v>]");

    menu->Insert(
        "schedule-report-definitions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_schedule_report_definitions(std::ref(out), std::ref(session), std::move(args));
        },
        "schedule-report-definitions <ids>");

    menu->Insert(
        "unschedule-report-definitions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_unschedule_report_definitions(
                std::ref(out), std::ref(session), std::move(args));
        },
        "unschedule-report-definitions <ids>");

    root_menu.Insert(std::move(menu));
}

void report_operations_operations_commands::process_trigger_report_instance(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating trigger-report-instance request.";

    using request_type = ores::reporting::messaging::trigger_report_instance_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run trigger-report-instance." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "job_instance_id", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 2;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_definition_id = ores::shell::app::from_token<boost::uuids::uuid>(
            parsed->positionals[next++], "report_definition_id");
        req.tenant_id = ores::shell::app::from_token<boost::uuids::uuid>(
            parsed->positionals[next++], "tenant_id");
        if (const auto& raw_job_instance_id = parsed->flag("job_instance_id");
            !raw_job_instance_id.empty()) {
            req.job_instance_id =
                ores::shell::app::from_token<std::int64_t>(raw_job_instance_id, "job_instance_id");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::trigger_report_instance_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::trigger_report_instance_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::trigger_report_instance_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_schedule_report_definitions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating schedule-report-definitions request.";

    using request_type = ores::reporting::messaging::schedule_report_definitions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run schedule-report-definitions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 1;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.ids = split_list_token(parsed->positionals[next++]);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::schedule_report_definitions_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::schedule_report_definitions_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::schedule_report_definitions_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_unschedule_report_definitions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating unschedule-report-definitions request.";

    using request_type = ores::reporting::messaging::unschedule_report_definitions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run unschedule-report-definitions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 1;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.ids = split_list_token(parsed->positionals[next++]);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::unschedule_report_definitions_response> result;
    if constexpr (request_type::requires_session) {
        result =
            do_auth_request<ores::reporting::messaging::unschedule_report_definitions_response>(
                out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::unschedule_report_definitions_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}

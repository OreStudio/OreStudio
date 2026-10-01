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
#include "ores.shell/app/commands/workflow/workflow_operations_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
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

void workflow_operations_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("workflow");

    menu->Insert(
        "step-result",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_step_result(std::ref(out), std::ref(session), std::move(args));
        },
        "step-result <step_id> <tenant_id>");

    menu->Insert(
        "instances",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_instances(std::ref(out), std::ref(session), std::move(args));
        },
        "instances [--limit <v>] [--status_filter <v>] [--type_filter <v>] [--target_kind_filter "
        "<v>] [--target_id_filter <v>]");

    menu->Insert(
        "steps",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_steps(std::ref(out), std::ref(session), std::move(args));
        },
        "steps <workflow_instance_id>");

    menu->Insert(
        "definitions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_definitions(std::ref(out), std::ref(session), std::move(args));
        },
        "definitions");

    menu->Insert(
        "retry",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_retry(std::ref(out), std::ref(session), std::move(args));
        },
        "retry <workflow_instance_id> [--step_name <v>]");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

void workflow_operations_commands::process_step_result(std::ostream& out,
                                                       nats_client& session,
                                                       const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating step-result request.";

    using request_type = ores::workflow::messaging::get_step_result_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run step-result." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
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
        req.step_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::workflow::messaging::get_step_result_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::workflow::messaging::get_step_result_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::workflow::messaging::get_step_result_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void workflow_operations_commands::process_instances(std::ostream& out,
                                                     nats_client& session,
                                                     const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating instances request.";

    using request_type = ores::workflow::messaging::list_workflow_instance_summaries_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run instances." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "limit", .requires_value = true, .default_value = ""},
        {.name = "status_filter", .requires_value = true, .default_value = ""},
        {.name = "type_filter", .requires_value = true, .default_value = ""},
        {.name = "target_kind_filter", .requires_value = true, .default_value = ""},
        {.name = "target_id_filter", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 0;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    try {
        if (const auto& raw_limit = parsed->flag("limit"); !raw_limit.empty()) {
            req.limit = ores::shell::app::from_token<int>(raw_limit, "limit");
        }
        if (const auto& raw_status_filter = parsed->flag("status_filter");
            !raw_status_filter.empty()) {
            req.status_filter = raw_status_filter;
        }
        if (const auto& raw_type_filter = parsed->flag("type_filter"); !raw_type_filter.empty()) {
            req.type_filter = raw_type_filter;
        }
        if (const auto& raw_target_kind_filter = parsed->flag("target_kind_filter");
            !raw_target_kind_filter.empty()) {
            req.target_kind_filter = raw_target_kind_filter;
        }
        if (const auto& raw_target_id_filter = parsed->flag("target_id_filter");
            !raw_target_id_filter.empty()) {
            req.target_id_filter = raw_target_id_filter;
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::workflow::messaging::list_workflow_instance_summaries_response> result;
    if constexpr (request_type::requires_session) {
        result =
            do_auth_request<ores::workflow::messaging::list_workflow_instance_summaries_response>(
                out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::workflow::messaging::list_workflow_instance_summaries_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void workflow_operations_commands::process_steps(std::ostream& out,
                                                 nats_client& session,
                                                 const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating steps request.";

    using request_type = ores::workflow::messaging::get_workflow_steps_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run steps." << std::endl;
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
        req.workflow_instance_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::workflow::messaging::get_workflow_steps_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::workflow::messaging::get_workflow_steps_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::workflow::messaging::get_workflow_steps_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void workflow_operations_commands::process_definitions(std::ostream& out,
                                                       nats_client& session,
                                                       const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating definitions request.";

    using request_type = ores::workflow::messaging::list_workflow_definitions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run definitions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 0;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    try {
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::workflow::messaging::list_workflow_definitions_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::workflow::messaging::list_workflow_definitions_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::workflow::messaging::list_workflow_definitions_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void workflow_operations_commands::process_retry(std::ostream& out,
                                                 nats_client& session,
                                                 const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating retry request.";

    using request_type = ores::workflow::messaging::retry_workflow_instance_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run retry." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "step_name", .requires_value = true, .default_value = ""},
    };
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
        req.workflow_instance_id = parsed->positionals[next++];
        if (const auto& raw_step_name = parsed->flag("step_name"); !raw_step_name.empty()) {
            req.step_name = raw_step_name;
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::workflow::messaging::retry_workflow_instance_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::workflow::messaging::retry_workflow_instance_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::workflow::messaging::retry_workflow_instance_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
